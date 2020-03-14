{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, DeriveGeneric, RecordWildCards #-}
module Site.Activities (Auth, newAuth, activities) where

import Data.Aeson (ToJSON(..), genericToEncoding, encode)
import Data.Aeson.Casing (aesonDrop, snakeCase)
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Char (toLower)
import qualified Data.List.NonEmpty as NEL
import Data.Maybe (isJust, fromMaybe)
import Data.Monoid ((<>))
import Data.Semigroup (Max(..), sconcat)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime, addUTCTime, nominalDay)
import GHC.Generics
import Hakyll hiding (relativizeUrls)
import Site.Util
import Site.Activities.Strava

data ShortActivity = ShortActivity { saT :: ActivityType, saD :: Double, saS :: UTCTime }
                     deriving (Generic)

instance ToJSON ShortActivity where
  toEncoding = genericToEncoding $ aesonDrop 2 snakeCase

stravaActivityPageUrlBase :: String
stravaActivityPageUrlBase = "https://www.strava.com/activities/"

recentActivityTypes, yearlyActivityTypes, speedActivityTypes, paceActivityTypes :: [ActivityType]
recentActivityTypes = [Ride, Run, Walk, Workout]
yearlyActivityTypes = [Ride, Run, Walk]
speedActivityTypes = [Ride]
paceActivityTypes = [Run, Walk]

activities :: Auth -> String -> Rules ()
activities auth env = do
  anyDependency <- makePatternDependency "**"
  rulesExtraDependencies [anyDependency] $
    create ["activities.html"] $ do
      route indexHTMLRoute
      compile $ do
        allActivities <- unsafeCompiler $ getActivities auth 200 1 <> getActivities auth 200 2
        cur           <- unsafeCompiler getCurrentTime
        let yearSince         = addUTCTime (-366 * nominalDay) cur
            yearActicities    = filterAllActivities yearSince allActivities
            activitiesCalJSON = mkActivitiesCalJSON yearActicities
            recentSince       = addUTCTime (-30 * nominalDay) cur
            recentActivities  = filterRecentActivities recentSince allActivities
            maxSufferScore    = calcMaxSufferScore recentActivities
            ctx =
              listField "activities" (activityCtx maxSufferScore) (mapM makeItem recentActivities) <>
              constField "title" "Activities" <>
              constField "page_type" "activities" <>
              constField "cal_json" activitiesCalJSON <>
              siteContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/activities.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls env
          >>= removeIndexHtml
  where
    mkActivitiesCalJSON =
      C8.unpack
      . encode
      . map (\Activity{..} -> ShortActivity
          (if activityType == Walk then Run else activityType) activityDistance activityStartDate)

    filterRecentActivities since =
      filter (not . isNaN . activitySufferScore)
      . filter ((`elem` recentActivityTypes) . activityType)
      . filter ((> since) . activityStartDate)

    filterAllActivities since =
      filter ((`elem` yearlyActivityTypes) . activityType)
      . filter ((> since) . activityStartDate)

    calcMaxSufferScore =
      getMax
      . sconcat
      . NEL.fromList
      . map Max
      . (0:)
      . map activitySufferScore

activityCtx :: Double -> Context Activity
activityCtx maxSufferScore = mconcat [
    activityField "name"          $ activityName
  , activityField "type"          $ map toLower . show . activityType
  , activityField "width"         $ show . (/ maxSufferScore) . activitySufferScore
  , activityField "url"           $ (stravaActivityPageUrlBase ++) . show . activityId
  , activityField "date"          $ formatTime defaultTimeLocale "%b %e" . activityStartDate
  , activityField "distance"      $ showDist . activityDistance
  , activityField "speed"         $ showSpeed . mpsToKmPerHr . activityAverageSpeed
  , activityField "pace"          $ showPace . round . mpsToSecPerKm . activityAverageSpeed
  , activityField "heart_rate"    $ show . round . fromMaybe 0 . activityAverageHeartrate
  , activityField "moving_time"   $ showSecs . activityMovingTime
  , activityField "elev_gain"     $ show . activityTotalElevationGain
  , activityField "suffer_score"  $ show . round . activitySufferScore
  , boolField     "show_distance" $ (`elem` yearlyActivityTypes) . activityType . itemBody
  , boolField     "show_speed"    $ (`elem` speedActivityTypes) . activityType . itemBody
  , boolField     "show_pace"     $ (`elem` paceActivityTypes) . activityType . itemBody
  , boolField     "show_hr"       $ isJust . activityAverageHeartrate . itemBody
  ]
  where
    activityField name f = field name (return . f . itemBody)

    mpsToKmPerHr speed = speed * 18 / 5
    mpsToSecPerKm speed = 60 / speed * 50 / 3
    showDist dist = let d = round (dist / 100) in show (d `div` 10) ++ "." ++ show (d `mod` 10)
    showSpeed speed = let s = round (speed * 10) in show (s `div` 10) ++ "." ++ show (s `mod` 10)
    showPace sec = show (sec `div` 60) ++ ":" ++ showPad 2 '0' (sec `mod` 60)

    showSecs sec = let
        hr = sec `div` 3600
        min = (sec - hr * 3600) `div` 60
      in show hr ++ ":" ++ showPad 2 '0' min ++ ":" ++ showPad 2 '0' (sec `mod` 60)

    showPad count char a = let s = show a in replicate (count - length s) char ++ s