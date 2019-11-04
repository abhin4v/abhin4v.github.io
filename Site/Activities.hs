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

allowedActivityTypes :: [ActivityType]
allowedActivityTypes = [Ride, Run, Walk]

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
      . filterAllActivities since

    filterAllActivities since =
      filter ((`elem` allowedActivityTypes) . activityType)
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
    activityField "name"         $ activityName
  , activityField "type"         $ map toLower . show . activityType
  , activityField "width"        $ show . (/ maxSufferScore) . activitySufferScore
  , activityField "url"          $ (stravaActivityPageUrlBase ++) . show . activityId
  , activityField "date"         $ formatTime defaultTimeLocale "%b %e" . activityStartDate
  , activityField "distance"     $ showDist . activityDistance
  , activityField "speed"        $ showSpeed . mpsToKmph . activityAverageSpeed
  , activityField "pace"         $ showPace . mpsToMinpKm . activityAverageSpeed
  , boolField     "show_speed"   $ (== Ride) . activityType . itemBody
  , boolField     "show_pace"    $ (/= Ride) . activityType . itemBody
  , activityField "heart_rate"   $ show . round . fromMaybe 0 . activityAverageHeartrate
  , boolField     "show_hr"      $ isJust . activityAverageHeartrate . itemBody
  , activityField "moving_time"  $ showSecs . activityMovingTime
  , activityField "elev_gain"    $ show . activityTotalElevationGain
  , activityField "suffer_score" $ show . round . activitySufferScore
  ]
  where
    activityField name f = field name (return . f . itemBody)

    mpsToKmph speed = speed * 18 / 5
    mpsToMinpKm speed = 1 / speed * 50 / 3
    showDist dist = let d = round (dist / 100) in show (d `div` 10) ++ "." ++ show (d `mod` 10)
    showSpeed speed = let s = round (speed * 10) in show (s `div` 10) ++ "." ++ show (s `mod` 10)
    showPace pace = let sec = round (pace * 60) in show (sec `div` 60) ++ ":" ++ showPad 2 '0' (sec `mod` 60)

    showSecs sec = let
        min = sec `div` 60
        hr = sec `div` 3600
      in case (hr, min, sec) of
        (0, 0, s) -> show s ++ " sec"
        (0, m, s) ->
          show m ++ " min" ++ recurse (s - min * 60)
        (h, _, s) ->
          show h ++ " hr" ++ recurse (s - h * 3600)
      where
        recurse rem = if rem == 0 then "" else " " ++ showSecs rem

    showPad count char a = let s = show a in replicate (count - length s) char ++ s