{-# LANGUAGE LambdaCase, ScopedTypeVariables, RecordWildCards, OverloadedStrings, DeriveGeneric #-}
module Site.Activities (Auth, newAuth, activities) where

import Control.Exception (try, SomeException, displayException)
import Data.Aeson ( FromJSON(..), ToJSON(..)
                  , genericParseJSON, genericToEncoding, eitherDecode', withText)
import Data.Aeson.Casing (aesonPrefix, aesonDrop, snakeCase)
import qualified Data.ByteString.Char8 as C8
import Data.Char (toLower)
import qualified Data.List.NonEmpty as NEL
import Data.Maybe (isJust, fromMaybe)
import Data.Monoid ((<>))
import Data.Semigroup (Max(..), sconcat)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Debug.Trace (trace)
import GHC.Generics
import Hakyll hiding (relativizeUrls)
import Network.HTTP.Simple ( httpLBS, parseRequest
                           , addRequestHeader, setRequestQueryString
                           , setRequestBodyJSON, setRequestMethod
                           , getResponseBody, getResponseStatusCode, Request)
import Network.HTTP.Types.Header (hAuthorization)
import Site.Util
import Text.Read (readMaybe)

data ActivityType = Ride | Run | Walk | Swim | Unknown deriving (Show, Read, Generic, Eq)

instance FromJSON ActivityType where
  parseJSON = withText "ActivityType" $ pure . fromMaybe Unknown . readMaybe . T.unpack

allowedActivityTypes :: [ActivityType]
allowedActivityTypes = [Ride, Run, Walk]

data Activity = Activity { activityName               :: String
                         , activityType               :: ActivityType
                         , activitySufferScore        :: Double
                         , activityId                 :: Integer
                         , activityStartDate          :: UTCTime
                         , activityDistance           :: Double
                         , activityAverageSpeed       :: Double
                         , activityAverageHeartrate   :: Maybe Double
                         , activityMovingTime         :: Int
                         , activityTotalElevationGain :: Double
                         } deriving (Show, Generic)

instance FromJSON Activity where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Auth = Auth { authGrantType    :: String
                 , authClientId     :: String
                 , authClientSecret :: String
                 , authRefreshToken :: String
                 } deriving (Generic)

newAuth :: String -> String -> String -> Auth
newAuth = Auth "refresh_token"

instance ToJSON Auth where
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

data AccessToken = AccessToken { atAccessToken :: String
                               , atTokenType   :: String
                               } deriving (Show, Eq, Generic)

instance FromJSON AccessToken where
  parseJSON = genericParseJSON $ aesonDrop 2 snakeCase

stravaAuthUrlBase, stravaActivityUrlBase, stravaActivityPageUrlBase :: String
stravaAuthUrlBase         = "https://www.strava.com/api/v3/oauth/token"
stravaActivityUrlBase     = "https://www.strava.com/api/v3/athlete/activities"
stravaActivityPageUrlBase = "https://www.strava.com/activities/"

execJSONReq :: (Show a, FromJSON a) => Request -> IO (Either String a)
execJSONReq req =
  try (httpLBS req) >>= \case
    Left (e :: SomeException) -> return . Left . displayException $ e
    Right resp | getResponseStatusCode resp == 200 ->
      return $ case eitherDecode' (getResponseBody resp) of
        Left err -> Left $ "Unable to decode response: " ++ err
        Right a -> Right a
    Right resp ->
      return $ Left $ "Invalid response code: " ++ show (getResponseStatusCode resp)

getAccessToken :: Auth -> IO (Maybe AccessToken)
getAccessToken auth =
  createRequest >>= execJSONReq >>= return . \case
    Left err -> trace ("Failed to get access token: " ++ err) Nothing
    Right atr -> Just atr
  where
    createRequest = do
      req <- parseRequest stravaAuthUrlBase
      return
        . setRequestMethod "POST"
        . setRequestBodyJSON auth
        $ req

getActivities :: Auth -> Int -> IO [Activity]
getActivities auth page =
  getAccessToken auth >>= \case
    Nothing -> return []
    Just accessToken -> createRequest accessToken >>= execJSONReq >>= return . \case
      Left err -> trace ("Failed to get activities: " ++ err) []
      Right activities -> activities
  where
    createRequest AccessToken{..} = do
      req <- parseRequest stravaActivityUrlBase
      return
        . setRequestQueryString [ ("per_page", Just "100")
                                , ("page", Just . C8.pack . show $ page)]
        . addRequestHeader hAuthorization (C8.pack $ atTokenType ++ " " ++ atAccessToken)
        $ req

activities :: Auth -> String -> Rules ()
activities auth env = do
  anyDependency <- makePatternDependency "**"
  rulesExtraDependencies [anyDependency] $
    create ["activities.html"] $ do
      route indexHTMLRoute
      compile $ do
        activities' <- unsafeCompiler $ take 25 . filterActivities <$> getActivities auth 1
        let maxSufferScore = calcMaxSufferScore activities'

        let ctx =
              listField "activities" (activityCtx maxSufferScore) (mapM makeItem activities') <>
              constField "title" "Activities" <>
              constField "page_type" "activities" <>
              siteContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/activities.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls env
          >>= removeIndexHtml
  where
    filterActivities =
      filter (not . isNaN . activitySufferScore)
      . filter ((`elem` allowedActivityTypes) . activityType)

    calcMaxSufferScore =
      getMax
      . sconcat
      . NEL.fromList
      . map Max
      . (0:)
      . map activitySufferScore

    activityField name f = field name (return . f . itemBody)

    activityCtx maxSufferScore = mconcat [
        activityField "name"         $ activityName
      , activityField "type"         $ map toLower . show . activityType
      , activityField "width"        $ show . (* widthMult maxSufferScore) . activitySufferScore
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

    widthMult maxSufferScore = 38 / maxSufferScore

    mpsToKmph speed = speed * 18 / 5
    mpsToMinpKm speed = 1 / speed * 50 / 3
    showDist dist = let d = round (dist / 100) in show (d `div` 10) ++ "." ++ show (d `mod` 10)
    showSpeed speed = let s = round (speed * 10) in show (s `div` 10) ++ "." ++ show (s `mod` 10)
    showPace pace = let sec = round (pace * 60) in show (sec `div` 60) ++ ":" ++ show (sec `mod` 60)

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