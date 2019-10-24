{-# LANGUAGE LambdaCase, ScopedTypeVariables, RecordWildCards, OverloadedStrings, DeriveGeneric #-}
module Site.Activities.Strava ( ActivityType(..)
                              , Activity(..)
                              , Auth
                              , newAuth
                              , getActivities) where

import Control.Exception (try, SomeException, displayException)
import Data.Aeson ( FromJSON(..), ToJSON(..)
                  , genericParseJSON, genericToEncoding, eitherDecode', withText)
import Data.Aeson.Casing (aesonPrefix, aesonDrop, snakeCase)
import qualified Data.ByteString.Char8 as C8
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Debug.Trace (trace)
import GHC.Generics
import Network.HTTP.Simple ( httpLBS, parseRequest, Request
                           , addRequestHeader, setRequestQueryString
                           , setRequestBodyJSON, setRequestMethod
                           , getResponseBody, getResponseStatusCode)
import Network.HTTP.Types.Header (hAuthorization)
import Text.Read (readMaybe)

data ActivityType = Ride | Run | Walk | Swim | Unknown deriving (Show, Read, Generic, Eq)

instance FromJSON ActivityType where
  parseJSON = withText "ActivityType" $ pure . fromMaybe Unknown . readMaybe . T.unpack

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

stravaAuthUrlBase, stravaActivityUrlBase :: String
stravaAuthUrlBase     = "https://www.strava.com/api/v3/oauth/token"
stravaActivityUrlBase = "https://www.strava.com/api/v3/athlete/activities"

execJSONReq :: FromJSON a => Request -> IO (Either String a)
execJSONReq req = try (httpLBS req) >>= \case
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
    createRequest =
      setRequestMethod "POST" . setRequestBodyJSON auth <$> parseRequest stravaAuthUrlBase

getActivities :: Auth -> Int -> IO [Activity]
getActivities auth page =
  getAccessToken auth >>= \case
    Nothing -> return []
    Just accessToken -> createRequest accessToken >>= execJSONReq >>= return . \case
      Left err -> trace ("Failed to get activities: " ++ err) []
      Right activities -> activities
  where
    createRequest AccessToken{..} =
      setRequestQueryString [ ("per_page", Just "100")
                            , ("page", Just . C8.pack . show $ page)]
      . addRequestHeader hAuthorization (C8.pack $ atTokenType ++ " " ++ atAccessToken)
      <$> parseRequest stravaActivityUrlBase