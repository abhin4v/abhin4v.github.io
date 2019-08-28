{-# LANGUAGE DeriveGeneric, LambdaCase, ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Site.Webmentions where

import Control.Exception (try, SomeException)
import Data.Char (toLower)
import Data.Aeson (FromJSON(..), constructorTagModifier, defaultOptions, genericParseJSON, eitherDecode')
import Data.Aeson.Casing (aesonPrefix, aesonDrop, snakeCase)
import Data.Function (on)
import Data.List (sortBy, nubBy)
import Data.Maybe (fromMaybe, fromJust)
import Data.Ord (comparing)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import GHC.Generics
import Hakyll
import Network.HTTP.Simple (httpLBS, parseRequest, getResponseBody)
import Debug.Trace (trace)

data ActivityType = Repost | Like | Link deriving (Show, Generic, Eq)

data Activity = Activity { activityType :: ActivityType } deriving (Show, Generic)

data Author = Author { authorName :: T.Text
                     , authorUrl :: Maybe T.Text
                     } deriving (Show, Generic)

data Data = Data { dataAuthor :: Maybe Author
                 , dataUrl :: Maybe T.Text
                 } deriving (Show, Generic)

data Mention = Mention { mentionId           :: Integer
                       , mentionSource       :: T.Text
                       , mentionTarget       :: T.Text
                       , mentionVerifiedDate :: UTCTime
                       , mentionData         :: Data
                       , mentionActivity     :: Activity
                       } deriving (Show, Generic)

newtype Webmentions = Webmentions {wmLinks :: [Mention]}
                      deriving (Show, Generic, Semigroup)

instance FromJSON ActivityType where
  parseJSON = genericParseJSON $ defaultOptions { constructorTagModifier = map toLower }

instance FromJSON Activity where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance FromJSON Author where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance FromJSON Data where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance FromJSON Mention where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance FromJSON Webmentions where
  parseJSON = genericParseJSON $ aesonDrop 2 snakeCase

getWebmentions :: String -> IO (Maybe Webmentions)
getWebmentions postSlug =
  fmap (dedupMentions . transform)
    <$> mconcat [go scheme prefix suffix | scheme <- ["https", "http"]
                                         , prefix <- ["posts", "drafts"]
                                         , suffix <- ["", "/"]]
  where
    go scheme prefix suffix =
      parseRequest (mentionsURL scheme prefix suffix) >>= try . httpLBS >>= \case
        Left (e :: SomeException) ->
          trace ("Unable to fetch mentions: " ++ show e) $ return Nothing
        Right resp -> return $ case eitherDecode' . getResponseBody $ resp of
          Left err -> trace ("Unable to decode: " ++ err) Nothing
          Right wm -> Just wm

    mentionsURL scheme prefix suffix =
      "https://webmention.io/api/mentions?perPage=100&target=" <>
      scheme <> "://abhinavsarkar.net" <> "/" <> prefix <> "/" <> postSlug <> suffix

    transform Webmentions{..} =
      Webmentions
      . sortBy (comparing mentionVerifiedDate <> comparing mentionSource)
      . map cleanupMention
      $ wmLinks

    cleanupMention mention@Mention{..} =
      mention { mentionTarget = T.replace "/drafts/" "/posts/" mentionTarget }

    dedupMentions wm@Webmentions{..} = wm {wmLinks = nubBy ((==) `on` mentionSource) wmLinks}

sourceName :: T.Text -> T.Text
sourceName source
  | "reddit.com" `T.isInfixOf` source = "reddit/r/" <> (parts !! 4)
  | "mailchi.mp" `T.isInfixOf` source = parts !! 3
  | "github.com" `T.isInfixOf` source = "github/" <> (parts !! 3) <> "/" <> (parts !! 4)
  | otherwise                         = fromMaybe (parts !! 2) $ T.stripPrefix "www." (parts !! 2)
    where
      parts = T.splitOn "/" source

webmentionsCtx :: Webmentions -> Context String
webmentionsCtx Webmentions{..} =
  let likes = filterLinks Like wmLinks
      reposts = filterLinks Repost wmLinks
  in boolField "mentions_enabled" (const . not . null $ wmLinks) <>
     constField "mention_count" (show $ length wmLinks) <>
     listField "mentions" mentionCtx (mapM makeItem (filterLinks Link wmLinks)) <>
     boolField "likes_enabled" (const . not . null $ likes) <>
     constField "likes_count" (show $ length likes) <>
     constField "likes_label" (inflect "Like" likes) <>
     listField "likes" likeCtx (mapM makeItem likes) <>
     boolField "reposts_enabled" (const . not . null $ reposts) <>
     constField "reposts_count" (show $ length reposts) <>
     constField "reposts_label" (inflect "Repost" reposts) <>
     listField "reposts" repostCtx (mapM makeItem reposts)
  where
    filterLinks typ = filter (\Mention{..} -> activityType mentionActivity == typ)
    inflect label col = label ++ if length col == 1 then "" else "s"

fnField :: String -> (a -> String) -> Context a
fnField name f = field name (return . f . itemBody)

mentionCtx :: Context Mention
mentionCtx =
  mconcat [ fnField "mention_id" $ show . mentionId
          , fnField "mention_source" $ T.unpack . mentionSource
          , fnField "mention_source_name" $ T.unpack . sourceName . mentionSource
          , fnField "mention_date" (formatTime defaultTimeLocale "%b %e, %Y" . mentionVerifiedDate)]

likeCtx :: Context Mention
likeCtx = fnField "name" (T.unpack . authorName . fromJust . dataAuthor . mentionData) <>
          fnField "url" (T.unpack . fromJust . authorUrl . fromJust . dataAuthor . mentionData)

repostCtx :: Context Mention
repostCtx = fnField "name" (T.unpack . authorName . fromJust . dataAuthor . mentionData) <>
            fnField "url" (T.unpack . getUrl)
  where
    getUrl mention =
      let dUrl = fromJust . dataUrl . mentionData $ mention
      in if "twitter" `T.isInfixOf` dUrl
         then fromJust . authorUrl . fromJust . dataAuthor . mentionData $ mention
         else dUrl
