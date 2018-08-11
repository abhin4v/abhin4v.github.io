{-# LANGUAGE DeriveGeneric, LambdaCase, ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Site.Webmentions where

import Control.Arrow ((&&&))
import Control.Exception (try, SomeException)
import Data.Aeson (FromJSON(..), genericParseJSON, decode')
import Data.Aeson.Casing (aesonPrefix, aesonDrop, snakeCase)
import Data.Function (on)
import Data.List (sortBy, nubBy)
import Data.Ord (comparing)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import GHC.Generics
import Hakyll
import Network.HTTP.Simple (httpLBS, parseRequest, getResponseBody)

newtype Webmentions = Webmentions {wmLinks :: [Link]}
                      deriving (Show, Generic, Semigroup)

data Link = Link { linkId           :: Integer
                 , linkSource       :: T.Text
                 , linkTarget       :: T.Text
                 , linkVerifiedDate :: UTCTime
                 } deriving (Show, Generic)

instance FromJSON Link where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance FromJSON Webmentions where
  parseJSON = genericParseJSON $ aesonDrop 2 snakeCase

getWebmentions :: String -> IO (Maybe Webmentions)
getWebmentions postSlug =
  fmap dedupLinks <$> go "https" "posts" <> go "https" "drafts" <> go "http" "posts" <> go "http" "drafts"
  where
    go scheme prefix = parseRequest (mentionsURL scheme prefix) >>= try . httpLBS >>= \case
      Left (_ :: SomeException) -> return Nothing
      Right resp -> return . fmap transform . decode' . getResponseBody $ resp

    mentionsURL scheme prefix =
      "https://webmention.io/api/mentions?target=" <>
      scheme <> "://abhinavsarkar.net" <> "/" <> prefix <> "/" <> postSlug <> "/"

    transform Webmentions{..} =
      (\links -> Webmentions {wmLinks = links})
      . sortBy (comparing (linkVerifiedDate &&& linkSource))
      . map cleanupLink
      $ wmLinks

    cleanupLink :: Link -> Link
    cleanupLink link@Link{..} = link { linkTarget = T.replace "/drafts/" "/posts/" linkTarget }

    dedupLinks wm@Webmentions{..} = wm {wmLinks = nubBy ((==) `on` linkSource) wmLinks}

sourceName :: T.Text -> T.Text
sourceName source
  | "reddit.com" `T.isInfixOf` source = "reddit/r/" <> (parts !! 4)
  | "mailchi.mp" `T.isInfixOf` source = parts !! 3
  | "github.com" `T.isInfixOf` source = "github/" <> (parts !! 3) <> "/" <> (parts !! 4)
  | otherwise                         = parts !! 2
    where
      parts = T.splitOn "/" source

mentionLinkCtx :: Context Link
mentionLinkCtx =
  mconcat [ mlField "link_id" $ show . linkId
          , mlField "link_source" $ T.unpack . linkSource
          , mlField "link_source_name" $ T.unpack . sourceName . linkSource
          , mlField "link_date" (formatTime defaultTimeLocale "%b %e, %Y" . linkVerifiedDate)]
  where
    mlField name f = field name (return . f . itemBody)
