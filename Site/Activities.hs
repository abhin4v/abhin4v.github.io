{-# LANGUAGE LambdaCase, ScopedTypeVariables, RecordWildCards, OverloadedStrings #-}
module Site.Activities (activities) where

import Control.Exception (try, SomeException)
import Control.Monad (forM, forM_)
import Data.Char (isAlpha)
import Data.List (find, isInfixOf, dropWhileEnd)
import Data.List.Split (endBy, splitOn)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Time (LocalTime, parseTimeM, defaultTimeLocale, rfc822DateFormat, formatTime)
import Hakyll
import Network.HTTP.Simple (httpLBS, parseRequest, getResponseBody)
import Site.Util
import Text.Blaze.Html ((!), toValue)
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Types (Feed(..))
import Text.RSS.Syntax (RSS(..), RSSChannel(..), RSSItem(..))

data Activity = Activity { activityName   :: String
                         , activityType   :: String
                         , activityEffort :: Double
                         , activityURL    :: String
                         , activityTime   :: LocalTime
                         , activityDesc   :: String
                         } deriving (Show)

getActivities :: String -> IO [Activity]
getActivities feedURL =
  parseRequest feedURL >>= try . httpLBS >>= \case
    Left (e :: SomeException) -> return []
    Right resp -> case parseFeedSource $ getResponseBody resp of
      Nothing -> return []
      Just (RSSFeed RSS { rssChannel = RSSChannel {..} }) ->
        forM rssItems $ \RSSItem {..} -> do
          t <- parseTimeM True defaultTimeLocale rfc822DateFormat (fromJust rssItemPubDate)
          let activityName = fromJust rssItemTitle
              activityType = if "Ride" `isInfixOf` activityName then "ride" else "run"
              desc = map (\x -> let [k, v] = splitOn ": " x in (k, v))
                     . splitOn ", "
                     . head
                     . endBy " (no power meter)"
                     . drop 2
                     . dropWhile (/= ':')
                     . fromJust
                     $ rssItemDescription
              rawEffort = read
                          . dropWhileEnd isAlpha
                          . fromJust
                          . lookup "Distance"
                          $ desc
              activityEffort = if activityType == "ride" then rawEffort * 0.25 else rawEffort
          return Activity { activityName   = activityName
                          , activityType   = activityType
                          , activityEffort = activityEffort
                          , activityURL    = fromJust rssItemLink
                          , activityTime   = t
                          , activityDesc   = renderDesc desc
                          }
  where
    renderDesc :: [(String, String)] -> String
    renderDesc kvs = renderHtml $ forM_ kvs $ \(k, v) ->
      H.li ! A.title (toValue k) $ H.toHtml $ cleanVal k v

    cleanVal k v = case k of
      "Moving Time" -> case splitOn ":" v of
                          ["00", m, _] -> m <> "min"
                          [h, m, _]    -> h <> "h " <> m <> "min"
      "Pace" -> let [x, m] = splitOn "/" v in x <> "min" <> "/" <> m
      "Estimated Avg Power" -> v <> "W"
      _ -> v

activities = do
  anyDependency <- makePatternDependency "**"
  rulesExtraDependencies [anyDependency] $
    create ["activities.html"] $ do
      route indexHTMLRoute
      compile $ do
        activities <- unsafeCompiler $ getActivities "http://feedmyride.net/activities/3485865"

        let ctx = listField "activities" activityFields (mapM makeItem activities) <>
                  constField "title" "Activities" <>
                  siteContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/activities.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls
          >>= removeIndexHtml
  where
    activityField name f = field name (return . f . itemBody)

    activityFields =
      mconcat [ activityField "name" activityName
              , activityField "type" activityType
              , activityField "eff" $ show . activityEffort
              , activityField "url" activityURL
              , activityField "desc" activityDesc
              , activityField "date" (formatTime defaultTimeLocale "%b %e, %Y" . activityTime)
              ]
