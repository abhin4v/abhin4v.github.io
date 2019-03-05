{-# LANGUAGE LambdaCase, ScopedTypeVariables, RecordWildCards, OverloadedStrings #-}
module Site.Activities (activities) where

import Control.Exception (try, SomeException)
import Control.Monad (forM, forM_)
import Data.Char (isAlpha)
import Data.List (isInfixOf, dropWhileEnd)
import Data.List.Split (endBy, splitOn)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Time (LocalTime, parseTimeM, defaultTimeLocale, rfc822DateFormat, formatTime)
import Hakyll hiding (relativizeUrls)
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

runEffortMult, pageScaleMult :: Double
runEffortMult = 3.0
pageScaleMult = 0.8

getActivities :: String -> IO [Activity]
getActivities feedURL =
  parseRequest feedURL >>= try . httpLBS >>= \case
    Left (_ :: SomeException) -> return []
    Right resp -> case parseFeedSource $ getResponseBody resp of
      Nothing -> return []
      Just (RSSFeed RSS { rssChannel = RSSChannel {..} }) ->
        forM rssItems $ \RSSItem {..} -> do
          t <- parseTimeM True defaultTimeLocale rfc822DateFormat (T.unpack $ fromJust rssItemPubDate)
          let activityName = T.unpack $ fromJust rssItemTitle
              activityType = getActivityType activityName
              desc = map (\x -> let [k, v] = splitOn ": " x in (k, v))
                     . splitOn ", "
                     . head
                     . endBy " (no power meter)"
                     . drop 2
                     . dropWhile (/= ':')
                     . T.unpack
                     . fromJust
                     $ rssItemDescription
              rawEffort = read
                          . dropWhileEnd isAlpha
                          . fromJust
                          . lookup "Distance"
                          $ desc
              activityEffort = (if activityType == "ride" then rawEffort else rawEffort * runEffortMult) * pageScaleMult
          return Activity { activityName   = activityName
                          , activityType   = activityType
                          , activityEffort = activityEffort
                          , activityURL    = T.unpack $ fromJust rssItemLink
                          , activityTime   = t
                          , activityDesc   = renderDesc desc
                          }
      _ -> error "Impossible"
  where
    getActivityType activityName
      | "Ride" `isInfixOf` activityName = "ride"
      | "Walk" `isInfixOf` activityName = "walk"
      | "Swim" `isInfixOf` activityName = "swim"
      | otherwise = "run"

    renderDesc :: [(String, String)] -> String
    renderDesc kvs = renderHtml $ forM_ kvs $ \(k, v) ->
      H.li ! A.title (toValue k) $ H.toHtml $ cleanVal k v

    cleanVal k v = case k of
      "Moving Time" -> case splitOn ":" v of
                          ["00", m, _] -> m <> "min"
                          [h, m, _]    -> h <> "h " <> m <> "min"
                          _            -> error "Impossible"
      "Pace" -> let [x, m] = splitOn "/" v in x <> "min" <> "/" <> m
      "Estimated Avg Power" -> v <> "W"
      _ -> v

activities :: String -> Rules ()
activities env = do
  anyDependency <- makePatternDependency "**"
  rulesExtraDependencies [anyDependency] $
    create ["activities.html"] $ do
      route indexHTMLRoute
      compile $ do
        activities' <- fmap removeSwims
                       . unsafeCompiler
                       . getActivities
                       $ "http://feedmyride.net/activities/3485865"

        let ctx = listField "activities" activityCtx (mapM makeItem activities') <>
                  constField "title" "Activities" <>
                  constField "page_type" "activities" <>
                  siteContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/activities.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls env
          >>= removeIndexHtml
  where
    removeSwims = filter ((/= "swim") . activityType)

    activityField name f = field name (return . f . itemBody)

    activityCtx =
      mconcat [ activityField "name" activityName
              , activityField "type" activityType
              , activityField "eff" $ show . activityEffort
              , activityField "url" activityURL
              , activityField "desc" activityDesc
              , activityField "date" (formatTime defaultTimeLocale "%b %e" . activityTime)
              ]
