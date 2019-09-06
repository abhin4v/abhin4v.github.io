{-# LANGUAGE LambdaCase, ScopedTypeVariables, RecordWildCards, OverloadedStrings #-}
module Site.Activities (activities) where

import Control.Exception (try, SomeException)
import Control.Monad (forM, forM_)
import Data.Char (isAlpha)
import Data.List (isInfixOf, dropWhileEnd)
import Data.List.Split (endBy, splitOn)
import Data.Maybe (fromJust, catMaybes)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Time (LocalTime, parseTimeM, defaultTimeLocale, rfc822DateFormat, formatTime)
import Hakyll hiding (relativizeUrls)
import Network.HTTP.Simple (httpLBS, parseRequest, getResponseBody, getResponseStatusCode)
import Site.Util
import Text.Blaze.Html ((!), toValue)
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Types (Feed(..))
import Text.RSS.Syntax (RSS(..), RSSChannel(..), RSSItem(..))

data ActivityType = Ride | Run | Walk | Swim | Unknown deriving (Eq)

instance Show ActivityType where
  show Ride    = "ride"
  show Run     = "run"
  show Walk    = "walk"
  show Swim    = "swim"
  show Unknown = "unknown"

getActivityType :: String -> ActivityType
getActivityType activityName
  | "Ride" `isInfixOf` activityName = Ride
  | "Run" `isInfixOf` activityName  = Run
  | "Walk" `isInfixOf` activityName = Walk
  | "Swim" `isInfixOf` activityName = Swim
  | otherwise = Unknown

allowedActivityTypes :: [ActivityType]
allowedActivityTypes = [Ride, Run, Walk]

data Activity = Activity { activityName   :: String
                         , activityType   :: ActivityType
                         , activityEffort :: Double
                         , activityURL    :: String
                         , activityTime   :: LocalTime
                         , activityDesc   :: String
                         } deriving (Show)

getActivities :: String -> IO [Activity]
getActivities feedURL =
  parseRequest feedURL >>= try . httpLBS >>= \case
    Left (_ :: SomeException) -> return []
    Right resp | getResponseStatusCode resp == 200 ->
      case parseFeedSource $ getResponseBody resp of
        Nothing -> return []
        Just (RSSFeed RSS { rssChannel = RSSChannel {..} }) -> parseRSSItems rssItems
        _ -> error "Impossible"
    Right _ -> return []

parseRSSItems :: [RSSItem] -> IO [Activity]
parseRSSItems rssItems = fmap catMaybes . forM rssItems $ \RSSItem {..} -> do
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
      activityEffort = if activityType == Ride then rawEffort else rawEffort * runEffortMult
  return $ if activityType `elem` allowedActivityTypes
    then Just $ Activity { activityName   = activityName
                         , activityType   = activityType
                         , activityEffort = activityEffort
                         , activityURL    = T.unpack $ fromJust rssItemLink
                         , activityTime   = t
                         , activityDesc   = renderDesc desc
                         }
    else Nothing
  where
    runEffortMult = 3.0

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
        activities' <- unsafeCompiler
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
    activityField name f = field name (return . f . itemBody)

    activityCtx =
      mconcat [ activityField "name" activityName
              , activityField "type" $ show . activityType
              , activityField "width" $ show . (* widthMult) . activityEffort
              , activityField "max_width" $ show . (* maxWidthMult) . activityEffort
              , activityField "url" activityURL
              , activityField "desc" activityDesc
              , activityField "date" (formatTime defaultTimeLocale "%b %e" . activityTime)
              ]

    widthMult = 0.75
    maxWidthMult = 0.6
