module Site.Util where

import Data.Monoid ((<>))
import Data.List (isInfixOf)
import Hakyll hiding (customRoute)
import qualified Hakyll as H
import System.FilePath.Posix  (takeBaseName, takeDirectory, (</>), splitFileName)
import qualified Text.Numeral.Language.ENG as EN
import qualified Text.Numeral as Num
import qualified Data.Text as T

siteRoot :: String
siteRoot = "https://abhinavsarkar.net"

numToWord :: Int -> String
numToWord n = maybe (show n) T.unpack (EN.us_cardinal Num.defaultInflection n)

indexHTMLRoute :: Routes
indexHTMLRoute = customRoute "index.html"

tagFeedRoute :: Routes
tagFeedRoute = customRoute "feed.xml"

customRoute :: String -> Routes
customRoute fileName = H.customRoute createIndexRoute
  where
    createIndexRoute ident = let p = toFilePath ident
      in takeDirectory p </> takeBaseName p </> fileName

dirIndexHTMLRoute :: Routes
dirIndexHTMLRoute = H.customRoute createDirIndexRoute
  where
    createDirIndexRoute ident =
      takeDirectory (toFilePath ident) </> "index.html"

removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml item = return $ fmap (withUrls removeIndexURL) item

removeIndexURL :: String -> String
removeIndexURL url = case splitFileName url of
  (dir, "index.html") | isLocal dir -> dir
  _                                 -> url
  where isLocal uri = not ("://" `isInfixOf` uri)

siteContext :: Context String
siteContext = defaultContext <> field "full_url" (const fullUrl)
  where
    fullUrl = do
      path <- getResourceFilePath
      return $ siteRoot <> case path of
        "./index.html" -> ""
        "./404.html"   -> "/404.html"
        _              -> drop 1 (takeDirectory path </> takeBaseName path <> "/")

relativizeUrls :: String -> Item String -> Compiler (Item String)
relativizeUrls env item = do
  route <- getRoute $ itemIdentifier item
  return $ case route of
    Nothing -> item
    Just r  -> flip fmap item $ H.relativizeUrlsWith $ case env of
      "DEV" -> toSiteRoot r
      _     -> drop 6 siteRoot