module Site.Util where

import Data.Monoid ((<>))
import Data.List (isInfixOf)
import Hakyll
import System.FilePath.Posix  (takeBaseName, takeDirectory, (</>), splitFileName)

siteRoot :: String
siteRoot = "https://abhinavsarkar.net"

indexHTMLRoute :: Routes
indexHTMLRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = let p = toFilePath ident
      in takeDirectory p </> takeBaseName p </> "index.html"

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
      return $ siteRoot <> drop 1 (takeDirectory path </> takeBaseName path <> "/")