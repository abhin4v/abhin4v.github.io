{-# LANGUAGE OverloadedStrings #-}
module Site.Pages where

import Data.List (sortOn)
import Data.Maybe (catMaybes)
import qualified Data.Ord
import Hakyll hiding (relativizeUrls)
import Site.Pandoc
import Site.PostCompiler
import Site.Util
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Walk (walk)

pages :: String -> Rules ()
pages env = do
  -- about page
  page "about" (fromList ["about.md"]) env

  -- projects page
  page "projects" (fromList ["projects.md"]) env

  nowPages env

nowPages :: String -> Rules ()
nowPages env = do
  page "now" "now/*.md" env

  match (fromList ["now.md"]) $ do
    nows <- sortOn Data.Ord.Down <$> getMatches "now/*.md"

    route indexHTMLRoute
    compile $ do
      nowPaths <- map removeIndexURL . catMaybes <$> mapM getRoute nows
      let ctx = listField "nows" nowCtx (mapM makeItem nowPaths) <> siteContext

      contentCompiler (pageContentTransforms "now")
        >>= loadAndApplyTemplate "templates/now.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" siteContext
        >>= relativizeUrls env
  where
    nowCtx = field "path" (return . itemBody) <>
             field "date" (return . getDate . itemBody)

    getDate = take 10 . drop 4

page :: String -> Pattern -> String -> Rules ()
page pageName pat env =
  match pat $ do
    route indexHTMLRoute
    compile $ contentCompiler (pageContentTransforms pageName)
      >>= loadAndApplyTemplate "templates/default.html" siteContext
      >>= relativizeUrls env

pageContentTransforms :: String -> Pandoc -> Pandoc
pageContentTransforms postSlug =
    walk (addHeaderTracking postSlug)
  . walk linkImages
  . walk blankTargetLinks
