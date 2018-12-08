{-# LANGUAGE OverloadedStrings #-}
module Site.Pages where

import Hakyll hiding (relativizeUrls)
import Site.Pandoc
import Site.PostCompiler
import Site.Util
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Walk (walk)

pages :: String -> Rules ()
pages env = do
  -- about page
  page "about" "about.md" env

  -- now page
  page "now" "now.md" env

page :: String -> Identifier -> String -> Rules ()
page pageName pageFile env =
  match (fromList [pageFile]) $ do
    route indexHTMLRoute
    compile $ contentCompiler (pageContentTransforms pageName)
      >>= loadAndApplyTemplate "templates/default.html" siteContext
      >>= relativizeUrls env

pageContentTransforms :: String -> Pandoc -> Pandoc
pageContentTransforms postSlug =
    walk (addHeaderTracking postSlug)
  . walk linkImages
  . walk blankTargetLinks
