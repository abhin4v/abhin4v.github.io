{-# LANGUAGE OverloadedStrings #-}
module Site.Pages where

import Hakyll hiding (relativizeUrls)
import Site.Util
import Site.PostCompiler

pages :: String -> Rules ()
pages env = do
  -- about page
  match (fromList ["about.md"]) $ do
    route indexHTMLRoute
    compile $ contentCompiler "about" "left" False
      >>= loadAndApplyTemplate "templates/default.html" siteContext
      >>= relativizeUrls env
