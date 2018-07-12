{-# LANGUAGE OverloadedStrings #-}
module Site.Pages where

import Hakyll
import Site.Util
import Site.Posts

pages :: Rules ()
pages = do
  -- about page
  match (fromList ["about.md"]) $ do
    route indexHTMLRoute
    compile $ contentCompiler "about" "left" False
      >>= loadAndApplyTemplate "templates/default.html" siteContext
      >>= relativizeUrls
