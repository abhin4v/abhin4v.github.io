{-# LANGUAGE OverloadedStrings #-}
module Site.Pages where

import Hakyll
import Site.Util
import Site.Posts

pages = do
  -- about page
  match (fromList ["about.md"]) $ do
    route indexHTMLRoute
    compile $ contentCompiler "left" False
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls