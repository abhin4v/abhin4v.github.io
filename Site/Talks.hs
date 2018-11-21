{-# LANGUAGE OverloadedStrings #-}
module Site.Talks where

import Hakyll hiding (relativizeUrls)
import Site.Util

talks :: String -> Rules ()
talks env =
  match "talks/*.md" $ do
    route indexHTMLRoute
    compile $ do
      let talkCtx = 
            constField "page_type" "talk" <> 
            metadataField <>
            siteContext
      pandocCompiler
        >>= loadAndApplyTemplate "templates/talk.html" talkCtx
        >>= loadAndApplyTemplate "templates/default.html" talkCtx
        >>= relativizeUrls env
        >>= removeIndexHtml
  