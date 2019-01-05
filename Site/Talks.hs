{-# LANGUAGE OverloadedStrings #-}
module Site.Talks where

import Hakyll hiding (relativizeUrls)
import Site.Util

talks :: String -> Rules ()
talks env = do
  match "talks/*.md" $ do
    route indexHTMLRoute
    compile $ do
      title <- flip getMetadataField' "title" =<< getUnderlying
      let talkCtx =
            constField "page_type" "talk" <>
            constField "title" ("Talk » " ++  title) <>
            siteContext
      pandocCompiler
        >>= loadAndApplyTemplate "templates/talk.html" talkCtx
        >>= loadAndApplyTemplate "templates/default.html" talkCtx
        >>= relativizeUrls env
        >>= removeIndexHtml

  match "talks/*/*" $ do
    route   idRoute
    compile copyFileCompiler
