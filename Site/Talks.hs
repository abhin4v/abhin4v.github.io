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

  match ("slides/*/*" .&&. complement "slides/*/*.md") $ do
    route   idRoute
    compile copyFileCompiler

  match "slides/*/*.md" $ do
    route dirIndexHTMLRoute
    compile $ do
      let slidesCtx =
            boolField "live_reload" (const $ env == "DEV") <>
            constField "page_type" "slides" <>
            siteContext
      getResourceBody
        >>= loadAndApplyTemplate "templates/slides.html" slidesCtx
        >>= relativizeUrls env
