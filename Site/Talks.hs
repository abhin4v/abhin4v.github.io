{-# LANGUAGE OverloadedStrings #-}
module Site.Talks where

import Data.Maybe (isJust)
import Hakyll hiding (relativizeUrls)
import Site.Util

talks :: String -> Rules ()
talks env = do
  match "talks/*.md" $ do
    route indexHTMLRoute
    compile $ do
      ident    <- getUnderlying
      title    <- getMetadataField' ident "title"
      isRemark <- isJust <$> getMetadataField ident "slides_path"
      aRatio   <- read <$> getMetadataField' ident "slides_ratio"

      let talkCtx =
            constField "page_type" "talk" <>
            constField "title" ("Talk » " ++  title) <>
            (if isRemark then constField "inv_aratio" (invARatio aRatio) else mempty) <>
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
  where
    invARatio = show . floor . (100 /)
