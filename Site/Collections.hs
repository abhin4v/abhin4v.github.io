{-# LANGUAGE OverloadedStrings #-}
module Site.Collections where

import Data.Monoid ((<>))
import Data.String (fromString)
import Hakyll
import Site.Posts
import Site.Sitemap
import Site.Util
import System.FilePath.Posix  (takeBaseName, takeDirectory, (</>))
import Text.Blaze.Html ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String (renderHtml)

collections :: String -> Tags -> Rules ()
collections siteRoot tags = do
  -- tag pages
  tagsRules tags $ \tag pattrn -> do
    let title = "Posts tagged ‘" ++ tag ++ "’"
    route indexHTMLRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattrn
      let ctx = constField "title" title <>
                constField "tag" tag <>
                listField "posts" postCtx (return posts) <>
                defaultContext

      makeItem ""
          >>= loadAndApplyTemplate "templates/tag.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls
          >>= removeIndexHtml

    -- tag feeds
    version "feed" $ do
      route tagFeedRoute
      compile $ loadAllSnapshots pattrn "content"
          >>= fmap (take 10) . recentFirst
          >>= return . map (fmap (relativizeUrlsWith siteRoot) . addPostLink)
          >>= renderAtom (feedConfiguration siteRoot title) feedCtx

  -- tags page
  tagsDependencies <- makePatternDependency "tags/*.html"
  rulesExtraDependencies [tagsDependencies] $
    create ["tags.html"] $ do
      route indexHTMLRoute
      compile $ do
        let tagsCtx = tagCloudField "taglist" 100 200 (sortTagsBy caseInsensitiveTags tags) <>
                      constField "title" "Tags" <>
                      defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/tag-list.html" tagsCtx
          >>= loadAndApplyTemplate "templates/default.html" tagsCtx
          >>= relativizeUrls
          >>= removeIndexHtml

  -- post archive
  create ["archive.html"] $ do
    route indexHTMLRoute
    compile $ do
      posts <- recentFirst =<< loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content"
      let archiveCtx =
            listField "posts" postCtx (return posts) <>
            constField "title" "Archives"            <>
            defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls
        >>= removeIndexHtml

  -- main feed
  create ["feed.xml"] $ do
    route idRoute
    compile $
      loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content"
      >>= fmap (take 10) . recentFirst
      >>= return . map (fmap (relativizeUrlsWith siteRoot) . addPostLink)
      >>= renderAtom (feedConfiguration siteRoot "All posts") feedCtx

  -- sitemap
  create ["sitemap.xml"] $ do
    route idRoute
    compile $ generateSitemap defaultSitemapConfig { sitemapBase     = siteRoot
                                                   , sitemapRewriter = removeIndexURL
                                                   }

  -- home page
  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content"
      let indexCtx =
            listField "posts" (teaserField "teaser" "content" <> postCtx) (return posts) <>
            constField "title" "Home"                                                    <>
            defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls
        >>= removeIndexHtml

feedCtx :: Context String
feedCtx =  defaultContext <>
           -- $description$ will render as the post body
           bodyField "description"

feedConfiguration :: String -> String -> FeedConfiguration
feedConfiguration siteRoot title = FeedConfiguration
  { feedTitle = title <> " on abhinavsarkar.net"
  , feedDescription = title <> " on abhinavsarkar.net"
  , feedAuthorName = "Abhinv Sarkar"
  , feedAuthorEmail = "abhinav@abhinavsarkar.net"
  , feedRoot = siteRoot
  }

tagFeedRoute :: Routes
tagFeedRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = let p = toFilePath ident
      in takeDirectory p </> takeBaseName p </> "feed.xml"

addPostLink :: Item String -> Item String
addPostLink item = let
    p = toFilePath (itemIdentifier item)
    postUrl = "/" <> takeDirectory p </> takeBaseName p <> "/"
    postLink = renderHtml $ H.p $ do
      H.text "If you liked this post, please "
      H.a ! A.href (fromString $ postUrl <> "#comment-container") $ "leave a comment"
      H.text "."
  in itemSetBody (itemBody item <> postLink) item
