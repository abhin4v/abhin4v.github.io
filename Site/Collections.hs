{-# LANGUAGE OverloadedStrings #-}
module Site.Collections where

import Data.Monoid ((<>))
import Data.String (fromString)
import Hakyll hiding (relativizeUrls)
import Site.Posts
import Site.Sitemap
import Site.Util
import System.FilePath.Posix  (takeBaseName, takeDirectory, (</>))
import Text.Blaze.Html ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String (renderHtml)

collections :: Tags -> String -> Rules ()
collections tags env = do
  -- tag pages
  tagsRules tags $ \tag pattrn -> do
    let title = "Posts tagged ‘" ++ tag ++ "’"
    route indexHTMLRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattrn
      let ctx = constField "title" title <>
                constField "tag" tag <>
                listField "posts" postCtx (return posts) <>
                siteContext

      makeItem ""
          >>= loadAndApplyTemplate "templates/tag.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls env
          >>= removeIndexHtml

    -- tag feeds
    version "feed" $ do
      route tagFeedRoute
      compile $ loadAllSnapshots pattrn "content"
          >>= fmap (take 10) . recentFirst
          >>= return . map (fmap (relativizeUrlsWith siteRoot) . addPostLink)
          >>= renderAtom (feedConfiguration siteRoot title) feedCtx

  -- post archive
  tagsDependency <- makePatternDependency "tags/*.html"
  rulesExtraDependencies [tagsDependency] $
    create ["archive.html"] $ do
      route indexHTMLRoute
      compile $ do
        posts <- recentFirst =<< loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content"
        let archiveCtx = listField "posts" postCtx (return posts) <>
                         tagCloudField "taglist" 100 200 (sortTagsBy caseInsensitiveTags tags) <>
                         constField "title" "Archive"             <>
                         siteContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
          >>= loadAndApplyTemplate "templates/default.html" archiveCtx
          >>= relativizeUrls env
          >>= removeIndexHtml

  -- posts index
  create ["posts.html"] $ do
    route indexHTMLRoute
    compile $ makeItem $ Redirect "/archive/"

  -- tags index
  create ["tags.html"] $ do
    route indexHTMLRoute
    compile $ makeItem $ Redirect "/archive/"

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
      let indexPostCount = 3
      allPosts <- loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content"
      posts <- take indexPostCount <$> recentFirst allPosts
      let morePostCount = length allPosts - indexPostCount
          morePostCountW = numToWord morePostCount
          morePosts = if morePostCount == 1
                      then morePostCountW <> " more post"
                      else morePostCountW <> " more posts"
      let indexCtx =
            listField "posts" (teaserField "teaser" "content" <> postCtxWithTags tags) (return posts) <>
            constField "title" "Home"                                                                 <>
            constField "more_posts" morePosts                                                         <>
            siteContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls env
        >>= removeIndexHtml

feedCtx :: Context String
feedCtx =  bodyField "description" <> field "url" postUrl <> siteContext
  where
    postUrl item = do
      let path = toFilePath (itemIdentifier item)
      return $ ("/" <> takeDirectory path </> takeBaseName path <> "/")

feedConfiguration :: String -> String -> FeedConfiguration
feedConfiguration siteRoot title = FeedConfiguration
  { feedTitle       = title <> " on abhinavsarkar.net"
  , feedDescription = title <> " on abhinavsarkar.net"
  , feedAuthorName  = "Abhinv Sarkar"
  , feedAuthorEmail = "abhinav@abhinavsarkar.net"
  , feedRoot        = siteRoot
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
