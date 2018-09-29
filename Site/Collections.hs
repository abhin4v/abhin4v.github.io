{-# LANGUAGE OverloadedStrings, TupleSections, ScopedTypeVariables #-}
module Site.Collections where

import Control.Monad (forM)
import Data.Function (on)
import Data.Monoid ((<>))
import Data.List (groupBy)
import Data.String (fromString)
import Data.Time (formatTime, defaultTimeLocale)
import Hakyll hiding (relativizeUrls, renderAtom)
import Site.PostCompiler
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
      posts <- groupPostsByYear =<< recentFirst =<< loadAll pattrn
      let ctx = constField "title" title                        <>
                constField "page_type" "archive"                <>
                constField "tag" tag                            <>
                listField "posts" defaultContext (return posts) <>
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
          >>= return . map (fmap (relativizeUrlsWith siteRoot) . addTrackingImg . addPostLink)
          >>= renderAtom (feedConfiguration siteRoot title) feedCtx

  -- post archive
  tagsDependency <- makePatternDependency "tags/*.html"
  rulesExtraDependencies [tagsDependency] $
    create ["archive.html"] $ do
      route indexHTMLRoute
      compile $ do
        posts <- groupPostsByYear =<< recentFirst =<< loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content"
        let archiveCtx = listField "posts" defaultContext (return posts) <>
                         tagCloudField "taglist" 100 200 (sortTagsBy caseInsensitiveTags tags) <>
                         constField "title" "Archive" <>
                         constField "page_type" "archive" <>
                         siteContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
          >>= loadAndApplyTemplate "templates/default.html" archiveCtx
          >>= relativizeUrls env
          >>= removeIndexHtml

  -- drafts
  create ["drafts.html"] $ do
    route indexHTMLRoute
    compile $ do
      posts <- groupPostsByYear =<< recentFirst =<< loadAllSnapshots ("drafts/*.md" .&&. hasNoVersion) "content"
      let archiveCtx = listField "posts" defaultContext (return posts) <>
                        tagCloudField "taglist" 100 200 (sortTagsBy caseInsensitiveTags tags) <>
                        constField "title" "Drafts" <>
                        constField "page_type" "archive" <>
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
      >>= return . map (fmap (relativizeUrlsWith siteRoot) . addTrackingImg . addPostLink)
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
          morePosts = morePostCountW <> " more post" <> (if morePostCount == 1 then "" else "s")
      let indexCtx =
            listField "posts" postCtx (return posts) <>
            constField "title" "Home"                <>
            constField "page_type" "website"         <>
            constField "more_posts" morePosts        <>
            siteContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls env
        >>= removeIndexHtml
  where
    postCtx = teaserField "teaser" "content" <>
      postCtxWithTags tags <>
      field "post_ert" (fmap itemBody . flip loadSnapshot "ert" . itemIdentifier) <>
      field "comment_count" (fmap itemBody . flip loadSnapshot "comment_count" . itemIdentifier)

    getPostYear post =
      formatTime defaultTimeLocale "%Y" <$> getItemUTC defaultTimeLocale (itemIdentifier post)

    groupPostsByYear posts = do
      postsByYear <- groupBy ((==) `on` snd) <$> mapM (\p -> (p,) <$> getPostYear p) posts
      forM postsByYear $ \postYears -> do
        let posts = map fst postYears
            year = head . map snd $ postYears
            ctx = listField "posts" postCtx (return posts) <>
                  constField "year" year <>
                  siteContext

        makeItem "" >>= loadAndApplyTemplate "templates/post-list.html" ctx

feedCtx :: Context String
feedCtx =  bodyField "description" <> field "url" postUrl <> postCtx <> siteContext
  where
    postUrl item = do
      let path = toFilePath (itemIdentifier item)
      return $ "/" <> takeDirectory path </> takeBaseName path <> "/"

feedConfiguration :: String -> String -> FeedConfiguration
feedConfiguration siteRoot title = FeedConfiguration
  { feedTitle       = title <> " on abhinavsarkar.net"
  , feedDescription = title <> " on abhinavsarkar.net"
  , feedAuthorName  = "Abhinav Sarkar"
  , feedAuthorEmail = "abhinav@abhinavsarkar.net"
  , feedRoot        = siteRoot
  }

addTrackingImg :: Item String -> Item String
addTrackingImg item = itemSetBody (itemBody item <> trackingImg) item
  where
    trackingImg =
      "<img src=\"https://anna.abhinavsarkar.net/piwik.php?idsite=1&amp;rec=1\" style=\"border:0; display: none;\" />"

addPostLink :: Item String -> Item String
addPostLink item = let
    p = toFilePath (itemIdentifier item)
    postUrl = "/" <> takeDirectory p </> takeBaseName p <> "/"
    postLink = renderHtml $ H.p $ do
      H.text "If you liked this post, please "
      H.a ! A.href (fromString $ postUrl <> "#comment-container") $ "leave a comment"
      H.text "."
  in itemSetBody (itemBody item <> postLink) item

renderAtom :: FeedConfiguration -> Context String -> [Item String] -> Compiler (Item String)
renderAtom config context items = do
  atomTemplate     <- unsafeCompiler $ readFile "templates/atom.xml"
  atomItemTemplate <- unsafeCompiler $ readFile "templates/atom-item.xml"
  renderAtomWithTemplates atomTemplate atomItemTemplate config context items
