--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (sortBy, isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Hakyll
import Site.TOC
import Site.ERT
import Site.Sitemap
import System.FilePath.Posix  (takeBaseName, takeDirectory, (</>), splitFileName)
import Text.Pandoc.Definition (Inline(Link))
import Text.Pandoc.Options
import Text.Pandoc.Walk

--------------------------------------------------------------------------------

siteRoot :: String
siteRoot = "https://abhinavsarkar.net"

main :: IO ()
main = hakyll $ do
  match "CNAME" $ do
    route   idRoute
    compile copyFileCompiler

  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match (fromList ["about.md"]) $ do
    route indexHTMLRoute
    compile $ contentCompiler "left" False
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  match "404.html" $ do
    route idRoute
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext

  tags <- buildTags "posts/*" (fromCapture "tags/*.html")

  match "posts/*" $ do
    route indexHTMLRoute
    compile $ do
      alignment <- fromMaybe "left" <$> (flip getMetadataField "toc" =<< getUnderlying)
      contentCompiler alignment True
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
        >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
        >>= relativizeUrls
        >>= removeIndexHtml

  match "posts/*" $ version "raw" $ do
    route   idRoute
    compile getResourceBody

  tagsRules tags $ \tag pattern -> do
    let title = "Posts tagged \"" ++ tag ++ "\""
    route indexHTMLRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let ctx = constField "title" title <> listField "posts" postCtx (return posts) <> defaultContext

      makeItem ""
          >>= loadAndApplyTemplate "templates/tag.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls
          >>= removeIndexHtml

    version "feed" $ do
      route tagFeedRoute
      compile $ loadAllSnapshots pattern "content"
          >>= fmap (take 10) . recentFirst
          >>= renderAtom (feedConfiguration title) feedCtx

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

  create ["tags.html"] $ do
    route indexHTMLRoute
    compile $ do
      let tagsCtx = field "taglist" (\_ -> renderTagList $ sortTagsBy caseInsensitiveTags tags) <>
                    constField "title" "Tags" <>
                    defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/tag-list.html" tagsCtx
        >>= loadAndApplyTemplate "templates/default.html" tagsCtx
        >>= relativizeUrls
        >>= removeIndexHtml

  create ["feed.xml"] $ do
    route idRoute
    compile $
      loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content"
      >>= fmap (take 10) . recentFirst
      >>= renderAtom (feedConfiguration "All posts") feedCtx

  create ["sitemap.xml"] $ do
    route idRoute
    compile $ generateSitemap defaultSitemapConfig { sitemapBase     = siteRoot
                                                   , sitemapRewriter = removeIndexURL
                                                   }

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

  match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
contentCompiler :: String -> Bool -> Compiler (Item String)
contentCompiler alignment ertEnabled =
  pandocCompilerWithTransformM
    defaultHakyllReaderOptions
    (defaultHakyllWriterOptions { writerHtml5 = True
                                , writerEmailObfuscation = ReferenceObfuscation
                                })
    (return . estimatedReadingTime ertEnabled
            . tableOfContents alignment
            . walk blankTargetLinks)
  where

blankTargetLinks :: Inline -> Inline
blankTargetLinks (Link (ident, classes, props) children (url, title)) =
  Link (ident, classes, props') children (url, title)
  where
    localUrlStartChars :: String
    localUrlStartChars = "/#.?"

    props' = if (head url) `elem` localUrlStartChars
      then props
      else props <> [("target", "_blank")]
blankTargetLinks x = x

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" <>
  defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags <> postCtx

feedCtx :: Context String
feedCtx =  defaultContext <>
           -- $description$ will render as the post body
           bodyField "description"

feedConfiguration :: String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
  { feedTitle = title <> " on abhinavsarkar.net"
  , feedDescription = title <> " on abhinavsarkar.net"
  , feedAuthorName = "Abhinv Sarkar"
  , feedAuthorEmail = "abhinav@abhinavsarkar.net"
  , feedRoot = siteRoot
  }

-- replace a foo/bar.md by foo/bar/index.html
-- this way the url looks like: foo/bar in most browsers
indexHTMLRoute :: Routes
indexHTMLRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = let p = toFilePath ident
      in takeDirectory p </> takeBaseName p </> "index.html"

tagFeedRoute :: Routes
tagFeedRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = let p = toFilePath ident
      in takeDirectory p </> takeBaseName p </> "feed.xml"

-- replace url of the form foo/bar/index.html by foo/bar
removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml item = return $ fmap (withUrls removeIndexURL) item

removeIndexURL :: String -> String
removeIndexURL url = case splitFileName url of
  (dir, "index.html") | isLocal dir -> dir
  _                                 -> url
  where isLocal uri = not ("://" `isInfixOf` uri)
