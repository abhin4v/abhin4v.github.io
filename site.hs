--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections #-}

module Main where

import Control.Monad (liftM)
import Data.List (sortBy, isInfixOf)
import Data.Maybe (fromMaybe, fromJust)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale, parseTimeM, iso8601DateFormat)
import Hakyll
import Hakyll.Web.Sass
import Site.TOC
import Site.ERT
import Site.Sitemap
import System.FilePath.Posix  (takeBaseName, takeDirectory, (</>), splitFileName)
import Text.Pandoc.Definition (Inline(Link), Block(Header))
import Text.Pandoc.Options
import Text.Pandoc.Walk

--------------------------------------------------------------------------------

siteRoot :: String
siteRoot = "https://abhinavsarkar.net"

main :: IO ()
main = hakyll $ do
  match (fromList ["CNAME", "robots.txt", "staticman.yml"]) $ do
    route   idRoute
    compile copyFileCompiler

  match ("images/*" .||. "js/*") $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*.css" $ do
    route   idRoute
    compile compressCssCompiler

  scssDependencies <- makePatternDependency "css/_*.scss"
  rulesExtraDependencies [scssDependencies] $ do
    match "css/default.scss" $ do
      route $ setExtension "css"
      compile (fmap compressCss <$> sassCompiler)

  match (fromList ["about.md"]) $ do
    route indexHTMLRoute
    compile $ contentCompiler "left" False
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  match "404.html" $ do
    route idRoute
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext

  match "comments/*/*" $ do
    compile $ do
      ident <- getUnderlying
      tss <- getMetadataField' ident "date"
      date :: UTCTime <- parseTimeM False defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S%QZ") tss
      let dateS = formatTime defaultTimeLocale "%B %e, %Y" date
      email <- getMetadataField' ident "email"
      message <- getMetadataField' ident "message"

      (itemSetBody message <$> makeItem "")
        >>= renderPandocWith
              (defaultHakyllReaderOptions { readerExtensions = Set.delete Ext_raw_html (writerExtensions writerOptions) })
              writerOptions
        >>= loadAndApplyTemplate "templates/comment.html" (commentCtx dateS email)
        >>= saveSnapshot "comment"

  tags <- buildTags "posts/*" (fromCapture "tags/*.html")

  match "posts/*" $ do
    route indexHTMLRoute
    compile $ do
      alignment <- fromMaybe "left" <$> (flip getMetadataField "toc" =<< getUnderlying)
      path <- getResourceFilePath
      let postSlug = takeBaseName path
      let fullUrl = siteRoot <> drop 1 (takeDirectory path </> postSlug <> "/")

      comments <- sortComments =<< loadAllSnapshots (fromGlob $ "comments/" <> postSlug <> "/*") "comment"
      contentCompiler alignment True
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags <>
                                                           constField "full_url" fullUrl <>
                                                           constField "post_slug" postSlug <>
                                                           constField "comment_count" (show $ length comments) <>
                                                           listField "comments" defaultContext (return comments))
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
writerOptions = defaultHakyllWriterOptions { writerHtml5 = True
                                           , writerEmailObfuscation = ReferenceObfuscation
                                           }

contentCompiler :: String -> Bool -> Compiler (Item String)
contentCompiler alignment ertEnabled =
  pandocCompilerWithTransformM defaultHakyllReaderOptions writerOptions
    (return . estimatedReadingTime ertEnabled
            . walk linkHeaders
            . tableOfContents alignment
            . walk blankTargetLinks)

blankTargetLinks :: Inline -> Inline
blankTargetLinks (Link (ident, classes, props) children (url, title)) =
  Link (ident, classes, props') children (url, title)
  where
    localUrlStartChars :: String
    localUrlStartChars = "/#.?"

    props' = if (head url) `elem` localUrlStartChars
      then props
      else props <> [("target", "_blank"), ("rel", "noopener")]
blankTargetLinks x = x

linkHeaders :: Block -> Block
linkHeaders (Header level attr@(ident, _, _) content) =
  Header level attr $ content <>
                      [ Link ("", ["ref-link"], []) [] ("#" <> ident, "")
                      , Link ("", ["top-link"], []) [] ("#top", "Back to top")
                      ]
linkHeaders x = x

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" <>
  dateField "date_num" "%Y-%m-%d" <>
  defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags <> postCtx

commentCtx :: String -> String -> Context String
commentCtx date email =
  constField "date" date <>
  constField "email" email <>
  defaultContext

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

sortComments :: MonadMetadata m => [Item a] -> m [Item a]
sortComments = sortByM $ flip getMetadataField' "date" . itemIdentifier
  where
    sortByM f xs = liftM (map fst . sortBy (comparing snd)) $
                   mapM (\x -> liftM (x,) (f x)) xs
