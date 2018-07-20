{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections #-}
module Site.Posts where

import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale, parseTimeM, iso8601DateFormat)
import Hakyll hiding (relativizeUrls)
import Site.ERT
import Site.TOC
import Site.Util
import System.FilePath.Posix (takeBaseName, splitExtension)
import Text.Pandoc.Definition (Inline(Link, Image, Span), Block(Header, Table, Div), nullAttr)
import Text.Pandoc.Extensions (disableExtension)
import Text.Pandoc.Options
import Text.Pandoc.Walk (walk)

posts :: Tags -> String -> Rules ()
posts tags env = do
  -- post comments
  match "comments/*/*" $
    compile $ do
      ident <- getUnderlying
      tss <- getMetadataField' ident "date"
      date :: UTCTime <- parseTimeM False defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S%QZ") tss
      let dateS = formatTime defaultTimeLocale "%B %e, %Y" date
      email <- getMetadataField' ident "email"
      id <- getMetadataField' ident "_id"

      getResourceBody
        >>= renderPandocWith readerOptions writerOptions
        >>= loadAndApplyTemplate "templates/comment.html" (commentCtx id dateS email)
        >>= saveSnapshot "comment"

  -- posts
  match "posts/*" $ do
    route indexHTMLRoute
    compilePosts tags env

  -- raw posts
  match "posts/*" $ version "raw" $ do
    route   idRoute
    compile getResourceBody

  -- redirects for drafts
  match "posts/*" $ version "draft-redirects" $ do
    route (indexHTMLRoute `composeRoutes` gsubRoute "posts" (const "drafts"))
    compile $ do
      (path, _) <- splitExtension <$> getResourceFilePath
      makeItem $ Redirect ("/" ++ path ++ "/")

drafts :: Tags -> String -> Rules ()
drafts tags env = do
  match "drafts/*" $ do
    route indexHTMLRoute
    compileDrafts tags env

  create ["drafts.html"] $ do
    route indexHTMLRoute
    compile $ do
      posts <- recentFirst =<< loadAllSnapshots ("drafts/*" .&&. hasNoVersion) "content"
      let archiveCtx = listField "posts" postCtx (return posts) <>
                       tagCloudField "taglist" 100 200 (sortTagsBy caseInsensitiveTags tags) <>
                       constField "title" "Drafts"             <>
                       siteContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls env
        >>= removeIndexHtml

doCompilePosts :: Bool -> Tags -> String -> Rules ()
doCompilePosts commentsEnabled tags env = compile $ do
  alignment <- fromMaybe "left" <$> (flip getMetadataField "toc" =<< getUnderlying)

  path <- getResourceFilePath
  let postSlug = takeBaseName path

  comments <- sortComments =<< loadAllSnapshots (fromGlob $ "comments/" <> postSlug <> "/*") "comment"
  let ctx = postCtxWithTags tags <>
            constField "post_slug" postSlug <>
            constField "comment_count" (show $ length comments) <>
            listField "comments" siteContext (return comments) <>
            boolField "comments_enabled" (const commentsEnabled)

  contentCompiler postSlug alignment True
    >>= saveSnapshot "content"
    >>= loadAndApplyTemplate "templates/post.html" ctx
    >>= loadAndApplyTemplate "templates/default.html" ctx
    >>= relativizeUrls env
    >>= removeIndexHtml

compilePosts :: Tags -> String -> Rules ()
compilePosts = doCompilePosts True

compileDrafts :: Tags -> String -> Rules ()
compileDrafts = doCompilePosts False

readerOptions :: ReaderOptions
readerOptions = defaultHakyllReaderOptions {
    readerExtensions = disableExtension Ext_raw_html (readerExtensions defaultHakyllReaderOptions)
  }

writerOptions :: WriterOptions
writerOptions = defaultHakyllWriterOptions { writerEmailObfuscation = ReferenceObfuscation }

contentCompiler :: String -> String -> Bool -> Compiler (Item String)
contentCompiler postSlug alignment ertEnabled =
  pandocCompilerWithTransformM defaultHakyllReaderOptions writerOptions
    (return . estimatedReadingTime ertEnabled
            . walk linkHeaders
            . walk (addHeaderTracking postSlug)
            . walk linkImages
            . walk mkScrollableTables
            . tableOfContents alignment
            . walk blankTargetLinks)

blankTargetLinks :: Inline -> Inline
blankTargetLinks (Link (ident, classes, props) children (url, title)) =
  Link (ident, classes, props') children (url, title)
  where
    localUrlStartChars :: String
    localUrlStartChars = "/#.?"

    props' = if head url `elem` localUrlStartChars
      then props
      else props <> [("target", "_blank"), ("rel", "noopener")]
blankTargetLinks x = x

addHeaderTracking :: String -> Block -> Block
addHeaderTracking postSlug (Header level attr@(ident, cls, kvs) content) =
  if level == 2
  then Header level (ident, cls, kvs <> trackingAttrs) content
  else Header level attr content
  where
    trackingAttrs = [ ("data-track-content", "")
                    , ("data-content-name", ident)
                    , ("data-content-piece", postSlug)]
addHeaderTracking _ x = x

linkHeaders :: Block -> Block
linkHeaders (Header level attr@(ident, _, _) content) =
  Header level attr $ content <>
                      [ Link ("", ["ref-link"], []) [] ("#" <> ident, "")
                      , Link ("", ["top-link"], []) [] ("#top", "Back to top")
                      ]
linkHeaders x = x

linkImages :: Inline -> Inline
linkImages (Image (_, _, _) elems (url, _)) =
  Link ("", ["img-link"], []) [Image ("",[],[]) [] (url, ""), Span nullAttr elems] (url, "")
linkImages x = x

mkScrollableTables :: Block -> Block
mkScrollableTables table@Table{} = Div ("", ["scrollable-table"], []) [table]
mkScrollableTables x             = x

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" <>
  dateField "date_num" "%Y-%m-%d" <>
  field "ttags" ttags <>
  constField "page_type" "article" <>
  siteContext
  where
    ttags item = do
      mtags <- getMetadataField (itemIdentifier item) "tags"
      case mtags of
        Nothing -> fail "No tags found"
        Just tags -> return tags

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags <> postCtx

commentCtx :: String -> String -> String -> Context String
commentCtx id date email =
  constField "id" id <>
  constField "date" date <>
  constField "email" email <>
  siteContext

sortComments :: MonadMetadata m => [Item a] -> m [Item a]
sortComments = sortByM $ flip getMetadataField' "date" . itemIdentifier
  where
    sortByM f xs = (map fst . sortBy (comparing snd)) <$> mapM (\x -> (x,) <$> f x) xs
