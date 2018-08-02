{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections #-}
module Site.PostCompiler
  ( compilePosts
  , compileDrafts
  , contentCompiler
  , readerOptions
  , writerOptions
  , postCtx
  , postCtxWithTags
  , commentCtx
  ) where

import Data.List (sortBy, intercalate)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Hakyll hiding (relativizeUrls)
import Site.ERT
import Site.TOC
import Site.Util
import System.FilePath.Posix (takeBaseName)
import Text.Pandoc.Definition (Inline(Link, Image, Span), Block(Header, Table, Div), nullAttr)
import Text.Pandoc.Extensions (disableExtension)
import Text.Pandoc.Options
import Text.Pandoc.Walk (walk)

compilePosts :: Tags -> String -> Rules ()
compilePosts = doCompilePosts True

compileDrafts :: Tags -> String -> Rules ()
compileDrafts = doCompilePosts False

doCompilePosts :: Bool -> Tags -> String -> Rules ()
doCompilePosts commentsEnabled tags env = compile $ do
  alignment <- fromMaybe "left" <$> (flip getMetadataField "toc" =<< getUnderlying)

  path <- getResourceFilePath
  let postSlug = takeBaseName path

  comments <- sortComments =<< loadAllSnapshots (fromGlob $ "comments/" <> postSlug <> "/*.md") "comment"
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

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" <>
  dateField "date_num" "%Y-%m-%d" <>
  field "tags_str" tagsStr <>
  listFieldWith "tags_list" tagCtx tagsList <>
  constField "page_type" "article" <>
  siteContext
  where
    tagsStr item = do
      tags <- getTags $ itemIdentifier item
      case tags of
        [] -> fail "No tags found"
        _  -> return $ intercalate ", " tags

    tagsList item = do
      tags <- getTags $ itemIdentifier item
      mapM makeItem tags

    tagCtx = field "tag" (return . itemBody)

commentCtx :: String -> String -> String -> Context String
commentCtx commentID date email =
  constField "id" commentID <>
  constField "date" date <>
  constField "email" email <>
  siteContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags <> postCtx

sortComments :: MonadMetadata m => [Item a] -> m [Item a]
sortComments = sortByM $ flip getMetadataField' "date" . itemIdentifier
  where
    sortByM f xs = (map fst . sortBy (comparing snd)) <$> mapM (\x -> (x,) <$> f x) xs

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
