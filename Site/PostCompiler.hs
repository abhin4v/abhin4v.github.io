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
import System.FilePath.Posix (takeBaseName, takeDirectory)
import Text.Pandoc.Definition (Inline(Link, Image, Span), Block(Header, Table, Div), nullAttr)
import Text.Pandoc.Extensions (disableExtension)
import Text.Pandoc.Options
import Text.Pandoc.Walk (walk)

compilePosts :: Tags -> String -> [Identifier] -> Rules ()
compilePosts = doCompilePosts True

compileDrafts :: Tags -> String -> [Identifier] -> Rules ()
compileDrafts = doCompilePosts False

doCompilePosts :: Bool -> Tags -> String -> [Identifier] -> Rules ()
doCompilePosts commentsEnabled tags env posts = compile $ do
  post         <- getUnderlying
  alignment    <- fromMaybe "left" <$> getMetadataField post "toc"
  path         <- getResourceFilePath
  let postSlug = takeBaseName path
  comments     <- sortComments =<< loadAllSnapshots (fromGlob $ "comments/" <> postSlug <> "/*.md") "comment"
  sortedPosts  <- sortChronological posts

  nextPostCtx <- navLinkCtx "next_post" $ sortedPosts `itemAfter` post
  prevPostCtx <- navLinkCtx "prev_post" $ sortedPosts `itemBefore` post

  let ctx = postCtxWithTags tags <>
            nextPostCtx <> prevPostCtx <>
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

navLinkCtx :: String -> Maybe Identifier -> Compiler (Context String)
navLinkCtx key mIdent = case mIdent of
  Nothing    -> return mempty
  Just ident -> do
    title <- getMetadataField' ident "title"
    let filePath = toFilePath ident
        url = "/" <> takeDirectory filePath <> "/" <> takeBaseName filePath <> "/"
    return $ constField (key <> "_url") url <> constField (key <> "_title") title

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

commentCtx :: String -> String -> String -> String -> Context String
commentCtx commentID date datetime email =
  constField "id" commentID <>
  constField "date" date <>
  constField "datetime" datetime <>
  constField "email" email <>
  siteContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags <> postCtx

sortComments :: MonadMetadata m => [Item a] -> m [Item a]
sortComments = sortByM $ flip getMetadataField' "date" . itemIdentifier
  where
    sortByM f xs = map fst . sortBy (comparing snd) <$> mapM (\ x -> (x,) <$> f x) xs

itemAfter, itemBefore :: Eq a => [a] -> a -> Maybe a
itemAfter xs x  = lookup x $ zip xs (tail xs)
itemBefore xs x = lookup x $ zip (tail xs) xs

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
