{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections, RecordWildCards #-}
module Site.PostCompiler where

import Control.Monad (foldM, void)
import Data.List (stripPrefix, sortOn, intercalate, (\\))
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import qualified Data.Tree as Tree
import Hakyll hiding (relativizeUrls)
import Site.ERT
import Site.Pandoc
import Site.TOC
import Site.Util
import qualified Site.Webmentions as WM
import System.FilePath.Posix (takeBaseName, takeDirectory)
import Text.Pandoc.Definition (Pandoc)
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

  comments <- sortComments =<< loadAllSnapshots (fromGlob $ "comments/" <> postSlug <> "/*.md") "comment"
  let commentCount = show $ length comments

  mentions <- if commentsEnabled then unsafeCompiler $ WM.getWebmentions postSlug else return Nothing

  sortedPosts <- sortChronological posts
  nextPostCtx <- navLinkCtx "next_post" $ sortedPosts `itemAfter` post
  prevPostCtx <- navLinkCtx "prev_post" $ sortedPosts `itemBefore` post

  content <- readContentWithPandoc
  let ert = timeEstimate . itemBody $ content

  void $ makeItem ert >>= saveSnapshot "ert"
  void $ makeItem commentCount >>= saveSnapshot "comment_count"

  let ctx = postCtxWithTags tags <>
            nextPostCtx <> prevPostCtx <>
            constField "post_slug" postSlug <>
            constField "post_ert" ert <>
            constField "comment_count" commentCount <>
            listField "comments" siteContext (return comments) <>
            boolField "comments_enabled" (const commentsEnabled) <>
            boolField "live_reload" (const $ env == "DEV")

  let ctxWithMentions = case mentions of
        Nothing -> ctx
        Just wm -> ctx <> WM.webmentionsCtx wm

  pandocContentCompiler (postContentTransforms postSlug alignment) content
    >>= saveSnapshot "content"
    >>= loadAndApplyTemplate "templates/post.html" ctxWithMentions
    >>= loadAndApplyTemplate "templates/default.html" ctxWithMentions
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

maxCommentLevel :: Int
maxCommentLevel = 3

commentCtx :: String -> String -> String -> String -> Int -> Context String
commentCtx commentID date datetime email level =
  constField "id" commentID <>
  constField "date" date <>
  constField "datetime" datetime <>
  constField "email" email <>
  constField "level" (show level) <>
  boolField "reply_allowed" (const $ level < maxCommentLevel) <>
  boolField "author_reply" (const $ email == authorEmail) <>
  siteContext
  where
    authorEmail = "4d29918c109bc75d2a1fd8420660d72b"

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags <> postCtx

commentReplyToMap :: MonadMetadata m => [Identifier] -> m (Map.Map Identifier Identifier)
commentReplyToMap comments =
  flip (`foldM` Map.empty) comments $ \acc comment -> do
    let postSlug = fromJust . stripPrefix "comments/" . takeDirectory . toFilePath $ comment
    replyToID <- getMetadataField' comment "reply_to"
    case replyToID of
      "" -> return acc
      _  -> do
        parentComments <- getMatches $ fromGlob $ "comments/" <> postSlug <> "/" <> replyToID <> ".md"
        case parentComments of
          []              -> return acc
          [parentComment] -> return $ Map.insert comment parentComment acc
          _               -> error "Impossibru"

sortComments :: MonadMetadata m => [Item a] -> m [Item a]
sortComments items = do
  let itemIdentifierMap =
        foldl (\acc i -> Map.insert (itemIdentifier i) i acc) Map.empty items

  replyToMap <- commentReplyToMap . map itemIdentifier $ items
  let replyFromMap =
        Map.foldlWithKey (\acc k v -> Map.insertWith (++) v [k] acc) Map.empty replyToMap

  topLevelCommentIDs <- sortByDate $ Map.keys itemIdentifierMap \\ Map.keys replyToMap
  commentIDForest    <- flip Tree.unfoldForestM_BF topLevelCommentIDs $ \commentID ->
    (commentID,) <$> sortByDate (Map.findWithDefault [] commentID replyFromMap)

  return . map (itemIdentifierMap Map.!) . concatMap Tree.flatten $ commentIDForest
  where
    sortByDate xs =
      map fst . sortOn snd
      <$> mapM (\ x -> (x,) <$> getMetadataField' x "date") xs

itemAfter, itemBefore :: Eq a => [a] -> a -> Maybe a
itemAfter xs x  = lookup x $ zip xs (tail xs)
itemBefore xs x = lookup x $ zip (tail xs) xs

noHTMLreaderOptions :: ReaderOptions
noHTMLreaderOptions = defaultHakyllReaderOptions {
    readerExtensions = disableExtension Ext_raw_html (readerExtensions defaultHakyllReaderOptions)
  }

writerOptions :: WriterOptions
writerOptions = defaultHakyllWriterOptions { writerEmailObfuscation = ReferenceObfuscation }

readContentWithPandoc :: Compiler (Item Pandoc)
readContentWithPandoc = getResourceBody >>= readPandocWith defaultHakyllReaderOptions

readContentWithPandocWith :: ReaderOptions -> Compiler (Item Pandoc)
readContentWithPandocWith readerOptions = getResourceBody >>= readPandocWith readerOptions

pandocContentCompiler :: (Pandoc -> Pandoc) -> Item Pandoc -> Compiler (Item String)
pandocContentCompiler transform = return . writePandocWith writerOptions . fmap transform

contentCompiler :: (Pandoc -> Pandoc) -> Compiler (Item String)
contentCompiler transform = readContentWithPandoc >>= pandocContentCompiler transform

contentCompilerWith :: ReaderOptions -> WriterOptions -> (Pandoc -> Pandoc) -> Compiler (Item String)
contentCompilerWith readerOp writerOp transform =
  writePandocWith writerOp . fmap transform <$> readContentWithPandocWith readerOp

postContentTransforms :: String -> String -> Pandoc -> Pandoc
postContentTransforms postSlug alignment =
    walk linkHeaders
  . walk (addHeaderTracking postSlug)
  . walk linkImages
  . walk mkScrollableTables
  . tableOfContents alignment
  . walk blankTargetLinks
  . walk expandWikiLinks
  . walk emphasizeCode
