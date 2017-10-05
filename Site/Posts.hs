{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections #-}
module Site.Posts where

import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale, parseTimeM, iso8601DateFormat)
import Hakyll
import Site.ERT
import Site.TOC
import Site.Util
import System.FilePath.Posix  (takeBaseName, takeDirectory, (</>))
import Text.Blaze.Html ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Pandoc.Definition (Inline(Link), Block(Header))
import Text.Pandoc.Options
import Text.Pandoc.Walk (walk)

posts :: String -> Tags -> Rules ()
posts siteRoot tags = do
  -- post comments
  match "comments/*/*" $
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

  -- posts
  match "posts/*" $ do
    route indexHTMLRoute
    compile $ do
      alignment <- fromMaybe "left" <$> (flip getMetadataField "toc" =<< getUnderlying)

      path <- getResourceFilePath
      let postSlug = takeBaseName path
      let postUrl = drop 1 (takeDirectory path </> postSlug <> "/")
      let fullUrl = siteRoot <> postUrl
      let pleaseComment = renderHtml $ H.p $ do
            H.text "If you liked this post, please "
            H.a ! A.href (fromString $ postUrl <> "#comment-container") $ "leave a comment"
            H.text "."

      comments <- sortComments =<< loadAllSnapshots (fromGlob $ "comments/" <> postSlug <> "/*") "comment"
      let ctx = postCtxWithTags tags <>
                constField "full_url" fullUrl <>
                constField "post_slug" postSlug <>
                constField "comment_count" (show $ length comments) <>
                listField "comments" defaultContext (return comments)

      contentCompiler alignment True
        >>= withItemBody (\x -> return (x <> pleaseComment))
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
        >>= relativizeUrls
        >>= removeIndexHtml

  -- raw posts
  match "posts/*" $ version "raw" $ do
    route   idRoute
    compile getResourceBody

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

    props' = if head url `elem` localUrlStartChars
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

sortComments :: MonadMetadata m => [Item a] -> m [Item a]
sortComments = sortByM $ flip getMetadataField' "date" . itemIdentifier
  where
    sortByM f xs = (map fst . sortBy (comparing snd)) <$> mapM (\x -> (x,) <$> f x) xs
