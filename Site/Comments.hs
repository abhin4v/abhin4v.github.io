{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Site.Comments where

import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale, parseTimeM, iso8601DateFormat)
import Hakyll
import Site.PostCompiler

comments :: Rules ()
comments =
  match "comments/*/*.md" $
    compile $ do
      ident           <- getUnderlying
      tss             <- getMetadataField' ident "date"
      date :: UTCTime <- parseTimeM False defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S%QZ") tss
      email           <- getMetadataField' ident "email"
      commentID       <- getMetadataField' ident "_id"
      let dateS       = formatTime defaultTimeLocale "%B %e, %Y" date

      getResourceBody
        >>= renderPandocWith readerOptions writerOptions
        >>= loadAndApplyTemplate "templates/comment.html" (commentCtx commentID dateS tss email)
        >>= saveSnapshot "comment"
