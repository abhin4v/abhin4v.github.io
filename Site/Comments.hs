{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Site.Comments where

import qualified Data.Map as Map
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale, parseTimeM, iso8601DateFormat)
import Hakyll
import Site.Pandoc
import Site.Pandoc.Compiler
import Site.PostCompiler
import Text.Pandoc.Walk (walk)

comments :: Rules ()
comments = do
  replyToMap <- commentReplyToMap =<< getMatches "comments/*/*.md"

  match "comments/*/*.md" $
    compile $ do
      ident           <- getUnderlying
      tss             <- getMetadataField' ident "date"
      date :: UTCTime <- parseTimeM False defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S%QZ") tss
      email           <- getMetadataField' ident "email"
      commentID       <- getMetadataField' ident "_id"
      let dateS       = formatTime defaultTimeLocale "%B %e, %Y" date
          level       = calcLevel replyToMap ident

      pandocCompilerWithTransform noHTMLreaderOptions writerOptions (walk blankTargetLinks)
        >>= loadAndApplyTemplate "templates/comment.html" (commentCtx commentID dateS tss email level)
        >>= saveSnapshot "comment"

calcLevel :: Map.Map Identifier Identifier -> Identifier -> Int
calcLevel replyToMap comment = go comment 0
  where
    go c l = case Map.lookup c replyToMap of
      Nothing -> l
      Just pc -> go pc (l+1)