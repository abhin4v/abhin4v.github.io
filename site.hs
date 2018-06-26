{-# LANGUAGE OverloadedStrings #-}
module Main where

import Hakyll
import Site.Activities
import Site.Assets
import Site.Collections
import Site.Pages
import Site.Posts
import Site.Readings

main :: IO ()
main = hakyll $ do
  tags <- buildTags "posts/*" (fromCapture "tags/*.html")
  draftTags <- buildTags "drafts/*" (fromCapture "tags/*.html")

  assets
  pages
  posts tags
  drafts draftTags
  collections tags
  activities
  readings

  -- templates
  match "templates/*" $ compile templateBodyCompiler
