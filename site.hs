{-# LANGUAGE OverloadedStrings #-}
module Main where

import Hakyll
import Site.Assets
import Site.Collections
import Site.Pages
import Site.Posts
import Site.Activities

main :: IO ()
main = hakyll $ do
  tags <- buildTags "posts/*" (fromCapture "tags/*.html")

  assets
  pages
  posts tags
  collections tags
  activities

  -- templates
  match "templates/*" $ compile templateBodyCompiler
