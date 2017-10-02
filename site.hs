{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Hakyll
import Site.Collections
import Site.ERT
import Site.Posts
import Site.Assets
import Site.Sitemap
import Site.TOC
import Site.Util

siteRoot :: String
siteRoot = "https://abhinavsarkar.net"

main :: IO ()
main = hakyll $ do
  tags <- buildTags "posts/*" (fromCapture "tags/*.html")

  assets
  posts siteRoot tags
  collections siteRoot tags

  -- templates
  match "templates/*" $ compile templateBodyCompiler
