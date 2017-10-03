{-# LANGUAGE OverloadedStrings #-}
module Main where

import Hakyll
import Site.Assets
import Site.Collections
import Site.Pages
import Site.Posts

siteRoot :: String
siteRoot = "https://abhinavsarkar.net"

main :: IO ()
main = hakyll $ do
  tags <- buildTags "posts/*" (fromCapture "tags/*.html")

  assets
  pages
  posts siteRoot tags
  collections siteRoot tags

  -- templates
  match "templates/*" $ compile templateBodyCompiler
