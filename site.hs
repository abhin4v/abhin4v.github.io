{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe (fromMaybe)
import Hakyll
import Site.Activities
import Site.Assets
import Site.Collections
import Site.Pages
import Site.Posts
import Site.Readings
import System.Environment (lookupEnv)

main :: IO ()
main = do
  env <- fromMaybe "DEV" <$> lookupEnv "ENV"
  hakyll $ do
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")
    draftTags <- buildTags "drafts/*" (fromCapture "tags/*.html")

    assets
    pages env
    posts tags env
    drafts draftTags env
    collections tags env
    activities env
    readings env

    -- templates
    match "templates/*" $ compile templateBodyCompiler
