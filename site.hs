{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import Hakyll
import Site.Activities
import Site.Assets
import Site.Collections
import Site.Home
import Site.Notes
import Site.Pages
import Site.Photos
import Site.Posts
import Site.Readings
import Site.ShortURLs
import Site.Talks
import System.Environment (lookupEnv)

main :: IO ()
main = do
  env <- fromMaybe "DEV" <$> lookupEnv "ENV"
  offline <- (== Just "1") <$> lookupEnv "OFFLINE"
  hakyll $ do
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")
    draftTags <- buildTags "drafts/*" (fromCapture "tags/*.html")

    assets
    pages env
    posts tags env offline
    drafts draftTags env
    home tags env offline
    collections tags env
    photos env
    talks env
    shortURLs
    unless offline $ do
      notes env
      activities env
      readings env

    -- templates
    match "templates/*" $ compile templateBodyCompiler
