{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Site.Posts where

import Hakyll hiding (relativizeUrls)
import Site.Comments
import Site.PostCompiler
import Site.Util
import System.FilePath.Posix (splitExtension)

posts :: Tags -> String -> Rules ()
posts tags env = do
  -- post comments
  comments

  -- posts
  match "posts/*.md" $ do
    route indexHTMLRoute
    compilePosts tags env =<< getMatches "posts/*.md"

  -- raw posts
  match "posts/*.md" $ version "raw" $ do
    route   idRoute
    compile getResourceBody

  -- redirects for drafts
  match "posts/*.md" $ version "draft-redirects" $ do
    route (indexHTMLRoute `composeRoutes` gsubRoute "posts" (const "drafts"))
    compile $ do
      (path, _) <- splitExtension <$> getResourceFilePath
      makeItem $ Redirect ("/" ++ path ++ "/")

drafts :: Tags -> String -> Rules ()
drafts tags env =
  match "drafts/*.md" $ do
    route indexHTMLRoute
    compileDrafts tags env =<< getMatches "drafts/*.md"