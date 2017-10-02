{-# LANGUAGE OverloadedStrings #-}
module Site.Assets where

import Hakyll
import Hakyll.Web.Sass

assets :: Rules ()
assets = do
  -- static files
  match (fromList ["CNAME", "robots.txt", "staticman.yml"]) $ do
    route   idRoute
    compile copyFileCompiler

  -- 404
  match "404.html" $ do
    route idRoute
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext

  -- images and js
  match ("images/*" .||. "js/*") $ do
    route   idRoute
    compile copyFileCompiler

  -- css files
  match "css/*.css" $ do
    route   idRoute
    compile compressCssCompiler

  -- scss files
  scssDependencies <- makePatternDependency "css/_*.scss"
  rulesExtraDependencies [scssDependencies] $
    match "css/default.scss" $ do
      route $ setExtension "css"
      compile (fmap compressCss <$> sassCompiler)
