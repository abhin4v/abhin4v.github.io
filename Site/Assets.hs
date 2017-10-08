{-# LANGUAGE OverloadedStrings #-}
module Site.Assets where

import qualified Data.ByteString.Lazy.Char8 as C
import Hakyll
import Hakyll.Web.Sass
import Text.Jasmine

assets :: Rules ()
assets = do
  -- static files
  match (fromList ["CNAME", "robots.txt", "staticman.yml", "README.md"]) $ do
    route   idRoute
    compile copyFileCompiler

  -- 404
  match "404.html" $ do
    route idRoute
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext

  -- images
  match "images/**" $ do
    route   idRoute
    compile copyFileCompiler

  -- js
  match "js/*" $ do
   route   idRoute
   compile compressJsCompiler

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

compressJsCompiler :: Compiler (Item String)
compressJsCompiler = do
  let minifyJS = C.unpack . minify . C.pack . itemBody
  s <- getResourceString
  return $ itemSetBody (minifyJS s) s
