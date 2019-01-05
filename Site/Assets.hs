{-# LANGUAGE OverloadedStrings #-}
module Site.Assets where

import qualified Data.ByteString.Lazy.Char8 as C
import Hakyll
import Hakyll.Web.Sass
import Site.Util
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
    compile $ getResourceBody
      >>= applyAsTemplate siteContext
      >>= loadAndApplyTemplate "templates/default.html" siteContext

  -- images
  match "images/**" $ do
    route   idRoute
    compile copyFileCompiler

  -- files
  match "files/**" $ do
    route   idRoute
    compile copyFileCompiler

  -- fonts
  match "fonts/**" $ do
    route   idRoute
    compile copyFileCompiler

  -- js
  match ("js/*.js" .&&. complement "js/*.min.js") $ do
   route   idRoute
   compile compressJsCompiler

  -- .min.js
  match "js/*.min.js" $ do
    route   idRoute
    compile copyFileCompiler

  -- css files
  match "css/*.css" $ do
    route   idRoute
    compile compressCssCompiler

  -- scss files
  scssDependencies <- makePatternDependency "css/_*.scss"
  rulesExtraDependencies [scssDependencies] $
    match (fromRegex "css/[^_]+.scss") $ do
      route $ setExtension "css"
      compile (fmap compressCss <$> sassCompiler)

compressJsCompiler :: Compiler (Item String)
compressJsCompiler = do
  let minifyJS = C.unpack . minify . C.pack . itemBody
  s <- getResourceString
  return $ itemSetBody (minifyJS s) s
