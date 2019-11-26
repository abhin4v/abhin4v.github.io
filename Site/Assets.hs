{-# LANGUAGE OverloadedStrings #-}
module Site.Assets where

import Control.Monad (forM_)
import qualified Data.ByteString.Lazy.Char8 as C
import Hakyll
import Hakyll.Web.Sass
import Site.Util
import Site.Pandoc
import Site.Pandoc.Compiler
import Text.Jasmine
import Text.Pandoc.Walk (walkM)

assets :: Rules ()
assets = do
  -- static files
  forM_ [ "CNAME"
        , "robots.txt"
        , "staticman.yml"
        , "README.md"
        , "images/**"
        , "files/**"
        , "fonts/**"
        , "js/*.min.js"
        ] $ flip match $ route idRoute >> compile copyFileCompiler

  -- 404
  match "404.html" $ do
    route idRoute
    compile $ getResourceBody
      >>= applyAsTemplate siteContext
      >>= loadAndApplyTemplate "templates/default.html" siteContext

  -- code
  match "code/**.md" $ do
    route   $ setExtension "html"
    compile $ contentCompiler (unsafeCompiler . walkM includeCodeTransform)
      >>= loadAndApplyTemplate "templates/code.html" siteContext

  -- js
  match ("js/*.js" .&&. complement "js/*.min.js") $ do
   route   idRoute
   compile compressJsCompiler

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
