module Site.Pandoc.Compiler where

import Hakyll
import Site.Pandoc
import Site.TOC
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Extensions (disableExtension)
import Text.Pandoc.Options
import Text.Pandoc.Walk (walkM, walk)

noHTMLreaderOptions :: ReaderOptions
noHTMLreaderOptions = defaultHakyllReaderOptions {
    readerExtensions = disableExtension Ext_raw_html (readerExtensions defaultHakyllReaderOptions)
  }

writerOptions :: WriterOptions
writerOptions = defaultHakyllWriterOptions { writerEmailObfuscation = ReferenceObfuscation }

readContentWithPandocWith :: ReaderOptions -> Compiler (Item Pandoc)
readContentWithPandocWith readerOptions = getResourceBody >>= readPandocWith readerOptions

readContentWithPandoc :: Compiler (Item Pandoc)
readContentWithPandoc = readContentWithPandocWith defaultHakyllReaderOptions

pandocItemCompiler :: (Pandoc -> Compiler Pandoc) -> Item Pandoc -> Compiler (Item String)
pandocItemCompiler transform item =
  transform (itemBody item)
  >>= makeItem
  >>= return . writePandocWith writerOptions

contentCompiler :: (Pandoc -> Compiler Pandoc) -> Compiler (Item String)
contentCompiler transform = readContentWithPandoc >>= pandocItemCompiler transform

postContentTransforms :: String -> String -> Pandoc -> Compiler Pandoc
postContentTransforms postSlug alignment =
  unsafeCompiler
  . walkM includeCodeTransform
  . pureTransforms
  where
    pureTransforms =
      walk linkHeaders
      . walk (addHeaderTracking postSlug)
      . walk linkImages
      . walk mkScrollableTables
      . tableOfContents alignment
      . walk blankTargetLinks
      . walk expandWikiLinks
      . walk emphasizeCode
