{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Site.Sitemap
  ( SitemapConfiguration(..)
  , ChangeFrequency(..)
  , generateSitemap
  , defaultSitemapConfig
  ) where
import           Control.Applicative ((<$>))
import           Data.Char           (toLower)
import           Data.Maybe          (catMaybes)
import           Data.List           (isInfixOf, isPrefixOf)
import           Data.Time
import           Hakyll
import           System.Directory    (getModificationTime)
import           System.FilePath     (takeExtensions)
import           System.IO.Error
import           Text.XML.Light

data SitemapConfiguration = SitemapConfiguration {
    sitemapExtensions :: [String]
  , sitemapChangeFreq :: FilePath -> ChangeFrequency
  , sitemapPriority   :: FilePath -> Double
  , sitemapBase       :: String
  , sitemapRewriter   :: FilePath -> FilePath
}

data ChangeFrequency = Always
                     | Hourly
                     | Daily
                     | Weekly
                     | Monthly
                     | Yearly
                     | Never
                     deriving Show

defaultSitemapConfig :: SitemapConfiguration
defaultSitemapConfig = SitemapConfiguration {
    sitemapExtensions = [".html"]
  , sitemapChangeFreq = const Weekly
  , sitemapPriority   = const 0.5
  , sitemapBase       = "http://example.com/"
  , sitemapRewriter   = ('/':)
}

type SitemapRecord = (FilePath, String)

showFreq :: ChangeFrequency -> String
showFreq = map toLower . show

generateSitemap :: SitemapConfiguration -> Compiler (Item String)
generateSitemap config = do
    ids <- getMatches "**"
    urls <- catMaybes <$> mapM routeWithMod ids
    let filteredUrls = flip filter urls $ \u -> all ($ u) preds
    let urlset = xmlUrlSet config filteredUrls
    makeItem $ ppcTopElement prettyConfigPP urlset
    where
        extPred (p,_)          = takeExtensions p `elem` sitemapExtensions config
        draftPred (p,_)        = not ("drafts" `isInfixOf` p)
        notFoundPagePred (p,_) = not ("404" `isInfixOf` p)
        shortURLPred (p,_)     = not ("s/" `isPrefixOf` p)
        preds = [notFoundPagePred, draftPred, extPred, shortURLPred]

        routeWithMod i = do
            mtime <- itemModTime i
            rt <- getRoute i
            return $ fmap (,mtime) rt

itemModTime :: Identifier -> Compiler String
itemModTime i = do
    let path = toFilePath i
    mTimeUtc <- unsafeCompiler $ modTimeOrCurrent path
    return $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" mTimeUtc
    where
        modTimeOrCurrent path =
            catchIOError
            (getModificationTime path)
            (const getCurrentTime)

elementString :: String -> String -> Element
elementString name content = Element {
    elName    = unqual name
  , elAttribs = []
  , elContent = [Text (CData CDataText content Nothing)]
  , elLine    = Nothing
}

element :: String -> [Element] -> Element
element name content = Element {
    elName    = unqual name
  , elAttribs = []
  , elContent = map Elem content
  , elLine    = Nothing
}

xmlUrlSet :: SitemapConfiguration -> [SitemapRecord] -> Element
xmlUrlSet config = add_attr xmlns . element "urlset" . map (xmlUrl config)
    where xmlns = Attr (unqual "xmlns") "http://www.sitemaps.org/schemas/sitemap/0.9"

xmlUrl :: SitemapConfiguration -> SitemapRecord -> Element
xmlUrl conf r = element "url" [f conf r | f <- sub]
    where sub = [xmlLoc, xmlLastMod, xmlChangeFreq, xmlPriority]

xmlLoc, xmlLastMod, xmlChangeFreq, xmlPriority
    :: SitemapConfiguration
    -> SitemapRecord
    -> Element
xmlLastMod _ (_, m) = elementString "lastmod" m
xmlLoc config (r, _) = elementString "loc" loc
    where loc = sitemapBase config ++ rew
          rew = case sitemapRewriter config r of
                  ('.':xs) -> xs
                  xs       -> '/':xs
xmlChangeFreq config (r, _) = elementString "changefreq" freq
    where freq = showFreq $ sitemapChangeFreq config r
xmlPriority config (r, _) = elementString "priority" p
    where p = show $ sitemapPriority config r
