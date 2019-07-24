{-# LANGUAGE LambdaCase, ScopedTypeVariables, RecordWildCards, OverloadedStrings #-}
module Site.Notes where

import Control.Exception (try, SomeException)
import Control.Monad (forM, void)
import Data.List (sortOn)
import qualified Data.Text as T
import Data.Time (LocalTime, parseTimeM, defaultTimeLocale, iso8601DateFormat, formatTime)
import Hakyll hiding (relativizeUrls)
import Network.HTTP.Simple (httpLBS, getResponseBody, parseRequest)
import Site.Util
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Types (Feed(..))
import qualified Text.Atom.Feed as Atom

data Note = Note { noteName :: String
                 , noteLink :: String
                 , noteDate :: LocalTime
                 , noteTags :: [String]
                 }

getNotes :: String -> IO [Note]
getNotes sitemapURL =
  parseRequest sitemapURL >>= try . httpLBS >>=  \case
    Left (_ :: SomeException) -> return []
    Right resp -> case parseFeedSource $ getResponseBody resp of
      Nothing -> return []
      Just (AtomFeed Atom.Feed {..}) -> do
        let entries = reverse $ sortOn Atom.entryUpdated $ feedEntries
        forM entries $ \e -> do
          date <- parseTimeM True defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S+00:00")) $ T.unpack $ Atom.entryUpdated e
          let name = Atom.txtToString $ Atom.entryTitle e
              link = T.unpack $ Atom.linkHref $ head $ Atom.entryLinks e
              tags = map (T.unpack . Atom.catTerm) $ Atom.entryCategories e
          return $ Note name link date tags
      _ -> error "Impossible"

indexNotesCount :: Int
indexNotesCount = 5

notes :: String -> Rules ()
notes env = do
  anyDependency <- makePatternDependency "**"
  rulesExtraDependencies [anyDependency] $
    create ["notes.html"] $ do
      route indexHTMLRoute
      compile $ do
        notes' <- unsafeCompiler $ getNotes "https://notes.abhinavsarkar.net/feed.atom"

        let ctx = listField "notes" noteCtx (mapM makeItem notes') <>
                  constField "title" "Notes" <>
                  constField "page_type" "notes" <>
                  siteContext

        void $ saveLatestNotesSnapshot (take indexNotesCount notes')

        makeItem ""
          >>= loadAndApplyTemplate "templates/notes.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls env
          >>= removeIndexHtml
  where
    noteField name f = field name (return . f . itemBody)

    noteCtx = noteField "link" noteLink <>
              noteField "name" noteName <>
              noteField "date" (formatTime defaultTimeLocale "%b %e %Y" . noteDate) <>
              noteField "mdate" (formatTime defaultTimeLocale "%F" . noteDate) <>
              listFieldWith "tags" tagCtx (mapM makeItem . noteTags . itemBody)

    tagCtx = field "tag" (return . itemBody)

    saveLatestNotesSnapshot notes' = do
      let ctx = listField "notes" noteCtx (mapM makeItem notes') <>
                constField "title" "Recent Notes" <>
                siteContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/notes.html" ctx
        >>= relativizeUrls env
        >>= removeIndexHtml
        >>= saveSnapshot "latest"
