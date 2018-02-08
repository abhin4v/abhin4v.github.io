{-# LANGUAGE LambdaCase, ScopedTypeVariables, RecordWildCards, OverloadedStrings #-}
module Site.Readings (readings) where

import Control.Exception (try, SomeException)
import Data.Functor.Identity (runIdentity)
import Data.Maybe (fromJust, isJust, fromMaybe)
import Data.Monoid ((<>))
import Data.List (find, isInfixOf, sortBy)
import Data.Ord (comparing)
import qualified Data.Text as T
import Data.Time (LocalTime, parseTimeM, defaultTimeLocale, rfc822DateFormat, formatTime)
import Hakyll
import Network.HTTP.Simple (httpLBS, getResponseBody, parseRequest)
import Site.Util
import Text.RSS.Syntax (RSS(..), RSSChannel(..), RSSItem(..))
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Types (Feed(..))
import Data.XML.Types (Element(..), Name(..), elementText)

data Shelf = Read | ToRead | OnHold | Reading deriving (Show, Eq)

data Book = Book { bookName          :: String
                 , bookURL           :: String
                 , bookImageURL      :: String
                 , bookAuthor        :: String
                 , bookRating        :: Maybe Int
                 , bookReadDate      :: Maybe LocalTime
                 , bookAddedDate     :: LocalTime
                 , bookPublishedYear :: Maybe Int
                 , bookShelf         :: Shelf
                 } deriving (Show)

data Books = Books { booksRead    :: [Book]
                   , booksToRead  :: [Book]
                   , booksOnHold  :: [Book]
                   , booksReading :: [Book]
                   } deriving (Show)

noBooks :: Books
noBooks = Books [] [] [] []

getBooks :: String -> IO Books
getBooks feedURL =
  parseRequest feedURL >>= try . httpLBS >>= \case
    Left (_ :: SomeException) -> return noBooks
    Right resp -> case parseFeedSource $ getResponseBody resp of
      Nothing -> return noBooks
      Just (RSSFeed RSS { rssChannel = RSSChannel {..} }) ->
        let books        = map itemToBook rssItems
        in return Books { booksRead    = shelfBooks Read books
                        , booksToRead  = shelfBooks ToRead books
                        , booksOnHold  = shelfBooks OnHold books
                        , booksReading = shelfBooks Reading books
                        }
      _ -> error "Impossible"
  where
    shelfBooks shelf =
      sortBy (flip (comparing bookDate) <> comparing bookName)
      . filter ((== shelf) . bookShelf)

    bookDate Book {..} = fromMaybe bookAddedDate bookReadDate

itemToBook :: RSSItem -> Book
itemToBook RSSItem {..} =
  Book { bookName          = T.unpack $ fromJust rssItemTitle
       , bookURL           = "https://www.goodreads.com/book/show/" <> getBookProp "book_id"
       , bookImageURL      = getBookProp "book_small_image_url"
       , bookAuthor        = getBookProp "author_name"
       , bookRating        = getBookRating
       , bookReadDate      = getReadDate
       , bookAddedDate     = getAddedDate
       , bookPublishedYear = read <$> mGetBookProp "book_published"
       , bookShelf         = getBookShelf
       }
  where
    getBookProp :: T.Text -> String
    getBookProp prop =
      T.unpack
      . T.intercalate " "
      . elementText
      . fromJust
      . flip find rssItemOther
      $ \Element {elementName = Name{..}} -> nameLocalName == prop

    mGetBookProp prop = case getBookProp prop of
      "" -> Nothing
      x  -> Just x

    getBookRating = case getBookProp "user_rating" of
      "0" -> Nothing
      x   -> Just $ read x

    parseTime = runIdentity . parseTimeM True defaultTimeLocale rfc822DateFormat
    getReadDate = parseTime <$> mGetBookProp "user_read_at"
    getAddedDate = parseTime $ getBookProp "user_date_created"

    getBookShelf = case getBookProp "user_shelves" of
      x | "half-read" `isInfixOf` x         -> OnHold
      x | "currently-reading" `isInfixOf` x -> Reading
      x | "to-read" `isInfixOf` x           -> ToRead
      _                                     -> Read

readings :: Rules ()
readings = do
  anyDependency <- makePatternDependency "**"
  rulesExtraDependencies [anyDependency] $
    create ["readings.html"] $ do
      route indexHTMLRoute
      compile $ do
        Books {..} <- unsafeCompiler $ getBooks "https://www.goodreads.com/review/list_rss/24614151"

        let ctx = listField "books_read" bookFields (mapM makeItem booksRead) <>
                  listField "books_to_read" bookFields (mapM makeItem booksToRead) <>
                  listField "books_on_hold" bookFields (mapM makeItem booksOnHold) <>
                  listField "books_reading" bookFields (mapM makeItem booksReading) <>
                  constField "title" "Readings" <>
                  siteContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/readings.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls
          >>= removeIndexHtml
  where
    bookField name f = field name (return . f . itemBody)
    bookHasField name f = boolField name (isJust . f. itemBody)

    bookFields =
      mconcat [ bookField "name" bookName
              , bookField "url" bookURL
              , bookField "image_url" bookImageURL
              , bookField "author" bookAuthor
              , bookField "rating" (show . fromJust . bookRating)
              , bookHasField "has_rating" bookRating
              , bookField "published" (maybe "" show . bookPublishedYear)
              , bookHasField "has_published" bookPublishedYear
              , bookField "read_date" (maybe "" (formatTime defaultTimeLocale "%b, %Y") . bookReadDate)
              , bookHasField "has_read_date" bookReadDate
              , bookField "added_date" (formatTime defaultTimeLocale "%b, %Y" . bookAddedDate)
              ]
