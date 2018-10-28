{-# LANGUAGE RecordWildCards, OverloadedStrings, TupleSections #-}
module Site.Photos (photos) where

import Data.Char (toLower)
import Data.Function (on)
import Data.List (isPrefixOf, sort, groupBy, permutations, minimumBy, transpose, isInfixOf)
import Data.List.Split (splitOn, chunksOf)
import Data.Ord (comparing)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT, MonadResource)
import Data.Default (def)
import qualified Graphics.ThumbnailPlus as T
import Hakyll hiding (relativizeUrls)
import Site.Util
import System.Directory (copyFile, listDirectory, removeFile)
import System.FilePath (takeBaseName, takeExtension, (</>), (<.>))

thumbSizes :: [Int]
thumbSizes = [200, 300, 500, 800]

columnCount :: Int
columnCount = 3

type Thumbs = [FilePath]

createThumbnails :: FilePath -> FilePath -> IO [[Thumbs]]
createThumbnails photosDir thumbsDir = do
  thumbPaths <- listDirectory thumbsDir
  photoPaths <- sort
                . map (photosDir </>)
                . filter ((`elem` [".jpg", ".png"]) . takeExtension)
                <$> listDirectory photosDir
  let newPhotoPaths =
          filter (\f -> not $ any (takeBaseName f `isPrefixOf`) thumbPaths) photoPaths
      oldThumbPaths = map (thumbsDir </>)
          . filter (\f -> not $ any (takeWhile (/= '-') (takeBaseName f) `isInfixOf`) photoPaths) 
          $ thumbPaths

  runResourceT $ forM_ newPhotoPaths $ createPhotoThumbnails thumbsDir
  forM_ oldThumbPaths $ \f -> removeFile f >> putStrLn ("Deleted thumb: " ++ f)

  sortThumbs
    . zipWith (:) photoPaths
    . map (\ts -> take (length thumbSizes) $ ts ++ repeat (last ts))
    . groupBy ((==) `on` take 32 . takeBaseName)
    . sort
    . map (thumbsDir </>)
    . filter ((`elem` [".jpg", ".png"]) . takeExtension)
    <$> listDirectory thumbsDir

sortThumbs :: [Thumbs] -> [[Thumbs]]
sortThumbs = map (filter (not . null))
  . transpose
  . go (replicate columnCount 0)
  . chunksOf columnCount
  where
    go _ [] = []
    go heights (row:rows) =
      let (heights', row') = sortThumbRow heights row
      in row' : go heights' rows

    sortThumbRow heights = unzip
      . minimumBy (comparing (variance . map fst))
      . map (zipWith (\h t -> (h + thumbHeight t, t)) heights . (++ repeat []))
      . permutations

    thumbHeight []    = 0
    thumbHeight thumb =
      let [w,h] = splitOn "x" . drop 33 . takeBaseName . head . tail $ thumb
      in read h * fromIntegral (head thumbSizes) / read w

    variance [] = 0
    variance xs = let avg = sum xs / fromIntegral (length xs)
      in sum (map (\x -> sqr (x - avg)) xs)

    sqr x = x * x

createPhotoThumbnails :: MonadResource m => FilePath -> FilePath -> m ()
createPhotoThumbnails thumbsDir photoPath = do
  cthumbs <- T.createThumbnails config photoPath
  case cthumbs of
    T.CreatedThumbnails thumbs _ -> liftIO $ do
      forM_ thumbs $ \thumb -> do
        let thumbPath     = T.thumbFp thumb
            T.Size { .. } = T.thumbSize thumb
            thumbFormat   = map toLower . show . T.thumbFormat $ thumb
            thumbName     = takeBaseName photoPath ++ "-" ++ show width ++ "x" ++ show height
            newThumbPath  = thumbsDir </> thumbName <.> thumbFormat
        copyFile thumbPath newThumbPath
      putStrLn $ "Created " ++ show (length thumbs) ++ " thumbs for: " ++ photoPath
    e -> liftIO $ putStrLn $ "Error: " ++ show e
  where
    sizes = map (\s -> (T.Size s s, Nothing)) thumbSizes
    config = def { T.thumbnailSizes = sizes
                  , T.reencodeOriginal = T.Never
                  }

photos :: String -> Rules ()
photos env = do
  scssDependencies <- makePatternDependency "photos/images/*.jpg"
  rulesExtraDependencies [scssDependencies] $
    create ["photos.html"] $ do
      route indexHTMLRoute
      compile $ do
        thumbCols <- unsafeCompiler $ createThumbnails "photos/images" "photos/thumbs"
        let ctx = listField "photoCols"
                    (listFieldWith "photos" photoFields (mapM makeItem . itemBody))
                    (mapM makeItem thumbCols) <>
                  constField "title" "Photos" <>
                  constField "page_type" "photos" <>
                  siteContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/photos.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls env
          >>= removeIndexHtml
  where
    photoField name f = field name (return . f . itemBody)
    photoFields =
        mconcat [ photoField "orig" (!! 0)
                , photoField "small" (!! 1)
                , photoField "medium" (!! 2)
                , photoField "large" (!! 3)
                , photoField "xlarge" (!! 4)
                ]
