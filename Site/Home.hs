{-# LANGUAGE OverloadedStrings #-}
module Site.Home where

import Hakyll hiding (relativizeUrls)
import Site.Photos (createThumbnails, photoFields)
import Site.PostCompiler
import Site.Util

home :: Tags -> String -> Rules ()
home tags env = do
  imageDependencies <- makePatternDependency "photos/images/*.jpg"
  rulesExtraDependencies [imageDependencies] $
    match "index.html" $ do
      route idRoute
      compile $ do
        let indexPostCount = 3
        allPosts <- loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content"
        posts <- take indexPostCount <$> recentFirst allPosts
        let morePostCount = length allPosts - indexPostCount
            morePostCountW = numToWord morePostCount
            morePosts = morePostCountW <> " more post" <> (if morePostCount == 1 then "" else "s")

        thumbCols <- unsafeCompiler
                     $ fmap (map (take 3))
                     $ createThumbnails "photos/images" "photos/thumbs"

        let indexCtx =
              listField "posts" postCtx (return posts) <>
              constField "title" "Home"                <>
              constField "page_type" "website"         <>
              constField "more_posts" morePosts        <>
              listField "photoCols"
                    (listFieldWith "photos" photoFields (mapM makeItem . itemBody))
                    (mapM makeItem thumbCols)          <>
              siteContext

        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls env
          >>= removeIndexHtml
  where
    postCtx = teaserField "teaser" "content" <>
      postCtxWithTags tags <>
      field "post_ert" (fmap itemBody . flip loadSnapshot "ert" . itemIdentifier) <>
      field "comment_count" (fmap itemBody . flip loadSnapshot "comment_count" . itemIdentifier)
