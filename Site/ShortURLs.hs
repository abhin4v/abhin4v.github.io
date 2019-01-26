module Site.ShortURLs where

import Control.Monad (forM_)
import Hakyll hiding (relativizeUrls)
import Site.Util

urlMappings :: [(String, String)]
urlMappings = [
    ("s/ccy", "/talks/clojure-concurrency-you/")
  ]

shortURLs :: Rules ()
shortURLs =
  forM_ urlMappings $ \(su, lu) ->
    create [fromFilePath $ su ++ ".html"] $ do
      route indexHTMLRoute
      compile $ makeItem $ Redirect lu