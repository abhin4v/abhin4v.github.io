module Site.ShortURLs where

import Control.Monad (forM_)
import Hakyll hiding (relativizeUrls)
import Site.Util

urlMappings :: [(String, String)]
urlMappings = [
    ("ccy", "/talks/clojure-concurrency-you/")
  , ("fpp", "/talks/fp-patterns/")
  ]

shortURLs :: Rules ()
shortURLs =
  forM_ urlMappings $ \(su, lu) ->
    create [fromFilePath $ "s/" ++ su ++ ".html"] $ do
      route indexHTMLRoute
      compile $ makeItem $ Redirect lu