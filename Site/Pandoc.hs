module Site.Pandoc where

import Data.Char (isSpace)
import Data.List ((\\))
import Text.Pandoc.Definition

blankTargetLinks :: Inline -> Inline
blankTargetLinks (Link (ident, classes, props) children (url, title)) =
  Link (ident, classes, props') children (url, title)
  where
    localUrlStartChars :: String
    localUrlStartChars = "/#.?"

    props' = if head url `elem` localUrlStartChars
      then props
      else props <> [("target", "_blank"), ("rel", "noopener")]
blankTargetLinks x = x

addHeaderTracking :: String -> Block -> Block
addHeaderTracking postSlug (Header level attr@(ident, cls, kvs) content) =
  if level == 2
  then Header level (ident, cls, kvs <> trackingAttrs) content
  else Header level attr content
  where
    trackingAttrs = [ ("data-track-content", "")
                    , ("data-content-name", ident)
                    , ("data-content-piece", postSlug)]
addHeaderTracking _ x = x

linkHeaders :: Block -> Block
linkHeaders (Header level attr@(ident, _, _) content) =
  Header level attr $ content <>
                      [ Link ("", ["ref-link"], []) [] ("#" <> ident, "")
                      , Link ("", ["top-link"], []) [] ("#top", "Back to top")
                      ]
linkHeaders x = x

linkImages :: Inline -> Inline
linkImages (Image (_, _, _) elems (url, _)) =
  Link ("", ["img-link"], []) [Image ("",[],[]) [] (url, ""), Span nullAttr elems] (url, "")
linkImages x = x

mkScrollableTables :: Block -> Block
mkScrollableTables table@Table{} = Div ("", ["scrollable-table"], []) [table]
mkScrollableTables x             = x

expandWikiLinks :: Inline -> Inline
expandWikiLinks el = case el of
  Link (aId, aClasses, aAttrs) children (targetURL, title)
    | "w" `elem` aClasses -> Link (aId, aClasses \\ ["w"], aAttrs) children (urlBase ++ targetURL, title)
  Span (aId, aClasses, aAttrs) children
    | "w" `elem` aClasses -> Link (aId, aClasses \\ ["w"], aAttrs) children (urlBase ++ replaceSpace (inlineToString el), "")
  Code (aId, aClasses, aAttrs) s
    | "w" `elem` aClasses -> Link (aId, aClasses \\ ["w"], aAttrs) [Str s] (urlBase ++ replaceSpace s, "")
  _ -> el
  where
    urlBase = "https://en.wikipedia.org/wiki/"

    replaceSpace = map (\x -> if isSpace x then '_' else x)

    inlineToString (Str s) = s
    inlineToString (Emph ils) = concatMap inlineToString ils
    inlineToString (Strong ils) = concatMap inlineToString ils
    inlineToString (Strikeout ils) = concatMap inlineToString ils
    inlineToString (Superscript ils) = concatMap inlineToString ils
    inlineToString (Subscript ils) = concatMap inlineToString ils
    inlineToString (SmallCaps ils) = concatMap inlineToString ils
    inlineToString (Quoted _ ils) = concatMap inlineToString ils
    inlineToString (Cite _ ils) = concatMap inlineToString ils
    inlineToString (Code _ s) = s
    inlineToString (Math _ s) = s
    inlineToString (RawInline _ s) = s
    inlineToString (Link _ ils _) = concatMap inlineToString ils
    inlineToString (Image _ ils _) = concatMap inlineToString ils
    inlineToString (Span _ ils) = concatMap inlineToString ils
    inlineToString Space = " "
    inlineToString SoftBreak = " "
    inlineToString LineBreak = " "
    inlineToString _ = ""