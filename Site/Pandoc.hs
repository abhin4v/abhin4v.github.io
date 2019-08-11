module Site.Pandoc where

import Data.Char (isSpace)
import Data.List ((\\))
import Text.Pandoc.Definition
import qualified Data.Text as Text
import qualified Text.Pandoc.Filter.EmphasizeCode.Parser as EC
import qualified Text.Pandoc.Filter.EmphasizeCode.Chunking as EC
import qualified Text.Pandoc.Filter.EmphasizeCode.Range as EC
import qualified Text.Pandoc.Filter.EmphasizeCode.Renderable as EC
import qualified Text.Pandoc.Filter.EmphasizeCode.Html as EC

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
linkImages img@(Image attrs@(_, cls, _) elems (url, title)) =
  if "nolink" `elem` cls
  then img
  else Link ("", ["img-link"], []) [Image attrs [] (url, title), Span nullAttr elems] (url, title)
linkImages i = i

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

-- | A Pandoc filter that emphasizes code blocks.
emphasizeCode :: Block -> Block
emphasizeCode cb@(CodeBlock (id', classes, attrs) contents) =
  case lookupRanges attrs >>= (EC.runParser . EC.parseRanges) of
    Just (Right ranges) ->
      EC.renderEmphasized (EC.Html EC.Mark)
        (id', classes, filter (\(k, _) -> k /= "emphasize") attrs)
        $ EC.emphasizeRanges (EC.splitRanges ranges) (Text.pack contents)
    _ -> cb
  where
    lookupRanges attrs = Text.pack <$> lookup "emphasize" attrs
emphasizeCode x = x
