module Site.ERT (estimatedReadingTime) where

import Text.Pandoc.Definition
import Text.Pandoc.Options

estimatedReadingTime :: Bool -> Pandoc -> Pandoc
estimatedReadingTime enabled p@(Pandoc meta blocks) =
  if enabled
    then Pandoc meta (ert:blocks)
    else Pandoc meta blocks
  where ert = Div ("ert", ["ert"], []) [Plain [Str $ timeEstimateString p ++ " read"]]

timeEstimateString :: Pandoc -> String
timeEstimateString = toClockString . timeEstimateSeconds

toClockString :: Int -> String
toClockString i
    | i >= 60 * 60 = show hours   ++ " hour " ++ show minutes ++ " minute"
    | i >= 60      = show minutes ++ " minute"
    | otherwise    = show seconds ++ " seccond"
  where
    hours   = i `quot` (60 * 60)
    minutes = (i `rem` (60 * 60)) `quot` 60
    seconds = i `rem` 60

timeEstimateSeconds :: Pandoc -> Int
timeEstimateSeconds = (`quot` 4) . nrWords

nrWords :: Pandoc -> Int
nrWords = (`quot` 5) . nrLetters

nrLetters :: Pandoc -> Int
nrLetters (Pandoc _ bs) = sum $ map cb bs
  where
    cbs = sum . map cb
    cbss = sum . map cbs
    cbsss = sum . map cbss

    cb :: Block -> Int
    cb (Plain is) = cis is
    cb (Para is) = cis is
    cb (CodeBlock _ s) = length s
    cb (RawBlock _ s) = length s
    cb (BlockQuote bs) = cbs bs
    cb (OrderedList _ bss) = cbss bss
    cb (BulletList bss) = cbss bss
    cb (DefinitionList ls) = sum $ map (\(is, bss) -> cis is + cbss bss) ls
    cb (Header _ _ is) = cis is
    cb HorizontalRule = 0
    cb (Table is _ _ tc tcs) = cis is + cbss tc + cbsss tcs
    cb (Div _ bs) = cbs bs
    cb Null = 0

    cis = sum . map ci
    ciss = sum . map cis

    ci :: Inline -> Int
    ci (Str s) = length s
    ci (Emph is) = cis is
    ci (Strong is) = cis is
    ci (Strikeout is) = cis is
    ci (Superscript is) = cis is
    ci (Subscript is) = cis is
    ci (SmallCaps is) = cis is
    ci (Quoted _ is) = cis is
    ci (Cite _ is) = cis is
    ci (Code _ s) = length s
    ci Space = 1
    ci SoftBreak = 1
    ci LineBreak = 1
    ci (Math _ s) = length s
    ci (RawInline _ s) = length s
    ci (Link _ is (_, s)) = cis is + length s
    ci (Image _ is (_, s)) = cis is + length s
    ci (Note bs) = cbs bs
