---
title: "No Matter How You Slice It with Haskell and Interval Trees"
date: 2018-12-25
description: Solving Advent of Code Puzzle 3 --- "No Matter How You Slice It" --- with Haskell and Interval Trees.
tags: haskell, aoc, aoc18, programming
author: Abhinav Sarkar
toc: right
---

Here's a condensed version of the third puzzle --- ["No Matter How You Slice It"] --- from [Advent of Code 2018]:

> The whole piece of fabric the Elfs are working on is a very large square. Each Elf has made a claim about which area of fabric would be ideal for Santa's suit. All claims have an ID and consist of a single rectangle with edges parallel to the edges of the fabric.
> 
> A claim like `#123 @ 3,2: 5x4` means that claim ID 123 specifies a rectangle 3 inches from the left edge, 2 inches from the top edge, 5 inches wide, and 4 inches tall. The problem is that many of the claims overlap, causing two or more claims to cover part of the same areas.
>
> _How many square inches of fabric are within two or more claims?_

Let's solve this with Haskell.

<!--more-->

* toc

## Grokking the Input

First, let's get the imports out of the way:

```haskell
{-# LANGUAGE Strict #-}
module Main where

import Control.Applicative (some)
import Data.Bits (Bits(shift))
import Data.Function (on)
import qualified Data.Set as Set
import qualified Data.Tree as T
import Data.List (maximumBy, foldl', sort, sortOn)
import Data.Ord (comparing)
import Text.Parsec hiding (Empty)
```

A claim is a rectangle with an ID. Let's define a type for it.

```haskell
data Claim = Claim { claimID     :: Int
                   , claimLeft   :: Int
                   , claimTop    :: Int
                   , claimWidth  :: Int
                   , claimHeight :: Int
                   }

instance Eq Claim where
  (==) = (==) `on` claimID

instance Ord Claim where
  compare = compare `on` claimID

instance Show Claim where
  show (Claim id l t w h) =
    "<#" ++ show id ++ " "
    ++ "(" ++ show l ++ "," ++ show t ++ ")-"
    ++ "(" ++ show (l+w) ++ "," ++ show (t+h) ++ ")>"
```

Since each claim is uniquely identified with an ID, we use the ID to equate and order claims. The `Show` instance of `Claim` shows the top-left and bottom-right coordinates of a claim along with the ID, as demonstrated by this session in GHCi:

```haskell
*Main> claim = Claim 1 3 4 12 32
*Main> print claim
<#1 (3,4)-(15,36)>
```

The [input][1] for the puzzle is one claim per line. Let's write a quick parser to parse an input line to a `Claim` value. We use the [parsec][^parsec] parser combinator library for the same.

```haskell
claimParser :: Parsec String () Claim
claimParser =
  (\id (l,t) (w,h) -> Claim id l t w h)
  <$> (idP <* spaces <* char '@' <* spaces)
  <*> (posP <* char ':' <* spaces)
  <*> dimP
  where
    intP = read <$> some digit
    idP  = char '#' *> intP
    posP = (,) <$> (intP <* char ',') <*> intP
    dimP = (,) <$> (intP <* char 'x') <*> intP
```

Let's see it in action in the REPL:

```haskell
*Main> parse claimParser "" "#123 @ 3,2: 5x4"
Right <#123 (3,2)-(8,6)>
```

Nice. Now, we can write the function to parse the whole puzzle input:

```haskell
readInput :: String -> [Claim]
readInput input = case traverse (parse claimParser "") $ lines input of
  Left e   -> error (show e)
  Right rs -> rs
```

Back to GHCi:

```haskell
*Main> input <- readFile "input"
*Main> mapM_ print . take 5 $ readInput input
<#1 (179,662)-(195,689)>
<#2 (609,961)-(634,982)>
<#3 (73,376)-(96,404)>
<#4 (599,405)-(625,430)>
<#5 (28,933)-(47,962)>
```

It works! Off to the actual solution now.

## Brute-force Solution

One simple way to solve this puzzle is to go through each 1x1 cell in the whole sheet and check if more than one claim contains them. This is a brute-force solution with no cleverness, but it will serve as a baseline for the later solutions. For this solution, first we need to find out the size of the sheet by going over all the claims:

```haskell
sheetSize :: [Claim] -> (Int, Int)
sheetSize claims = (calcBound claimRight, calcBound claimBottom)
  where
    claimRight  (Claim _ l _ w _) = l + w
    claimBottom (Claim _ _ t _ h) = t + h
    calcBound f = f (maximumBy (comparing f) claims)
```

We calculate the width and height of the sheet by finding the claims with furthest right and bottom boundaries. Let's run this over the input file in GHCi:

```haskell
*Main> input <- readFile "input"
*Main> claims = readInput input
*Main> sheetSize claims
(1000,1000)
```

So our sheet is a 1000x1000 square. Now, let's write a function to find out if a cell is contained in more than one claims.

```haskell
isOverlapCell :: [Claim] -> (Int, Int) -> Bool
isOverlapCell claims cell =
  (> 1) . length . filter (cellInClaim cell) $ claims
  where
    cellInClaim (x, y) (Claim _ l t w h) =
      l <= x && (l+w) >= (x+1) && t <= y && (t+h) >= (y+1)
```

`cellInClaim` function checks if a cell lies within a claim by comparing the cell and claim boundaries. Then, from all the claims we filter the ones which contain the given cell. If there is more than one such claim, the cell is an overlap cell.

With these functions done, writing the brute-force solver is easy:

```haskell
bruteForceOverlapArea :: [Claim] -> Int
bruteForceOverlapArea claims =
  let (width, height) = sheetSize claims
      cells           = [(i, j) | i <- [0..width-1], j <- [0..height-1]]
      overlapArea     = length . filter (isOverlapCell claims) $ cells
  in overlapArea
```

We go over all cell in the sheet and check if they are overlap cells. The overlap area is simply the count of such cells. Let's write the main function to finish this up:

```haskell
main :: IO ()
main = do
  claims <- readInput <$> getContents
  let overlapArea = bruteForceOverlapArea claims
  putStrLn $ "Overlap Area = " ++ show overlapArea
```

Now, we compile and run this on the puzzle input:

```plain
$ ghc -O2 --make *.hs -o overlap-area
Linking overlap-area ...
$ cat input | time ./overlap-area
Overlap Area = 109143
       14.35 real        14.30 user         0.03 sys
```

The answer _109143_ is correct; I've verified it with the _Advent of Code_ website. It takes about 14 seconds to brute-force solve the puzzle on my 2015 MacBook pro laptop.

Next, let's see how we can improve the run time with a special data structure.


[Advent of Code 2018]: https://adventofcode.com/2018/
["No Matter How You Slice It"]: https://adventofcode.com/2018/day/3
[parsec]: http://hackage.haskell.org/package/parsec

[1]: https://adventofcode.com/2018/day/3/input
[2]: http://book.realworldhaskell.org/read/using-parsec.html
[3]: http://kunigami.wordpress.com/2014/01/21/an-introduction-to-the-parsec-library

[^parsec]: You can learn more about parsec and parsing in general from [these][2] [tutorials][3].