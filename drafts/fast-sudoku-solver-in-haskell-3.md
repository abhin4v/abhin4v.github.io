---
title: "Fast Sudoku Solver in Haskell #3: Picking the Right Data Structures"
date: 2018-06-28
description: We make the Sudoku solution faster by using the right data structure
tags: haskell, sudoku, programming, puzzle
author: Abhinav Sarkar
toc: right
---

In the [previous part] of this series of posts, we optimized the Sudoku solver by implementing a new strategy to prune cells and were able to achieve a speedup of almost 200x. Afterwards, we profiled the solution and found that there were bottlenecks in the program leading to a slowdown. In this post, we are going to follow the profiler and use the right _Data Structures_ to improve the solution further and make it **faster**.

<!--more-->

This is the third post in a series of posts:

1. [Fast Sudoku Solver in Haskell #1: A Simple Solution]
2. [Fast Sudoku Solver in Haskell #2: A 200x Faster Solution]
3. [Fast Sudoku Solver in Haskell #3: Picking the Right Data Structures]

Discuss this post on [r/haskell].

* toc

## Quick Recap

[Sudoku] is a number placement puzzle. It consists of a 9x9 grid which is to be filled with digits from 1 to 9 such that each row, each column and each of the nine 3x3 sub-grids contain all the digits. Some of the cells of the grid come pre-filled and the player has to fill the rest.

In the previous post, we improved the performance of the simple Sudoku solver by implementing a new strategy to prune cells. This [new strategy][2] found the digits which occurred uniquely, in pairs, or in triplets and fixed the cells to those digits. It led to a speedup of about 200x over our original naive solution. This is our current run[^machinespec] time for solving all the 49151 [17-clue puzzles][1]:

```plain
$ cat sudoku17.txt | time stack exec sudoku > /dev/null
      258.97 real       257.34 user         1.52 sys
```

Let's try to improve this time.

## Profile Twice, Code Once

Instead of trying to guess how to improve the performance of our solution, let's be methodical about it. We start with profiling the code to find the bottlenecks. Let's compile and run the code with profiling flags.

```plain
$ stack build --profile
$ head -1000 sudoku17.txt | stack exec -- sudoku +RTS -p > /dev/null
```

This generates a `sudoku.prof` file with profiling output. Here are the top sever _Cost Centres_[^cc] from the file (cleaned for brevity):

Cost Centre                   Src                        %time  %alloc
-----------------             ---------                 ------ -------
`exclusivePossibilities`      Sudoku.hs:(49,1)-(62,26)   18.9   11.4
`pruneCellsByFixed.pruneCell` Sudoku.hs:(75,5)-(76,36)   17.7   30.8
`exclusivePossibilities.\.\`  Sudoku.hs:55:38-70         11.7   20.3
`fixM.\`                      Sudoku.hs:13:27-65         10.7    0.0
`==`                          Sudoku.hs:15:56-57          5.6    0.0
`pruneGrid'`                  Sudoku.hs:(103,1)-(106,64)  5.0    6.7
`pruneCellsByFixed`           Sudoku.hs:(71,1)-(76,36)    4.5    5.0
`exclusivePossibilities.\`    Sudoku.hs:58:36-68          3.4    2.5

_Cost Centre_ points to a function, either named or anonymous. _Src_ gives the line and column numbers of the source code of the function. _%time_ and _%alloc_ are the percentages of time spent and memory allocated in the function, respectively.

We see that `exclusivePossibilities` and the nested functions inside it take up almost 34% time of the entire run time. Seconds biggest bottleneck is the `pruneCell` function inside the `pruneCellsByFixed` function.

It is easy to guess the possible reason for `pruneCell` taking so much time. Here's the code for reference:

```haskell
pruneCellsByFixed :: [Cell] -> Maybe [Cell]
pruneCellsByFixed cells = traverse pruneCell cells
  where
    fixeds = [x | Fixed x <- cells]

    pruneCell (Possible xs) = makeCell (xs Data.List.\\ fixeds)
    pruneCell x             = Just x
```

`pruneCell` uses `Data.List.\\` to find the difference of the cell's possible digits and the fixed digits in the cell's block. In Haskell, lists are implemented as [singly linked lists]. So, finding the difference or intersection of two lists is O(n^2^), that is, quadratic [asymptotic complexity]. Let's tackle this bottleneck first.

## A Set for All Occasions

What is a efficient data structure for finding differences and intersections? Why, a [_Set_] of course! A Set stores unique values and provides fast operations for testing membership of its elements. If we use a Set to represent the possible values of cells instead of a List, the program should run faster. Since the possible values are already unique (`1` to `9`), it should not break anything.

Haskell comes with a bunch of Set implementations:

- [`Data.Set`] which is a generic data structure implemented as [self-balancing binary search tree].
- [`Data.HashSet`] which is a generic data structure implemented as [hash array mapped trie].
- [`Data.IntSet`] which is a specialized data structure for integer values, implemented as [radix tree].

However, a much faster implementation is possible for our particular use-case. We can use a [_BitSet_]. 

A BitSet uses [bits] to represent unique members of a Set. First, we map values to particular bits using some function. If the bit corresponding to a particular value is set to `1` then the value is present in the Set, else not. So, we need as many bits in a BitSet as the number of values in our domain, which makes is difficult to use for generic problems. But, for our Sudoku solver, we need to store just the digits `1` to `9` in our Set, which make BitSet very suitable for us. Also, the Set operations on BitSet are implemented using bit-level instructions in hardware, making them much faster than those on the other data structure listed above.

In Haskell, we can use the [`Data.Word`] module to represent a BitSet. Specifically, we can use [`Data.Word.Word16`] type which has sixteen bits because we need only nine bits to represent the nine digits. The bit-level operations on `Word16` are provided by the [`Data.Bits`] module.

## Bit by Bit, We Get Faster

First, we replace List with `Word16` in the `Cell` type and add a helper function:

```haskell
data Cell = Fixed Data.Word.Word16
          | Possible Data.Word.Word16
          deriving (Show, Eq)

setBits :: Data.Word.Word16 -> [Data.Word.Word16] -> Data.Word.Word16
setBits = Data.List.foldl' (Data.Bits..|.)
```

Then we replace `Int` related operations with bit related ones in the read and show functions.

```haskell
readGrid :: String -> Maybe Grid
readGrid s
  | length s == 81 = 
      traverse (traverse readCell) . Data.List.Split.chunksOf 9 $ s
  | otherwise      = Nothing
  where
    allBitsSet = 1022

    readCell '.' = Just $ Possible allBitsSet
    readCell c
      | Data.Char.isDigit c && c > '0' = 
          Just . Fixed . Data.Bits.bit . Data.Char.digitToInt $ c
      | otherwise = Nothing

showGrid :: Grid -> String
showGrid = unlines . map (unwords . map showCell)
  where
    showCell (Fixed x) = show . Data.Bits.countTrailingZeros $ x
    showCell _         = "."

showGridWithPossibilities :: Grid -> String
showGridWithPossibilities = unlines . map (unwords . map showCell)
  where
    showCell (Fixed x)     = (show . Data.Bits.countTrailingZeros $ x) ++ "          "
    showCell (Possible xs) =
      "[" ++ 
      map (\i -> if Data.Bits.testBit xs i then Data.Char.intToDigit i else ' ') [1..9] 
      ++ "]"
```

We set the same bit as the digit to indicate the presence of the digit in the possibilities. For example, for digit `1`, we set the bit 1 so that the resulting `Word16` is `0000 0000 0000 0010` or 2. So for fixed cells, the value is count of the zeros from right.

The change in the `exclusivePossibilities` function is pretty minimal:

```diff
-exclusivePossibilities :: [Cell] -> [[Int]]
+exclusivePossibilities :: [Cell] -> [Data.Word.Word16]
 exclusivePossibilities row =
   row
   & zip [1..9]
   & filter (isPossible . snd)
   & Data.List.foldl'
       (\acc ~(i, Possible xs) ->
-        Data.List.foldl' 
-          (\acc' x -> Map.insertWith prepend x [i] acc') 
-          acc 
-          xs)
+        Data.List.foldl'
+          (\acc' x -> if Data.Bits.testBit xs x
+                      then Map.insertWith prepend x [i] acc'
+                      else acc')
+          acc
+          [1..9])
       Map.empty
   & Map.filter ((< 4) . length)
   & Map.foldlWithKey'(\acc x is -> Map.insertWith prepend is [x] acc) Map.empty
   & Map.filterWithKey (\is xs -> length is == length xs)
   & Map.elems
+  & map (Data.List.foldl' Data.Bits.setBit Data.Bits.zeroBits)
   where
     prepend ~[y] ys = y:ys
```

In the nested folding step, instead of folding over the possible values of cells, now we fold over the digits from `1` to `9` and insert the entry in the map if the bit corresponding to the digit is set in the possibilities. And as the last step, we convert the exclusive possibilities to `Word16` by folding them, starting with zero. As example in the _REPL_ should be instructive:

```haskell
*Main> poss = Data.List.foldl' Data.Bits.setBit Data.Bits.zeroBits
*Main> row = [Possible $ poss [4,6,9], Fixed $ poss [1], Fixed $ poss [5], Possible $ poss [6,9], Fixed $ poss [7], Possible $ poss [2,3,6,8,9], Possible $ poss [6,9], Possible $ poss [2,3,6,8,9], Possible $ poss [2,3,6,8,9]]
*Main> putStr $ showGridWithPossibilities [row]
[   4 6  9] 1           5           [     6  9] 7           [ 23  6 89] [     6  9] [ 23  6 89] [ 23  6 89]
*Main> exclusivePossibilities row
[16,268]
*Main> [poss [4], poss [8,3,2]]
[16,268]
```

This is the same example row as the [last time][2]. And it returns same results, excepts as a list of `Word16` now.

We change `makeCell` to use bit operations instead of list ones:

```haskell
makeCell :: Data.Word.Word16 -> Maybe Cell
makeCell ys
  | ys == Data.Bits.zeroBits   = Nothing
  | Data.Bits.popCount ys == 1 = Just $ Fixed ys
  | otherwise                  = Just $ Possible ys
```

And we change cell pruning functions also:

```diff
 pruneCellsByFixed :: [Cell] -> Maybe [Cell]
 pruneCellsByFixed cells = traverse pruneCell cells
   where
-    fixeds = [x | Fixed x <- cells]
+    fixeds = setBits Data.Bits.zeroBits [x | Fixed x <- cells]

-    pruneCell (Possible xs) = makeCell (xs Data.List.\\ fixeds)
+    pruneCell (Possible xs) = makeCell (xs Data.Bits..&. Data.Bits.complement fixeds)
     pruneCell x             = Just x

 pruneCellsByExclusives :: [Cell] -> Maybe [Cell]
 pruneCellsByExclusives cells = case exclusives of
   [] -> Just cells
   _  -> traverse pruneCell cells
   where
     exclusives    = exclusivePossibilities cells
-    allExclusives = concat exclusives
+    allExclusives = setBits Data.Bits.zeroBits exclusives

     pruneCell cell@(Fixed _) = Just cell
     pruneCell cell@(Possible xs)
       | intersection `elem` exclusives = makeCell intersection
       | otherwise                      = Just cell
       where
-        intersection = xs `Data.List.intersect` allExclusives
+        intersection = xs Data.Bits..&. allExclusives
```

Notice how the list difference and intersection functions are replaced by `Data.Bits` functions. Specifically, list difference is replace by bitwise-and of the bitwise-complement and list intersection is replaced by bitwise-and.

We make a one-line change in the `isGridInvalid` function to find empty possible cells using bit ops:

```diff
 isGridInvalid :: Grid -> Bool
 isGridInvalid grid =
   any isInvalidRow grid
   || any isInvalidRow (Data.List.transpose grid)
   || any isInvalidRow (subGridsToRows grid)
   where
     isInvalidRow row =
       let fixeds         = [x | Fixed x <- row]
-          emptyPossibles = [x | Possible x <- row, null x]
+          emptyPossibles = [() | Possible x <- row, x == Data.Bits.zeroBits]
       in hasDups fixeds || not (null emptyPossibles)

     hasDups l = hasDups' l []

     hasDups' [] _ = False
     hasDups' (y:ys) xs
       | y `elem` xs = True
       | otherwise   = hasDups' ys (y:xs)
```

And finally, we change the `nextGrids` functions to use bit operations:

```haskell
nextGrids :: Grid -> (Grid, Grid)
nextGrids grid =
  let (i, first@(Fixed _), rest) =
        fixCell
        . Data.List.minimumBy (compare `Data.Function.on` (possibilityCount . snd))
        . filter (isPossible . snd)
        . zip [0..]
        . concat
        $ grid
  in (replace2D i first grid, replace2D i rest grid)
  where
    possibilityCount (Possible xs) = Data.Bits.popCount xs
    possibilityCount (Fixed _)     = 1

    fixCell ~(i, Possible xs) =
      let x = Data.Bits.countTrailingZeros xs
      in case makeCell (Data.Bits.clearBit xs x) of
        Nothing -> error "Impossible case"
        Just cell -> (i, Fixed (Data.Bits.bit x), cell)

    replace2D :: Int -> a -> [[a]] -> [[a]]
    replace2D i v = 
      let (x, y) = (i `quot` 9, i `mod` 9) in replace x (replace y (const v))
    replace p f xs = [if i == p then f x else x | (x, i) <- zip xs [0..]]
```

`possibilityCount` now uses `Data.Bits.popCount` to count the number of bits set to 1. `fixCell` now chooses the first set bit from right as the digit to fix. Rest of the code stays the same. Let's build and run it:

```plain
$ stack build
$ cat sudoku17.txt | time stack exec sudoku > /dev/null
       80.30 real        79.82 user         0.50 sys
```

Wow! That is almost 3.2x faster than the previous solution. It's a massive win! But let's not be content yet. To the profiler again!

## Back to the Profiler

Running the profiler again now gives us these top six culprits:

Cost Centre                  Src                         %time  %alloc
-------------                -------                    ------ -------
`exclusivePossibilities`     Sudoku.hs:(57,1)-(74,26)     22.2    16.6
`exclusivePossibilities.\.\` Sudoku.hs:64:23-96           18.7    32.8
`fixM.\`                     Sudoku.hs:15:27-65           12.3     0.1
`pruneGrid'`                 Sudoku.hs:(115,1)-(118,64)    5.6     8.6
`pruneCellsByFixed`          Sudoku.hs:(83,1)-(88,36)      5.1     7.1
`exclusivePossibilities.\`   Sudoku.hs:69:36-68            4.3     3.5

Hurray! `pruneCellsByFixed.pruneCell` has disappeared from the list of top bottlenecks. Though `exclusivePossibilities` still remains here as expected.

`exclusivePossibilities` is a big function. The profiler does not really tell us which parts of it are actually slow. That's because by default, the profiler only considers functions as _Cost Centres_. We need to give it hints for it to be able to find bottlenecks inside functions. For that, we need to insert [_Cost Centre_ annotations] in the code:

```haskell
exclusivePossibilities :: [Cell] -> [Data.Word.Word16]
exclusivePossibilities row =
  row
  & ({-# SCC "EP.zip" #-} zip [1..9])
  & ({-# SCC "EP.filter" #-} filter (isPossible . snd))
  & ({-# SCC "EP.foldl" #-} Data.List.foldl'
      (\acc ~(i, Possible xs) ->
        Data.List.foldl'
          (\acc' n -> if Data.Bits.testBit xs n
                      then Map.insertWith prepend n [i] acc'
                      else acc')
          acc
          [1..9])
      Map.empty)
  & ({-# SCC "EP.Map.filter1" #-} Map.filter ((< 4) . length))
  & ({-# SCC "EP.Map.foldl" #-} Map.foldlWithKey'(\acc x is -> Map.insertWith prepend is [x] acc) Map.empty)
  & ({-# SCC "EP.Map.filter2" #-} Map.filterWithKey (\is xs -> length is == length xs))
  & ({-# SCC "EP.Map.elems" #-} Map.elems)
  & ({-# SCC "EP.map" #-} map (Data.List.foldl' Data.Bits.setBit Data.Bits.zeroBits))
  where
    prepend ~[y] ys = y:ys
```

After profiling the code again, we get a different list of bottlenecks:

Cost Centre                  Src                         %time  %alloc
-------------                -------                    ------ -------
`exclusivePossibilities.\.\` Sudoku.hs:64:23-96           20.3   31.4
`fixM.\`                     Sudoku.hs:15:27-65           11.1    0.1
`pruneGrid'`                 Sudoku.hs:(115,1)-(118,64)    5.2    8.3
`EP.zip`                     Sudoku.hs:59:27-36            4.9   10.7
`pruneCellsByFixed`          Sudoku.hs:(83,1)-(88,36)      4.8    6.8
`exclusivePossibilities.\`   Sudoku.hs:69:64-96            4.3    3.4
`exclusivePossibilities.\`   Sudoku.hs:(63,9)-(66,16)      4.2    0.0

So almost one-fifth of the time is actually going in this nested one-line anonymous function inside `exclusivePossibilities`:

```haskell
(\acc' n -> if Data.Bits.testBit xs n then Map.insertWith prepend n [i] acc' else acc')
```

If you recall from the [explanation][2] given in the last part, this is the function which computes the mapping of the digits to the cells. Here, we accumulate the mapping in a [`Data.Map.Strict`] map, keys of which are the digits and values are the indices of the cells in which the digits occur.

So here's an idea: since this map is always going to have `1` to `9` as keys, what if we replace it with a data structure which has nine fixed slots to put the indices in? That _may_ be faster than using a generic map. Let's try it out.

## Accumulators Assemble!

We need a data structure with nine slots to accumulate lists of indices. That sounds easy:

```haskell
-- Exclusive Possibilities Accumulator
data ExPosAcc = ExPosAcc ![Int] ![Int] ![Int] ![Int] ![Int] ![Int] ![Int] ![Int] ![Int]

exPosAccEmpty :: ExPosAcc
exPosAccEmpty = ExPosAcc [] [] [] [] [] [] [] [] []
```

We just create a data type with nine fields --- one for each digit --- where the fields' values are just lists of integers. Adding indices to the accumulator is quite straightforward:

```haskell
exPosAccInsert :: Int -> Int -> ExPosAcc -> ExPosAcc
exPosAccInsert 1 i (ExPosAcc v1 v2 v3 v4 v5 v6 v7 v8 v9) =
  ExPosAcc (i:v1) v2 v3 v4 v5 v6 v7 v8 v9
exPosAccInsert 2 i (ExPosAcc v1 v2 v3 v4 v5 v6 v7 v8 v9) =
  ExPosAcc v1 (i:v2) v3 v4 v5 v6 v7 v8 v9
exPosAccInsert 3 i (ExPosAcc v1 v2 v3 v4 v5 v6 v7 v8 v9) =
  ExPosAcc v1 v2 (i:v3) v4 v5 v6 v7 v8 v9
exPosAccInsert 4 i (ExPosAcc v1 v2 v3 v4 v5 v6 v7 v8 v9) =
  ExPosAcc v1 v2 v3 (i:v4) v5 v6 v7 v8 v9
exPosAccInsert 5 i (ExPosAcc v1 v2 v3 v4 v5 v6 v7 v8 v9) =
  ExPosAcc v1 v2 v3 v4 (i:v5) v6 v7 v8 v9
exPosAccInsert 6 i (ExPosAcc v1 v2 v3 v4 v5 v6 v7 v8 v9) =
  ExPosAcc v1 v2 v3 v4 v5 (i:v6) v7 v8 v9
exPosAccInsert 7 i (ExPosAcc v1 v2 v3 v4 v5 v6 v7 v8 v9) =
  ExPosAcc v1 v2 v3 v4 v5 v6 (i:v7) v8 v9
exPosAccInsert 8 i (ExPosAcc v1 v2 v3 v4 v5 v6 v7 v8 v9) =
  ExPosAcc v1 v2 v3 v4 v5 v6 v7 (i:v8) v9
exPosAccInsert 9 i (ExPosAcc v1 v2 v3 v4 v5 v6 v7 v8 v9) =
  ExPosAcc v1 v2 v3 v4 v5 v6 v7 v8 (i:v9)
exPosAccInsert _ _ _ = error "Impossible"
```

We pattern match on the digit for the nine cases and cons the index onto the right list for the digit. And finally, a function to convert this accumulator to an associative list:

```haskell
exPosAccToList :: ExPosAcc -> [(Int, [Int])]
exPosAccToList (ExPosAcc v1 v2 v3 v4 v5 v6 v7 v8 v9) =
  [(1, v1), (2, v2), (3, v3), (4, v4), (5, v5), (6, v6), (7, v7), (8, v8), (9, v9)]
```

The change to the `exclusivePossibilities` function is quite minimal:

```diff
 exclusivePossibilities :: [Cell] -> [Data.Word.Word16]
 exclusivePossibilities row =
   row
   & zip [1..9]
   & filter (isPossible . snd)
   & Data.List.foldl'
       (\acc ~(i, Possible xs) ->
         Data.List.foldl'
-          (\acc' n -> if Data.Bits.testBit xs n
-                      then Map.insertWith prepend n [i] acc'
-                      else acc')
+          (\acc' n -> if Data.Bits.testBit xs n
+                      then exPosAccInsert n i acc'
+                      else acc')
           acc
           [1..9])
-      Map.empty
+      exPosAccEmpty
-  & Map.filter ((< 4) . length)
-  & Map.foldlWithKey'(\acc x is -> Map.insertWith prepend is [x] acc) Map.empty
+  & exPosAccToList
+  & filter ((< 4) . length . snd)
+  & Data.List.foldl' (\acc (x, is) -> Map.insertWith prepend is [x] acc) Map.empty
   & Map.filterWithKey (\is xs -> length is == length xs)
   & Map.elems
   & map (Data.List.foldl' Data.Bits.setBit Data.Bits.zeroBits)
   where
     prepend ~[y] ys = y:ys
```

Instead of doing insert in the map, now we do inserts in our custom accumulator. And instead of filtering and folding the map, we now filter and fold the list we get from the accumulator. No further code changes are required. Let's check the performance now:

```plain
$ stack build
$ cat sudoku17.txt | time stack exec sudoku > /dev/null
       64.58 real        64.20 user         0.40 sys
```

Nice! That brings our run time down by another 24%. A run with profiling on gives us the following top bottlenecks now:

Cost Centre                Src                                            %time  %alloc
-------------              -------                                       ------ -------
`exclusivePossibilities.\` Sudoku.hs:(85,9)-(88,16)                        15.0    8.7
`fixM.\`                   Sudoku.hs:15:27-65                              11.1    0.1
`exclusivePossibilities.\` Sudoku.hs:92:64-96                               7.0   13.9
`pruneCellsByFixed`        Sudoku.hs:(106,1)-(111,36)                       6.2    6.9
`pruneGrid'`               Sudoku.hs:(138,1)-(141,64)                       5.3    8.3
`EP.filter2`               Sudoku.hs:91:31-59                               4.2    6.4
`EP.zip`                   Sudoku.hs:81:27-36                               4.0   10.8
`chunksOf`                 Data/List/Split/Internals.hs:(514,1)-(517,49)    3.4    7.4
`exclusivePossibilities`   Sudoku.hs:(79,1)-(97,26)                         3.3    2.6
`==`                       Sudoku.hs:22:27-28                               2.9    0.0

The bottleneck function from the last round has disappeared now. If we look closely, we find that around 15% of the run time now goes into list traversal and manipulation. This is in the functions `pruneCellsByFixed`, `pruneGrid'` and `chunksOf`, two of which are majorly list traversal and transposition, and the third is list splitting. Maybe it's time to get rid of lists altogether?

## Vectors of Speed


[previous part]: /posts/fast-sudoku-solver-in-haskell-2/
[Fast Sudoku Solver in Haskell #1: A Simple Solution]: /posts/fast-sudoku-solver-in-haskell-1/
[Fast Sudoku Solver in Haskell #2: A 200x Faster Solution]: /posts/fast-sudoku-solver-in-haskell-2/
[Fast Sudoku Solver in Haskell #3: Picking the Right Data Structures]: /drafts/fast-sudoku-solver-in-haskell-3/
[Sudoku]: https://en.wikipedia.org/wiki/Sudoku
[University of Glasgow]: https://en.wikipedia.org/wiki/University_of_Glasgow
[singly linked lists]: https://en.wikipedia.org/wiki/Linked_list#Singly_linked_list
[asymptotic complexity]: https://en.wikipedia.org/wiki/Asymptotic_complexity
[_Set_]: https://en.wikipedia.org/wiki/Set_(abstract_data_type)
[`Data.Set`]: https://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Set.html
[self-balancing binary search tree]: https://en.wikipedia.org/wiki/Self-balancing_binary_search_tree
[`Data.HashSet`]: https://hackage.haskell.org/package/unordered-containers-0.2.9.0/docs/Data-HashSet.html
[hash array mapped trie]: https://en.wikipedia.org/wiki/Hash_array_mapped_trie
[`Data.IntSet`]: https://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-IntSet.html
[radix tree]: https://en.wikipedia.org/wiki/Radix_tree
[`Data.Word`]: https://hackage.haskell.org/package/base-4.6.0.1/docs/Data-Word.html
[_BitSet_]: https://en.wikipedia.org/wiki/Bitset
[bits]: https://en.wikipedia.org/wiki/Bit
[`Data.Word.Word16`]: https://hackage.haskell.org/package/base-4.6.0.1/docs/Data-Word.html#t:Word16
[`Data.Bits`]: https://hackage.haskell.org/package/base-4.6.0.1/docs/Data-Bits.html
[_Cost Centre_ annotations]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html#inserting-cost-centres-by-hand
[`Data.Map.Strict`]: https://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Strict.html


[1]: /files/sudoku17.txt.bz2
[2]: /posts/fast-sudoku-solver-in-haskell-2/#a-little-forward-a-little-backward

[^machinespec]: All the runs were done on my MacBook Pro from 2014 with 2.2 GHz Intel Core i7 CPU and 16 GB memory.
[^cc]: Notice the British english spelling. GHC was originally developed in [University of Glasgow] in Scotland.
