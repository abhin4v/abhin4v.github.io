---
title: "Notes for 'Thinking with Types: Type-level Programming in Haskell'"
date: 2019-11-22
description: "Notes for the book 'Thinking with Types: Type-level Programming in Haskell'"
tags: haskell, notes, programming
author: Abhinav Sarkar
toc: left
---

* toc

## Introduction

- Type-level Programming (TLP) is writing programs that run at compile-time, unlike term-level programming which is writing programs that run at run-time.
- TLP should be used in moderation.
- TLP should be mostly used
  - for programs that are catastrophic to get wrong (finance, healthcare, etc).
  - when it simplifies the program API massively.
  - when power-to-weight ratio of adding TLP is high.
- Types are not a silver bullet for fixing all errors:
  - Correct programs can be not well-typed.
  - It could be hard to assign type for useful programs. e.g. `printf` from C.
- Types can turn possible runtime errors into compile-time errors.

## Chapter 1. The Algebra Behind Types

### Isomorphisms and Cardinalities

- *Cardinality* of a type is the number of values it can have ignoring bottoms. The values of a type are also called the *inhabitants* of the type.
```haskell
data Void
 -- no possible values. cardinality: 0
data Unit = Unit
 -- only one possible value. cardinality: 1
data Bool = True | False
 -- only two possible values. cardinality: 2
```
- Cardinality is written using notation: `|Void| = 0`
- Two types are said to be _Isomorphic_ if they have same cardinality.
- An _isomorphism_ between types `a` and `b` is a pair of functions `to` and `from` such that:
```haskell
to :: a -> b
from :: b -> a
to . from = id
from . to = id
```

### Sum, Product and Exponential Types

- `Either a b` is a _Sum_ type. Its number of inhabitants is sum of the number of inhabitants of type `a` and `b` like so: `|a|` possible values with `Left` constructor and `|b|` possible values with the `Right` constructor. Formally:
```haskell
|Either a b| = |a| + |b|
```
- `(a, b)` is a _Product_ type. Its number of inhabitant is the product of the number of inhabitants of types `a` and `b`. Formally:
```haskell
|(a, b)| = |a| * |b|
```
- Some more examples:
```haskell
|Maybe a| = |Nothing| + |Just a| = 1 + |a|
|[a]| = 1 + |a| + |a|^2 + |a|^3 + ...
|Either a Void| = |a| + 0 = |a|
|Either Void a| = 0 + |a| = |a|
|(a, Unit)| = |a| * 1 = |a|
|(Unit, a)| = 1 * |a| = |a|
```
- Function types are exponentiation types.
```haskell
|a -> b| = |b|^|a|
```
For every value in domain `a` there can be `|b|` possible values in the range `b`. And there are `|a|` possible values in domain `a`. So:
```haskell
|a -> b|
  = |b| * |b| * ... * |b| -- (|a| times)
  = |b|^|a|
```
- Data can be represented in many possible isomorphic types. Some of them are more useful than others. Example:
```haskell
data TicTacToe1 a = TicTacToe1
  { topLeft      :: a
  , topCenter    :: a
  , topRight     :: a
  , middleLeft   :: a
  , middleCenter :: a
  , middleRight  :: a
  , bottomLeft   :: a
  , bottomCenter :: a
  , bottomRight  :: a
  }

|TicTacToe1 a|
  = |a| * |a| * ... * |a| -- 9 times
  = |a|^9

emptyBoard1 :: TicTacToe1 (Maybe Bool)
emptyBoard1 =
  TicTacToe1 Nothing Nothing Nothing
             Nothing Nothing Nothing
             Nothing Nothing Nothing

data Three = One | Two | Three
data TicTacToe2 a =
  TicTacToe2 (Three -> Three -> a)

|TicTacToe2 a| = |a|^(|Three| * |Three|)
               = |a|^(3*3)
               = |a|^9

emptyBoard2 :: TicTacToe2 (Maybe Bool)
emptyBoard2 =
  TicTacToe2 $ const $ const Nothing
```

### The Curry-Howard Isomorphism

- Every logic statement can be expressed as an equivalent computer program.
- Helps us analyze mathematical theorems through programming.

### Canonical Representations

- Since multiple equivalent representations of a type are possible, the representation in form of sum of products is considered the canonical representation of the type. Example:
```haskell
Either a (Either b (c, d)) -- canonical

(a, Bool) -- not canonical
Either a a
-- same cardinality as above but canonical
```

## Chapter 2. Terms, Types and Kinds

### The Kind System

- _Terms_ are things manipulated at runtime. _Types_ of terms are used by compiler to prove things about the terms.
- Similarly, _Types_ are things manipulated at compile-time. _Kinds_ of types are used by the compiler to prove things about the types.
- Kinds are "the types of the Types".
- Kind of things that can exist at runtime (terms) is `*`. That is, kind of `Int`, `String` etc is `*`.
```haskell
Prelude> :type True
True :: Bool
Prelude> :kind Bool
Bool :: *
```
- There are kinds other than `*`. For example:
```haskell
Prelude> :kind Show Int
Show Int :: Constraint
```
- Higher-kinded types have `(->)` in their kind signature:
```haskell
Prelude> :kind Maybe
Maybe :: * -> *
Prelude> :kind Maybe Int
Maybe Int :: *

Prelude> :type Control.Monad.Trans.Maybe.MaybeT
Control.Monad.Trans.Maybe.MaybeT
  :: m (Maybe a) -> Control.Monad.Trans.Maybe.MaybeT m a
Prelude> :kind Control.Monad.Trans.Maybe.MaybeT
Control.Monad.Trans.Maybe.MaybeT :: (* -> *) -> * -> *
Prelude> :kind Control.Monad.Trans.Maybe.MaybeT IO Int
Control.Monad.Trans.Maybe.MaybeT IO Int :: *
```

### Data Kinds

- `-XDataKinds` extension lets us create new kinds.
- It lifts data constructors into type constructors and types into kinds.
```haskell
Prelude> :set -XDataKinds
Prelude> data Allow = Yes | No
Prelude> :type Yes
Yes :: Allow
-- Yes is data constructor
Prelude> :kind Allow -- Allow is a type
Allow :: *
Prelude> :kind 'Yes
'Yes :: Allow
-- 'Yes is a type too. Its kind is 'Allow.
```
- Lifted constructors and types are written with a preceding `'` (called _tick_).

### Promotion of Built-In Types

- `-XDataKinds` extension promotes built-in types too.
- Strings are promoted to the kind `Symbol`.
- Natural numbers are promoted to the kind `Nat`.
```haskell
Prelude> :kind "hi"
"hi" :: GHC.Types.Symbol
-- "hi" is a type-level string
Prelude> :kind 123
123 :: GHC.Types.Nat
-- 123 is a type-level natural number
```
- We can do type level operations on `Symbol`s and `Nat`s.
```haskell
Prelude> :m +GHC.TypeLits
Prelude GHC.TypeLits> :kind AppendSymbol
AppendSymbol :: Symbol -> Symbol -> Symbol
Prelude GHC.TypeLits> :kind! AppendSymbol "hello " "there"
AppendSymbol "hello " "there" :: Symbol
= "hello there"
Prelude GHC.TypeLits> :set -XTypeOperators
Prelude GHC.TypeLits> :kind! (1 + 2) ^ 7
(1 + 2) ^ 7 :: Nat
= 2187
```
- `-XTypeOperators` extension is needed for applying type-level functions with symbolic identifiers.
- There are type-level lists and tuples:
```haskell
Prelude GHC.TypeLits> :kind '[ 'True ]
'[ 'True ] :: [Bool]
Prelude GHC.TypeLits> :kind '[1,2,3]
'[1,2,3] :: [Nat]
Prelude GHC.TypeLits> :kind '["abc"]
'["abc"] :: [Symbol]
Prelude GHC.TypeLits> :kind '(6, "x", 'False)
'(6, "x", 'False) :: (Nat, Symbol, Bool)
```

### Type-level Functions

- With the `-XTypeFamilies` extension, it's possible to write new type-level functions as closed type families:
```haskell
Prelude> :set -XDataKinds
Prelude> :set -XTypeFamilies
Prelude> :{
Prelude| type family And (x :: Bool) (y :: Bool) :: Bool where
Prelude|   And 'True 'True = 'True
Prelude|   And _     _     = 'False
Prelude| :}
Prelude> :kind And
And :: Bool -> Bool -> Bool
Prelude> :kind! And 'True 'False
And 'True 'False :: Bool
= 'False
Prelude> :kind! And 'True 'True
And 'True 'True :: Bool
= 'True
Prelude> :kind! And 'False 'True
And 'False 'True :: Bool
= 'False
```