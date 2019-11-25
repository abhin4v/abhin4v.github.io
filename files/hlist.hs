-- start snippet imports
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HList where

import Data.Typeable
-- end snippet imports

-- start snippet def
data HList (ts :: [*]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)
infixr 5 :#
-- end snippet def

-- start snippet instances
instance Eq (HList '[]) where
  HNil == HNil = True
instance (Eq t, Eq (HList ts))
    => Eq (HList (t ': ts)) where
  (x :# xs) == (y :# ys) =
    x == y && xs == ys

instance Ord (HList '[]) where
  HNil `compare` HNil = EQ
instance (Ord t, Ord (HList ts))
    => Ord (HList (t ': ts)) where
  (x :# xs) `compare` (y :# ys) =
    x `compare` y <> xs `compare` ys

instance Show (HList '[]) where
  show HNil = "[]"
instance (Typeable t, Show t, Show (HList ts))
    => Show (HList (t ': ts)) where
  show (x :# xs) =
    show x 
    ++ "@" ++ show (typeRep (Proxy @t))
    ++ " :# " ++ show xs
-- end snippet instances

-- start snippet ops
hLength :: HList ts -> Int
hLength HNil = 0
hLength (x :# xs) = 1 + hLength xs

hHead :: HList (t ': ts) -> t
hHead (t :# _) = t
-- end snippet ops
