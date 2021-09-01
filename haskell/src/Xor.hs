{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
module Xor (Xor (Xor)) where

import "group-theory" Data.Group (Group (invert), Abelian) -- package qualified for doctest
import Data.Group.Cyclic (Cyclic (generator))
import Data.Group.Finite (FiniteGroup, FiniteAbelianGroup)
import Test.QuickCheck (Arbitrary)

newtype Xor = Xor Bool
  deriving (Eq, Bounded, Show, Arbitrary)

-- |
-- >>> Xor True <> Xor True <> Xor True
-- Xor True
instance Semigroup Xor where
  Xor False <> Xor False = Xor False
  Xor False <> Xor True  = Xor True
  Xor True  <> Xor False = Xor True
  Xor True  <> Xor True  = Xor False

instance Monoid Xor where
  mempty = Xor False

instance Group Xor where
  invert (Xor False) = Xor False
  invert (Xor True)  = Xor True

-- |
-- prop> (x :: Xor) <> y == y <> x
-- prop> (x :: Xor) <> y <> invert x == y
instance Abelian Xor

-- |
-- >>> import Data.Group.Cyclic (generated')
-- >>> generated' :: [Xor]
-- [Xor True,Xor False]
instance Cyclic Xor where
  generator = Xor True

-- |
-- >>> import Data.Group.Finite (finiteOrder)
-- >>> finiteOrder (Xor False)
-- 1
-- >>> finiteOrder (Xor True)
-- 2
instance FiniteGroup Xor
instance FiniteAbelianGroup Xor
