{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
module NBonacci (nBonacci) where

import Control.Applicative (ZipList(ZipList))
import Data.Coerce (coerce)
import Data.Function (fix)
import Data.List (tails)
import Data.Monoid (Ap(Ap), Sum(Sum))

newtype SumZipList a = SumZipList (ZipList a)
  deriving newtype (Functor, Applicative)
  deriving (Semigroup, Monoid) via (Ap ZipList (Sum a))

-- |
-- >>> take 10 $ nBonacci 2
-- [0,1,1,2,3,5,8,13,21,34]
--
-- >>> take 10 $ nBonacci 3
-- [0,0,1,1,2,4,7,13,24,44]
nBonacci :: Int -> [Integer]
nBonacci n =
  fix \ns -> initialTerms ++ coerce (foldMap (coerce @_ @(SumZipList Integer)) (take n (tails ns)))
  where
    initialTerms :: [Integer]
    initialTerms = replicate (n - 1) 0 ++ [1]
