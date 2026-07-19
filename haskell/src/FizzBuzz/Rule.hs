{-# LANGUAGE DerivingStrategies #-}

module FizzBuzz.Rule (Rule, runRule, rule, modRule) where

import Control.Monad (guard)
import Numeric.Natural (Natural)
import Data.Coerce (coerce)

type Rule a = Natural -> Fallback Natural a

newtype Fallback a b = Fallback (Either a b)

instance Semigroup b => Semigroup (Fallback a b) where
  Fallback (Left _) <> v = v
  u <> Fallback (Left _) = u
  Fallback (Right x) <> Fallback (Right y) = Fallback (Right (x <> y))

runRule :: (Show a) => Rule a -> [String]
runRule r = map (either show show . coerce . r) [1 ..]

rule :: (Natural -> Bool) -> a -> Rule a
rule p x n = Fallback $ if p n then Right x else Left n

modRule :: Natural -> a -> Rule a
modRule n = rule (`isDivisibleBy` n)

isDivisibleBy :: (Integral a) => a -> a -> Bool
isDivisibleBy n m = n `mod` m == 0
