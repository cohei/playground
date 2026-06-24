{-# LANGUAGE DerivingStrategies #-}

module FizzBuzz.Rule (Rule, runRule, rule, modRule) where

import Control.Monad (guard)
import Numeric.Natural (Natural)

newtype Rule a = Rule (Natural -> Maybe a)
  deriving newtype (Semigroup)

runRule :: (Show a) => Rule a -> [String]
runRule (Rule r) = map (\n -> maybe (show n) show (r n)) [1 ..]

rule :: (Natural -> Bool) -> a -> Rule a
rule p x = Rule $ \n -> x <$ guard (p n)

modRule :: Natural -> a -> Rule a
modRule n = rule (`isDivisibleBy` n)

isDivisibleBy :: (Integral a) => a -> a -> Bool
isDivisibleBy n m = n `mod` m == 0
