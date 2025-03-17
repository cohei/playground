{-# LANGUAGE DerivingStrategies #-}

module FizzBuzz.Rule (Rule, runRule, rule, modRule) where

import Control.Monad (guard)

newtype Rule a = Rule (Int -> Maybe a)
  deriving newtype (Semigroup)

runRule :: (Show a) => Rule a -> [String]
runRule (Rule r) = map (\i -> maybe (show i) show (r i)) [1 ..]

rule :: (Int -> Bool) -> a -> Rule a
rule p x = Rule $ \i -> x <$ guard (p i)

modRule :: Int -> a -> Rule a
modRule n = rule (`isDivisibleBy` n)

isDivisibleBy :: (Integral a) => a -> a -> Bool
isDivisibleBy n m = n `mod` m == 0
