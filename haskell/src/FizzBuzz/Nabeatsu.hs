module FizzBuzz.Nabeatsu (nabeatsu) where

import Utility (toDigits)

import FizzBuzz.Rule (Rule, modRule, rule)

data Aho = Aho
  deriving (Show)

instance Semigroup Aho where
  _ <> _ = Aho

nabeatsu :: Rule Aho
nabeatsu = (modRule 3 <> contains3) Aho

contains3 :: a -> Rule a
contains3 = rule $ (3 `elem`) . toDigits 10
