module FizzBuzz.Nabeatsu (nabeatsu) where

import           FizzBuzz.Rule (Rule, modRule, rule)

data Aho =
  Aho
  deriving Show

instance Semigroup Aho where
  Aho <> Aho = Aho

nabeatsu :: Rule Aho
nabeatsu = (modRule 3 <> contains3) Aho

contains3 :: a -> Rule a
contains3 = rule $ \i -> '3' `elem` show i
