{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FizzBuzz.FizzBuzz (fizz, buzz, hoge) where

import           FizzBuzz.Rule (Rule, modRule)

newtype FizzBuzz =
  FizzBuzz String
  deriving Semigroup

instance Show FizzBuzz where
  show (FizzBuzz s) = s

fizz, buzz, hoge :: Rule FizzBuzz
fizz = modRule 3 $ FizzBuzz "Fizz"
buzz = modRule 5 $ FizzBuzz "Buzz"
hoge = modRule 7 $ FizzBuzz "Hoge"
