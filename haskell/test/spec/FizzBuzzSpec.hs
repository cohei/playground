{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FizzBuzzSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)

import FizzBuzz.FizzBuzz (buzz, fizz, hoge)
import FizzBuzz.Nabeatsu (nabeatsu)
import FizzBuzz.Rule (runRule)

spec :: Spec
spec = do
  describe "FizzBuzz.Rule" $ do
    it "Fizz Buzz" $ display (runRule $ fizz <> buzz) `shouldBe`
      "1 2 Fizz 4 Buzz Fizz 7 8 Fizz Buzz 11 Fizz 13 14 FizzBuzz 16 17 Fizz 19 Buzz Fizz 22 23 Fizz Buzz 26 Fizz 28 29 FizzBuzz 31 32 Fizz 34 Buzz Fizz 37 38 Fizz Buzz 41 Fizz 43 44 FizzBuzz 46 47 Fizz 49 Buzz Fizz 52 53 Fizz Buzz 56 Fizz 58 59 FizzBuzz 61 62 Fizz 64 Buzz Fizz 67 68 Fizz Buzz 71 Fizz 73 74 FizzBuzz 76 77 Fizz 79 Buzz Fizz 82 83 Fizz Buzz 86 Fizz 88 89 FizzBuzz 91 92 Fizz 94 Buzz Fizz 97 98 Fizz Buzz"

    it "Fizz Buzz Hoge" $ display (runRule $ fizz <> buzz <> hoge) `shouldBe`
      "1 2 Fizz 4 Buzz Fizz Hoge 8 Fizz Buzz 11 Fizz 13 Hoge FizzBuzz 16 17 Fizz 19 Buzz FizzHoge 22 23 Fizz Buzz 26 Fizz Hoge 29 FizzBuzz 31 32 Fizz 34 BuzzHoge Fizz 37 38 Fizz Buzz 41 FizzHoge 43 44 FizzBuzz 46 47 Fizz Hoge Buzz Fizz 52 53 Fizz Buzz Hoge Fizz 58 59 FizzBuzz 61 62 FizzHoge 64 Buzz Fizz 67 68 Fizz BuzzHoge 71 Fizz 73 74 FizzBuzz 76 Hoge Fizz 79 Buzz Fizz 82 83 FizzHoge Buzz 86 Fizz 88 89 FizzBuzz Hoge 92 Fizz 94 Buzz Fizz 97 Hoge Fizz Buzz"

    it "Nabeatsu" $ display (runRule nabeatsu) `shouldBe`
      "1 2 Aho 4 5 Aho 7 8 Aho 10 11 Aho Aho 14 Aho 16 17 Aho 19 20 Aho 22 Aho Aho 25 26 Aho 28 29 Aho Aho Aho Aho Aho Aho Aho Aho Aho Aho 40 41 Aho Aho 44 Aho 46 47 Aho 49 50 Aho 52 Aho Aho 55 56 Aho 58 59 Aho 61 62 Aho 64 65 Aho 67 68 Aho 70 71 Aho Aho 74 Aho 76 77 Aho 79 80 Aho 82 Aho Aho 85 86 Aho 88 89 Aho 91 92 Aho 94 95 Aho 97 98 Aho 100"

display :: [String] -> String
display = unwords . take 100
