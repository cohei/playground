{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
module LevityPolymorphismSpec (spec) where

import Test.Hspec (it , describe, shouldBe, Spec)

import           LevityPolymorphism (toString, twice, add)

spec :: Spec
spec = do
  describe "toString" do
    it "Int#" $ toString 42# `shouldBe` "I# 42"
    it "Int" $ toString (42 :: Int) `shouldBe` "42"

  describe "add" do
    it "Int#" $ toString (add 42# 3#) `shouldBe` "I# 45"
    it "Int" $ toString (add 42 3 :: Int) `shouldBe` "45"

  describe "twice" do
    it "Int#" $ toString (twice 12#) `shouldBe` "I# 24"
    it "Int" $ toString (twice 12 :: Int) `shouldBe` "24"
