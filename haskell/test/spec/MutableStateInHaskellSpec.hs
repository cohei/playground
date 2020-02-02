{-# LANGUAGE ScopedTypeVariables #-}
module MutableStateInHaskellSpec (spec) where

import           Data.List               (sort)
import           Test.Hspec              (Spec, describe)
import           Test.Hspec.QuickCheck   (prop)
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

import           MutableStateInHaskell   (bubbleSortIO, bubbleSortST)

spec :: Spec
spec = do
  describe "bubbleSortIO" $ do
    prop "sorts" $ \(xs :: [Int]) -> monadicIO $ do
      r <- run $ bubbleSortIO xs
      assert $ r == sort xs

  describe "bubbleSortST" $ do
    prop "sorts" $ \(xs :: [Int]) -> bubbleSortST xs == sort xs
