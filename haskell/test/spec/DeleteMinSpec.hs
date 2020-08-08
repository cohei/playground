{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE TypeApplications #-}
module DeleteMinSpec (spec) where

import           Data.Function             (on)
import           Data.List                 (sort)
import           Test.Hspec                (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck.Instances ()

import           DeleteMin                 (deleteMin, deleteMinPara)

spec :: Spec
spec = do
  describe "deleteMinPara" $ do
    it "returns list without minimum" $ deleteMinPara @Int [1, 2, 3] `shouldBe` [2, 3]
    it "returns empty list" $ deleteMinPara @Int [1] `shouldBe` []
    it "deletes one of the same elements" $ deleteMinPara @Int [0, 0] `shouldBe` [0]
    prop "deleteMinPara and deleteMin are the same" $ \xs -> ((==) `on` sort) (deleteMinPara @Int xs) (deleteMin xs)
