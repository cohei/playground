{-# LANGUAGE TypeApplications #-}
module SizeSpec (spec) where

import           Data.Proxy (Proxy (Proxy))
import           Test.Hspec (Spec, describe, it, shouldBe)

import           Size       (size)

spec :: Spec
spec =
  describe "size" $ do
    it "size of unit" $ size (Proxy @()) `shouldBe` 1
    it "size of boolean" $ size (Proxy @Bool) `shouldBe` 2
    it "size of Int" $ size (Proxy @Int) `shouldBe` 2 ^ (64 :: Int)
