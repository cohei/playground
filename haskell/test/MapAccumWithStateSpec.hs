module MapAccumWithStateSpec (spec) where

import           Data.List         (mapAccumL, mapAccumR)
import           Test.Hspec        (Spec, describe, it, shouldBe)

import qualified MapAccumWithState (mapAccumL, mapAccumR)

spec :: Spec
spec = do
  let apply f = f (\x y -> (x + y, 2 * x)) 0 [1..5 :: Int]

  describe "mapAccumL" $
    it "is same as `Data.List.mapAccumL`" $
      apply MapAccumWithState.mapAccumL `shouldBe` apply mapAccumL

  describe "mapAccumR" $
    it "is same as `Data.List.mapAccumR`" $
      apply MapAccumWithState.mapAccumR `shouldBe` apply mapAccumR
