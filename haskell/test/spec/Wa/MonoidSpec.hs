module Wa.MonoidSpec (spec) where

import           Test.Hspec (Spec, it, shouldBe)

import           Wa.Reader  (の, と, 和, 積)

spec :: Spec
spec = do
  it "和" $ show (56 `と` 97 `と` 33 `の` 和) `shouldBe` "186"

  it "和と積" $ show ((3 `と` 5 `と` 7 `の` 和) `と` (2 `と` 11 `の` 和) `の` 積) `shouldBe` "195"
