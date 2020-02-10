module IsItWrongToWantScalaToHaveExistentialOperatorsSpec (spec) where

import           Control.Lens                                   (Traversal',
                                                                 (&), (.~),
                                                                 (^?), _Just)
import           Test.Hspec                                     (Spec, describe,
                                                                 it, shouldBe)

import           IsItWrongToWantScalaToHaveExistentialOperators (A (A), B (B),
                                                                 C (C), D (D),
                                                                 E (E), a, b, c,
                                                                 d, e)

spec :: Spec
spec = do
  describe "edcba" $ do
    it "extract" $ edcba ^? p `shouldBe` Just 1
    it "modify" $ (edcba & p .~ 2) `shouldBe` Just (E (D (Just (C (B (Just (A 2)))))))

  describe "ednon" $ do
    it "extract" $ ednon ^? p `shouldBe` Nothing
    it "modify" $ (ednon & p .~ 2) `shouldBe` ednon

p :: Traversal' (Maybe E) Int
p = _Just . e . d . c . b . a

edcba :: Maybe E
edcba = Just $ E $ D $ Just $ C $ B $ Just $ A 1

ednon :: Maybe E
ednon = Just $ E $ D Nothing
