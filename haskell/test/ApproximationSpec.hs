module ApproximationSpec (spec) where

import           Data.Maybe    (fromJust)
import           Test.Hspec    (Spec, describe, it, shouldBe, shouldSatisfy)

import           Approximation (bisection, newton, simpson)

spec :: Spec
spec = do
  describe "bisection" $ do
    it "finds zero point" $ fromJust (bisection f zero 5) `shouldSatisfy` within 1e-3 2.1642
    it "does not find zero point" $ bisection f zero 1 `shouldBe` Nothing

  describe "newton" $
    it "finds zero point" $ newton f f' five `shouldSatisfy` within 1e-4 2.1642

  describe "simpson" $
    it "integrates" $ simpson f zero 5 `shouldSatisfy` within 1e-16 (1385 / 12)

f, f' :: Num a => a -> a
f  x =     x ^ (3 :: Int) +     x ^ (2 :: Int) - 5 * x - 4
f' x = 3 * x ^ (2 :: Int) + 2 * x              - 5

zero, five :: Double
zero = 0
five = 5

within :: (Num a, Ord a) => a -> a -> a -> Bool
within accuracy expectation x = abs (x - expectation) < accuracy
