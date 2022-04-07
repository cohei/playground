module Main (main) where

import           Test.Hspec                                         (describe,
                                                                     hspec)

import qualified ApproximationSpec                                  (spec)
import qualified DeleteMinSpec                                      (spec)
import qualified FizzBuzzSpec                                       (spec)
import qualified FoldToTreeSpec                                     (spec)
import qualified IOSpec                                             (spec)
import qualified IsItWrongToWantScalaToHaveExistentialOperatorsSpec (spec)
import qualified LevityPolymorphismSpec                             (spec)
import qualified MapAccumWithStateSpec                              (spec)
import qualified MutableStateInHaskellSpec                          (spec)
import qualified SizeSpec                                           (spec)
import qualified Wa.MonoidSpec                                      (spec)
import qualified Wa.ReaderSpec                                      (spec)

main :: IO ()
main = hspec $ do
  describe "Approximation" ApproximationSpec.spec
  describe "DeleteMinSpec" DeleteMinSpec.spec
  describe "FizzBuzz" FizzBuzzSpec.spec
  describe "FoldToTree" FoldToTreeSpec.spec
  describe "IOSpec" IOSpec.spec
  describe "IsItWrongToWantScalaToHaveExistentialOperators" IsItWrongToWantScalaToHaveExistentialOperatorsSpec.spec
  describe "LevityPolymorphism" LevityPolymorphismSpec.spec
  describe "MapAccumWithState" MapAccumWithStateSpec.spec
  describe "MutableStateInHaskell" MutableStateInHaskellSpec.spec
  describe "Size" SizeSpec.spec
  describe "Wa.Monoid" Wa.MonoidSpec.spec
  describe "Wa.Reader" Wa.ReaderSpec.spec
