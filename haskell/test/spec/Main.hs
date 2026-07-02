module Main (main) where

import Test.Hspec (describe, hspec)

import ApproximationSpec qualified (spec)
import DeleteMinSpec qualified (spec)
import FizzBuzzSpec qualified (spec)
import FoldToTreeSpec qualified (spec)
import IOSpec qualified (spec)
import IsItWrongToWantScalaToHaveExistentialOperatorsSpec qualified (spec)
import LevityPolymorphismSpec qualified (spec)
import MapAccumWithStateSpec qualified (spec)
import MutableStateInHaskellSpec qualified (spec)
import SizeSpec qualified (spec)
import Wa.MonoidSpec qualified (spec)
import Wa.ReaderSpec qualified (spec)

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
