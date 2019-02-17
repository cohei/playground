import           Test.Hspec             (describe, hspec)

import qualified ApproximationSpec      (spec)
import qualified IOSpec                 (spec)
import qualified LevityPolymorphismSpec (spec)

main :: IO ()
main = hspec $ do
  describe "Approximation" ApproximationSpec.spec
  describe "IOSpec" IOSpec.spec
  describe "LevityPolymorphism" LevityPolymorphismSpec.spec
