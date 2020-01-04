import           Test.Hspec             (describe, hspec)

import qualified ApproximationSpec      (spec)
import qualified FizzBuzzSpec           (spec)
import qualified IOSpec                 (spec)
import qualified LevityPolymorphismSpec (spec)
import qualified MapAccumWithStateSpec  (spec)
import qualified SizeSpec               (spec)
import qualified Wa.MonoidSpec          (spec)
import qualified Wa.ReaderSpec          (spec)

main :: IO ()
main = hspec $ do
  describe "Approximation" ApproximationSpec.spec
  describe "FizzBuzz" FizzBuzzSpec.spec
  describe "IOSpec" IOSpec.spec
  describe "LevityPolymorphism" LevityPolymorphismSpec.spec
  describe "MapAccumWithState" MapAccumWithStateSpec.spec
  describe "Size" SizeSpec.spec
  describe "Wa.Monoid" Wa.MonoidSpec.spec
  describe "Wa.Reader" Wa.ReaderSpec.spec
