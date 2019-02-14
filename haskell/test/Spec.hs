import           Test.Hspec             (hspec)

import qualified ApproximationSpec      (spec)
import qualified IOSpec                 (spec)
import qualified LevityPolymorphismSpec (spec)

main :: IO ()
main = hspec $ do
  ApproximationSpec.spec
  IOSpec.spec
  LevityPolymorphismSpec.spec
