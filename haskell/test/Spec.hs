import           Test.Hspec             (hspec)

import qualified IOSpec                 (spec)
import qualified LevityPolymorphismSpec (spec)

main :: IO ()
main = hspec $ do
  IOSpec.spec
  LevityPolymorphismSpec.spec
