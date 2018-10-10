import           Test.Hspec (hspec)

import qualified IOSpec     (spec)

main :: IO ()
main = hspec $ IOSpec.spec
