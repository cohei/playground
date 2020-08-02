-- | https://twitter.com/m2ym/status/1289096415122059265
module Prism (main) where

import           Control.Lens (each, (%~), (&), (^..), _2)
import           Data.Map     (Map, fromList)

m :: Map Char (Int, Double)
m = fromList [('a', (1, 1.1)), ('b', (2, 2.2))]

main :: IO ()
main = do
  print $ m & each . _2 %~ (+ 0.01)
  -- fromList [('a',(1,1.11)),('b',(2,2.21))]

  print $ m ^.. each . _2
  -- [1.1,2.2]
