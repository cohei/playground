-- https://x.com/motcho_tw/status/1836639297082134661
-- https://oeis.org/A075326
module Antifibonacci (antifibs) where

import Data.List (delete, unfoldr)
import Numeric.Natural (Natural)

antifibs :: [Natural]
antifibs = 0 : unfoldr (Just . step) [1 ..]

step :: (Eq a, Num a) => [a] -> (a, [a])
step (n : m : ns) = let s = n + m in (s, delete s ns)
step _ = error "more than 2 elements needed"
