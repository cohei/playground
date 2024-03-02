module Approximation
  ( bisection
  , newton
  , simpson
  ) where

import Data.Foldable (find)
import Data.List.NonEmpty (NonEmpty ((:|)), prependList, toList)
import Data.List.NonEmpty qualified as NE (iterate, last, tail)
import Data.Maybe (fromJust)

-- | Bisection Method
bisection :: (Ord a, Fractional a, Ord b, Num b) => (a -> b) -> a -> a -> Maybe a
bisection f a b
  | b - a < 1e-4              = Just b
  | f a == 0                  = Just a
  | f b == 0                  = Just b
  | differentSign (f a) (f m) = bisection f a m
  | differentSign (f m) (f b) = bisection f m b
  | otherwise                 = Nothing
  where
    m = middle a b

differentSign :: (Ord a, Num a) => a -> a -> Bool
differentSign x y = x > 0 && y < 0 || x < 0 && y > 0

-- | Newton Method
newton :: (Eq a, Fractional a) => (a -> a) -> (a -> a) -> a -> a
newton f f' = converge (==) . NE.iterate next
  where
    next x = x - f x / f' x

converge :: (a -> a -> Bool) -> NonEmpty a -> a
converge p = fst . fromJust . find (uncurry p) . pairs
  where
    pairs :: NonEmpty a -> [(a, a)]
    pairs = pairsWith (,)

-- | Simpson Method
simpson :: (Ord a, Floating a) => (a -> a) -> a -> a -> a
simpson f a b = converge (\x y -> x - y < 10 ** (-10)) $ fmap (simpSum f) $ NE.iterate addMiddle $ ne a b

addMiddle :: Fractional a => NonEmpty a -> NonEmpty a
addMiddle xs = concat (pairsWith (\a b -> [a, middle a b]) xs) |: NE.last xs

rule :: Fractional a => (a -> a) -> a -> a -> a
rule f a b = (b - a) / 6 * (f a + 4 * f (middle a b) + f b)

simpSum :: Fractional a => (a -> a) -> NonEmpty a -> a
simpSum f xs = sum $ pairsWith (rule f) xs

middle :: Fractional a => a -> a -> a
middle x y = (x + y) / 2

-- * NonEmpty utilities

ne :: a -> a -> NonEmpty a
ne x y = x :| [y]

(|:) :: [a] -> a -> NonEmpty a
xs |: x = xs `prependList` pure x

pairsWith :: (a -> a -> b) -> NonEmpty a -> [b]
pairsWith f xs = zipWith f (toList xs) (NE.tail xs)
