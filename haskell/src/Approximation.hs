module Approximation
  ( bisection
  , newton
  , simpson
  ) where

import           Data.List  (find, tails)
import           Data.Maybe (fromJust)

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
newton f f' = converge (==) . iterate next
  where
    next x = x - f x / f' x

converge :: (a -> a -> Bool) -> [a] -> a
converge p = head . fromJust . find isConverge . tails
  where
    isConverge []          = False
    isConverge [_]         = False
    isConverge (x : y : _) = p x y

-- | Simpson Method
simpson :: (Ord a, Floating a) => (a -> a) -> a -> a -> a
simpson f a b = converge (\x y -> x - y < 10 ** (-10)) $ map (simpSum f) $ iterate addMiddle [a, b]

addMiddle :: Fractional a => [a] -> [a]
addMiddle xs = (zip xs (tail xs) >>= \(a, b) -> [a, middle a b]) ++ [last xs]

rule :: Fractional a => (a -> a) -> a -> a -> a
rule f a b = (b - a) / 6 * (f a + 4 * f (middle a b) + f b)

simpSum :: Fractional a => (a -> a) -> [a] -> a
simpSum f xs = sum $ zipWith (rule f) xs $ tail xs

middle :: Fractional a => a -> a -> a
middle x y = (x + y) / 2
