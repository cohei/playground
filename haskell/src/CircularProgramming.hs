{-# LANGUAGE ViewPatterns #-}

-- | https://wiki.haskell.org/Circular_programming
module CircularProgramming (repmin, normalize, diff) where

import Control.Arrow (ArrowLoop (loop))
import Control.Monad.Writer (MonadWriter (tell), runWriter)
import Data.Maybe (fromJust)
import Data.Semigroup (Min (Min, getMin))
import Data.Monoid (Sum (Sum, getSum))
import Data.Bifunctor (second)

-- ほんとは Traversable1 がよさそう
-- でもそうすると Apply が必要になる
circular :: (Traversable t, Monoid m) => (a -> m) -> (m -> b) -> (a -> b -> c) -> t a -> t c
circular monoidFrom monoidTo withFeedback =
  loop $ \(xs, feedback) ->
  second monoidTo $ runWriter $ traverse (\x -> withFeedback x feedback <$ tell (monoidFrom x)) xs

-- |
-- >>> import Data.List.NonEmpty (fromList)
-- >>> repmin (fromList [1, 2, 3])
-- 1 :| [1,1]
--
-- >>> import Data.Tree.Binary.Leafy (Tree (Leaf, (:*:)), printTree)
-- >>> printTree $ repmin $ (Leaf 3 :*: Leaf 2) :*: Leaf 1
--  ┌1
-- ┌┤
-- │└1
-- ┤
-- └1
repmin :: (Traversable t, Ord a) => t a -> t a
repmin = circular (Just . Min) (getMin . fromJust) (const id)

-- |
-- >>> normalize [1, 2, 3] :: [Double]
-- [0.2672612419124244,0.5345224838248488,0.8017837257372732]
normalize :: (Traversable t, Floating a) => t a -> t a
normalize = circular (Sum . square) (sqrt . getSum) (/)

square :: Num a => a -> a
square = (^ (2 :: Int))

-- |
-- >>> diff [1, 2, 3] :: [Double]
-- [-1.0,0.0,1.0]
diff :: (Traversable t, Floating a) => t a -> t a
diff = circular (\x -> (Sum x, Sum 1)) (\(Sum s, Sum n) -> s / fromInteger n) (-)
