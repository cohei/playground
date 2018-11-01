{-# LANGUAGE NoImplicitPrelude #-}
module DataAsFunction
  (
  -- * Tuple
    pair
  , fst
  , snd
  -- * Maybe
  , nothing
  , just
  , maybe
  -- * Either
  , left
  , right
  , either
  ) where

pair :: a -> b -> (a -> b -> c) -> c
pair x y p = p x y

fst :: ((a -> b -> a) -> c) -> c
fst p = p (\x _y -> x)

snd :: ((a -> b -> b) -> c) -> c
snd p = p (\_x y -> y)

nothing :: b -> (a -> b) -> b
nothing n _ = n

just :: a -> b -> (a -> b) -> b
just x _n j = j x

maybe :: b -> (a -> b) -> (b -> (a -> b) -> c) -> c
maybe n j m = m n j

left :: a -> (a -> c) -> (b -> c) -> c
left  x l _r = l x

right :: b -> (a -> c) -> (b -> c) -> c
right x _l r = r x

either :: (a -> c) -> (b -> c) -> ((a -> c) -> (b -> c) -> d) -> d
either l r e = e l r
