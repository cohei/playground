{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | [Seemingly impossible functional programs](http://math.andrej.com/2007/09/28/seemingly-impossible-functional-programs/)
module SeeminglyImpossible where

import Data.Function (on)
import Numeric.Natural (Natural)
import qualified Data.MemoCombinators as Memo (integral)

type Cantor = Natural -> Bool

(#) :: Bool -> Cantor -> Cantor
b # c = \n -> if n == 0 then b else c (n - 1)

anyCantor, allCantor :: (Cantor -> Bool) -> Bool
find :: (Cantor -> Bool) -> Cantor

find = find7
anyCantor p = p (find p)
allCantor = not . anyCantor . (not .)

find1 :: (Cantor -> Bool) -> Cantor
find1 p
  | anyCantor (p . (False #)) = False # find1 (p . (False #))
  | otherwise                 = True  # find1 (p . (True  #))

search :: (Cantor -> Bool) -> Maybe Cantor
search p = if anyCantor p then Just (find p) else Nothing

instance Eq a => Eq (Cantor -> a) where
  f1 == f2 = allCantor $ liftA2 (==) f1 f2

b2n :: Bool -> Natural
b2n False = 0
b2n True = 1

f, g, h :: Cantor -> Natural

f c = c' (7 * c' 4 +  4 * c' 7 + 4)
  where
    c' = b2n . c

g c = c' (c' 4 + 11 *  c' 7)
  where
    c' = b2n . c

h c = c' $ case (c 4, c 7) of
  (False, False) -> 4
  (False, True)  -> 8
  (True,  False) -> 11
  (True,  True)  -> 15
  where
    c' = b2n . c

least :: (Natural -> Bool) -> Natural
least p = if p 0 then 0 else 1 + least (p . succ)

(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

eq :: Natural -> Cantor -> Cantor -> Bool
eq n c1 c2 = all (\n' -> c1 n' == c2 n') [0 .. (if n == 0 then 0 else pred n)]

modulus :: Eq a => (Cantor -> a) -> Natural
modulus c = least $ \n -> allCantor $ \c1 -> allCantor $ \c2 -> eq n c1 c2 --> (c c1 == c c2)

projection :: Natural -> Cantor -> Natural
projection n c = b2n (c n)

find2 :: (Cantor -> Bool) -> Cantor
find2 p
  | p (False # find2 (p . (False #))) = False # find2 (p . (False #))
  | otherwise                         = True  # find2 (p . (True  #))

find3 :: (Cantor -> Bool) -> Cantor
find3 p = b # find3 (p . (b #))
  where
    b = not $ p $ False # find3 (p . (False #))

find4 :: (Cantor -> Bool) -> Cantor
find4 p
  | p c       = c
  | otherwise = True # find4 (p . (True  #))
  where
    c = False # find4 (p . (False #))

find5 :: (Cantor -> Bool) -> Cantor
find5 p n = not $ anyCantor' (p . transform)
  where
    anyCantor' :: (Cantor -> Bool) -> Bool
    anyCantor' p' = p' (find5 p')

    transform :: Cantor -> Cantor
    transform c m =
      case compare m n of
        LT -> find5 p m
        EQ -> False
        GT -> c $ m - n - 1

find6 :: (Cantor -> Bool) -> Cantor
find6 p = cantor
  where
    cantor :: Cantor
    cantor n = not $ anyCantor' (p . transform n)

    anyCantor' :: (Cantor -> Bool) -> Bool
    anyCantor' p' = p' (find6 p')

    transform :: Natural -> Cantor -> Cantor
    transform n c m =
      case compare m n of
        LT -> cantor m
        EQ -> False
        GT -> c $ m - n - 1

find7 :: (Cantor -> Bool) -> Cantor
find7 p = cantor
  where
    cantor :: Cantor
    cantor = Memo.integral $ \n -> not $ anyCantor' (p . transform n)

    anyCantor' :: (Cantor -> Bool) -> Bool
    anyCantor' p' = p' (find7 p')

    transform :: Natural -> Cantor -> Cantor
    transform n c m =
      case compare m n of
        LT -> cantor m
        EQ -> False
        GT -> c $ m - n - 1

f',g',h' :: Cantor -> Natural

f' c = c' $ 10 * c' (3 ^ (80 :: Int)) + 100 * c' (4 ^ (80 :: Int)) + 1000 * c' (5 ^ (80 :: Int))
  where
    c' = b2n . c

g' c = c' $ 10 * c' (3 ^ (80 :: Int)) + 100 * c' (4 ^ (80 :: Int)) + 1000 * c' (6 ^ (80 :: Int))
  where
    c' = b2n . c

h' c = c' k
  where
    c' = b2n . c

    i, j, k :: Natural
    i = if c (5 ^ (80 :: Int)) then 1000    else 0
    j = if c (3 ^ (80 :: Int)) then 10 + i  else i
    k = if c (4 ^ (80 :: Int)) then 100 + j else j

pointwiseAnd :: [Natural] -> (Cantor -> Bool)
pointwiseAnd []    _ = True
pointwiseAnd (n:a) b = b n && pointwiseAnd a b

sameElements :: [Natural] -> [Natural] -> Bool
sameElements = (==) `on` pointwiseAnd
