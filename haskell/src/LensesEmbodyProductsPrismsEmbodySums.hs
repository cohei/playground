{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TupleSections             #-}
-- | <https://blog.jle.im/entry/lenses-products-prisms-sums.html Lenses embody Products, Prisms embody Sums>
module LensesEmbodyProductsPrismsEmbodySums where

import           Data.Bifunctor     (first)
import           Data.Bool          (bool)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Maybe         (isJust)
import           Data.Profunctor    (Choice (left'), Profunctor (dimap),
                                     Strong (first'))
import           Data.Tuple         (swap)
import           Numeric.Natural    (Natural)
import           Refined            (NotEqualTo, Refined, refineFail, unrefine)

class Isomorphic a b where
  from   :: a -> b
  to :: b -> a

data Person = Person { name :: String, age :: Int }

instance Isomorphic Person (String, Int) where
  from Person {..}   = (name, age)
  to (name, age) = Person {..}

instance Isomorphic (Either a a) (Bool, a) where
  from = either (False,) (True,)

  to (False, a) = Left a
  to (True, a)  = Right a

data Lens s t a b =
  forall q. Lens
  { split   :: s -> (a, q)
  , unsplit :: (b, q) -> t
  }

type Simple f s a = f s s a a
type Lens' s a = Simple Lens s a

view :: Lens s t a b -> s -> a
view Lens {..} = fst . split

set :: Lens s t a b -> b -> s -> t
set Lens {..} a = unsplit . (a,) . snd . split

overL :: Lens s t a b -> (a -> b) -> s -> t
overL Lens {..} f = unsplit . first f . split

personName :: Lens' Person String
personName = Lens { split = from :: Person -> (String, Int), unsplit = to }

personAge :: Lens' Person Int
personAge = Lens { split = swap . from :: Person -> (Int, String), unsplit = to . swap }

mysteryLens1 :: Lens' (Either a a) Bool
mysteryLens1 = Lens { split = from :: Either a a -> (Bool, a), unsplit = to }

mysteryLens2 :: Lens' (Either a a) a
mysteryLens2 = Lens { split = swap . from :: Either a a -> (a, Bool), unsplit = to . swap }

flipEither :: Either a a -> Either a a
flipEither = overL mysteryLens1 not

isRight :: Either a a -> Bool
isRight = view mysteryLens1

fromEither :: Either a a -> a
fromEither = view mysteryLens2

mapEither :: (a -> a) -> Either a a -> Either a a
mapEither = overL mysteryLens2

data Shape =
    Circle Double
  | RegPoly Natural Double

instance Isomorphic Shape (Either Double (Natural, Double)) where
  from (Circle r)    = Left r
  from (RegPoly n s) = Right (n, s)

  to = either Circle (\(n, s) -> RegPoly n s)

instance Isomorphic [a] (Either () (NonEmpty a)) where
  from []     = Left ()
  from (x:xs) = Right (x :| xs)

  to = either (const []) (\(x :| xs) -> x : xs)

type Not4 = Refined (NotEqualTo 4) Int

instance Isomorphic Int (Either () Not4) where
  from = maybe (Left ()) Right . refineFail
  to = either (\() -> 4) unrefine

data Prism s t a b =
  forall q. Prism
  { match  :: s -> Either a q
  , inject :: Either b q -> t
  }

type Prism' s a = Simple Prism s a

preview :: Prism s t a b -> s -> Maybe a
preview Prism {..} = either Just (const Nothing) . match

review :: Prism s t a b -> b -> t
review Prism {..} = inject . Left

overP :: Prism s t a b -> (a -> b) -> s -> t
overP Prism {..} f = inject . first f . match

_Circle :: Prism' Shape Double
_Circle = Prism { match = from :: Shape -> Either Double (Natural, Double), inject = to }

swapEither :: Either a b -> Either b a
swapEither = either Right Left

_RegPoly :: Prism' Shape (Natural, Double)
_RegPoly =
  Prism
  { match = swapEither . from :: Shape -> Either (Natural, Double) Double
  , inject = to . swapEither
  }

_Nil :: Prism' [a] ()
_Nil = Prism { match = from :: [a] -> (Either () (NonEmpty a)), inject = to }

_Cons :: Prism' [a] (NonEmpty a)
_Cons =
  Prism
  { match = swapEither . from :: [a] -> Either (NonEmpty a) ()
  , inject = to . swapEither
  }

only4 :: Prism' Int ()
only4 = Prism { match = from :: Int -> Either () Not4, inject = to }

not4 :: Prism' Int Not4
not4 = Prism { match = swapEither . from :: Int -> Either Not4 (), inject = to . swapEither }

isEqualTo4 :: Int -> Bool
isEqualTo4 = isJust . preview only4

four :: Int
four = review only4 ()

iso :: Profunctor p => (s -> a) -> (b -> t) -> p a b -> p s t
iso = dimap

makeLens :: Strong p => (s -> (a, q)) -> ((b, q) -> t) -> p a b -> p s t
makeLens split unsplit = iso split unsplit . first'

makePrism :: Choice p => (s -> Either a q) -> (Either b q -> t) -> p a b -> p s t
makePrism match inject = iso match inject . left'

-- type Lens s t a b = forall p. Strong p => p a b -> p s t
-- type Prism s t a b = forall p. Choice p => p a b -> p s t

instance Isomorphic (Bool -> a) (a, a) where
  from f = (f False, f True)
  to (x, y) = bool x y

powerTwo1 :: Lens' (Bool -> a) a
powerTwo1 = Lens { split = from :: (Bool -> a) -> (a, a), unsplit = to }

powerTwo2 :: Lens' (Bool -> a) a
powerTwo2 = Lens { split = swap . from :: (Bool -> a) -> (a, a), unsplit = to . swap }

(.&.) :: Lens' a b -> Lens' b c -> Lens' a c
Lens split1 unsplit1 .&. Lens split2 unsplit2 =
  Lens
  { split = \a -> let (b, q) = split1 a ; (c, r) = split2 b in (c, (q, r))
  , unsplit = \(c, (q, r)) -> unsplit1 (unsplit2 (c, r), q)
  }

(.|.) :: Prism' a b -> Prism' b c -> Prism' a c
Prism match1 inject1 .|. Prism match2 inject2 = Prism { match = match, inject = inject }
  where
    -- match :: a -> Either c (Either q r)
    match a =
      case match1 a of
        Left b -> case match2 b of
          Left c  -> Left c
          Right r -> Right (Right r)
        Right q -> Right (Left q)

    -- inject :: Either c (Either q r) -> a
    inject (Left c)          = inject1 $ Left $ inject2 $ Left c
    inject (Right (Left q))  = inject1 $ Right q
    inject (Right (Right r)) = inject1 $ Left $ inject2 $ Right r
