{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving        #-}
module DayConvolution where

import           Control.Applicative (liftA2)

data Day f g a = forall x y. Day (f x) (g y) (x -> y -> a)

deriving instance Functor (Day f g)

instance (Applicative f, Applicative g) => Applicative (Day f g) where
  pure x = Day (pure ()) (pure ()) (\() () -> x)
  Day f1 g1 h1 <*> Day f2 g2 h2 =
    Day (liftA2 (,) f1 f2) (liftA2 (,) g1 g2) (\(x1, x2) (y1, y2) -> h1 x1 y1 $ h2 x2 y2)
