{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Applicative2 where

import           Control.Applicative (liftA2)
import           Prelude             hiding ((**))

{-
Identity      pure id <*> v = v
Composition   pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
Homomorphism  pure f <*> pure x = pure (f x)
Interchange   u <*> pure y = pure ($ y) <*> u

Fmap          fmap f x = pure f <*> x
-}

class Functor f => Applicative2 f where
    unit :: f ()
    (**) :: f a -> f b -> f (a, b)

newtype Wrapped f a =
  Wrapped (f a)
  deriving (Functor)

instance (Applicative f) => Applicative2 (Wrapped f) where
  unit                   = Wrapped $ pure ()
  Wrapped f ** Wrapped g = Wrapped $ liftA2 (,) f g

instance (Applicative2 f) => Applicative (Wrapped f) where
  pure x                    = Wrapped $ const x <$> unit
  Wrapped ff <*> Wrapped fx = Wrapped $ (uncurry ($)) <$> ff ** fx

strength :: Functor f => a -> f b -> f (a, b)
strength = (<$>) . (,)
