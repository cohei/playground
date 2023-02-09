{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module LaxMonoidalFunctor where

import Control.Applicative (liftA2)
import Control.Lens (Iso', iso)
import Data.Bifunctor (Bifunctor)
import Data.Bifunctor.Assoc (Assoc)
import Data.Kind (Type, Constraint)
import Data.Void (Void, absurd)
import Prelude hiding (product, (**))

-- $setup
-- >>> import Control.Lens (view)
-- >>> import Data.Bifunctor (first, second)
-- >>> import Data.Bifunctor.Assoc (Assoc(assoc, unassoc))

class Bifunctor p => Unital p where
  type Unit p :: Type
  -- data Unit p :: Type

  leftUniter :: Iso' (Unit p `p` a) a
  rightUniter :: Iso' (a `p` Unit p) a

instance Unital (,) where
  type Unit (,) = ()
  -- data Unit (,) = ProductUnit ()

  leftUniter = iso snd ((),)
  rightUniter = iso fst (,())
  -- leftUniter = iso (\(ProductUnit (), a) -> a) (ProductUnit (),)
  -- rightUniter = iso (\(a, ProductUnit ()) -> a) (, ProductUnit ())

instance Unital Either where
  type Unit Either = Void
  -- data Unit Either = SumUnit Void

  leftUniter = iso (either absurd id) Right
  rightUniter = iso (either id absurd) Left

-- |
-- Monoid product of Hask category. It satisfies axioms below.
--
-- triangle identity:
--
-- prop> (second (view leftUniter) . assoc) object == first (view rightUniter) (object :: ((Int, ()), Char))
--
-- pentagon identity:
--
-- prop> (second assoc . assoc . first assoc) object == (assoc . assoc) (object :: (((Int, Char), Double), Bool))
type Monoidal :: (Type -> Type -> Type) -> Constraint
type Monoidal p = (Unital p, Assoc p)

class (Monoidal m, Functor f) => LaxMonoidalFunctor m f | f -> m where
  unit :: Unit m -> f (Unit m)
  product :: m (f a) (f b) -> f (m a b)

newtype Wrapped f a
  = Wrapped (f a)
  deriving (Functor)

instance Applicative f => LaxMonoidalFunctor (,) (Wrapped f) where
  unit () = Wrapped $ pure ()
  product (Wrapped f, Wrapped g) = Wrapped $ liftA2 (,) f g

{-
Identity      pure id <*> v = v
Composition   pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
Homomorphism  pure f <*> pure x = pure (f x)
Interchange   u <*> pure y = pure ($ y) <*> u

Fmap          fmap f x = pure f <*> x
-}
instance (LaxMonoidalFunctor (,) f) => Applicative (Wrapped f) where
  pure x = Wrapped $ x <$ unit ()
  Wrapped ff <*> Wrapped fx = Wrapped $ uncurry ($) <$> product (ff, fx)

-- Strong?
tensorStrength :: Functor f => a -> f b -> f (a, b)
tensorStrength = (<$>) . (,)
