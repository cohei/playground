{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | https://hackage.haskell.org/package/categories
module Categories where

import Control.Arrow (Kleisli, arr)

import qualified Control.Categorical.Functor as C (Functor())
import Control.Categorical.Object (HasInitialObject(Initial, initiate), HasTerminalObject(Terminal, terminate))
import Control.Category (Category((.)))
import Control.Category.Monoidal (Monoidal (Id))
import Data.Functor.Identity ()
import Data.Void (Void, absurd)

import Prelude (Monad, const, type (~))

instance HasInitialObject (->) where
  type Initial (->) = Void
  initiate = absurd

instance HasTerminalObject (->) where
  type Terminal (->) = ()
  terminate = const ()

instance Monad m => HasInitialObject (Kleisli m) where
  type Initial (Kleisli m) = Void
  initiate = arr absurd

instance Monad m => HasTerminalObject (Kleisli m) where
  type Terminal (Kleisli m) = ()
  terminate = arr (const ())

-- ほんとは始対象と終対象は isomorphic だけでいい
type HasZeroObject cat = (HasInitialObject cat, HasTerminalObject cat, Initial cat ~ Terminal cat)

zeroMorphism :: HasZeroObject cat => cat a b
zeroMorphism = initiate . terminate

-- | https://ncatlab.org/nlab/show/pointed+category
class HasTerminalObject cat => Pointed cat where
  basepoint :: cat (Terminal cat) a

newtype W cat a b = W (cat a b)
  deriving (Category, HasTerminalObject, Pointed)

instance Pointed cat => HasInitialObject (W cat) where
  type Initial (W cat) = Terminal cat
  initiate = basepoint

class (C.Functor f r t, Monoidal r p, Monoidal t q) => LaxMonoidalFunctor f r p t q | r -> p, t -> q where
  laxPlus :: t (q (f a) (f b)) (f (p a b))
  laxUnit :: t (Id t q) (f (Id r p))

class (C.Functor f r t, Monoidal r p, Monoidal t q) => ColaxMonoidalFunctor f r p t q | p -> r, q  -> t where
  colax :: p (f a) (f b) -> f (q a b)

-- class (LaxMonoidalFunctor f r p t q, ColaxMonoidalFunctor f) => StrongMonoidalFunctor f where
