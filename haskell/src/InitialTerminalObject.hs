{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module InitialTerminalObject where

import Control.Arrow (Kleisli, arr)
import Control.Categorical.Object (HasInitialObject(Initial, initiate), HasTerminalObject(Terminal, terminate))
import Data.Void (Void, absurd)

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

type HasZeroObject a = (HasInitialObject a, HasTerminalObject a)
