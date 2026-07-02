{-# OPTIONS_GHC -Wno-orphans #-}

module MonadComposition.Traversable where

import Control.Monad (join)
import Data.Functor.Compose (Compose (Compose, getCompose))

instance (Monad m, Monad n, Traversable n) => Monad (Compose m n) where
  mn >>= k = Compose $ fmap join $ join $ fmap sequenceA $ getCompose $ fmap (getCompose . k) mn
