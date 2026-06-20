{-# OPTIONS_GHC -Wno-orphans #-}
module MonadComposition.Traversable where

import Data.Functor.Compose (Compose (Compose, getCompose))
import Control.Monad (join)

instance (Monad m, Monad n, Traversable n) => Monad (Compose m n) where
  mn >>= k = Compose $ fmap join $ join $ fmap sequenceA $ getCompose $ fmap (getCompose . k) mn
