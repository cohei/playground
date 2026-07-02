{-# OPTIONS_GHC -Wno-orphans #-}

module MonadComposition.Distributive where

import Control.Monad (join)
import Data.Distributive (Distributive (distribute))
import Data.Functor.Compose (Compose (Compose, getCompose))

instance (Monad m, Monad n, Distributive m) => Monad (Compose m n) where
  mn >>= k = Compose $ fmap join $ join $ fmap distribute $ getCompose $ fmap (getCompose . k) mn
