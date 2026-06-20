{-# OPTIONS_GHC -Wno-orphans #-}
module MonadComposition.Distributive where

import Data.Functor.Compose (Compose (Compose, getCompose))
import Control.Monad (join)
import Data.Distributive (Distributive (distribute))

instance (Monad m, Monad n, Distributive m) => Monad (Compose m n) where
  mn >>= k = Compose $ fmap join $ join $ fmap distribute $ getCompose $ fmap (getCompose . k) mn
