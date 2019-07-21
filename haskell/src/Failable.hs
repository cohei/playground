-- https://github.com/matsubara0507/fallible
module Failable where

import           Control.Monad.Trans.Cont (ContT, withContT)

fromMaybeWith :: m r -> ContT r m (Maybe a) -> ContT r m a
fromMaybeWith = withContT . maybe

fromEitherWith :: (e -> m r) -> ContT r m (Either e a) -> ContT r m a
fromEitherWith = withContT . either
