{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE DefaultSignatures #-}
module MonadWithDefault where

import           Prelude ((.))

class Functor f where
  fmap :: (a -> b) -> f a -> f b
  default fmap :: Applicative f => (a -> b) -> f a -> f b
  fmap = (<*>) . pure

class Functor f => Applicative f where
  pure :: a -> f a
  default pure :: Monad f => a -> f a
  pure = return

  (<*>) :: f (a -> b) -> f a -> f b
  default (<*>) :: Monad f => f (a -> b) -> f a -> f b
  mf <*> mx = mf >>= \f -> mx >>= \x -> pure (f x)

class Applicative m => Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

newtype Identity a = Identity { runIdentity :: a }

instance Monad Identity where
  return = Identity
  m >>= f = f (runIdentity m)

instance Applicative Identity
instance Functor Identity
