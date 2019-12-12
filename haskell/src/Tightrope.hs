-- | すごいH本から
{-# LANGUAGE FlexibleContexts #-}
module Tightrope where

import           Control.Applicative (Alternative (empty))
import           Control.Monad       (void)
import           Control.Monad.State (MonadState (get), execStateT, modify)
import           Data.Bifunctor      (Bifunctor (first, second))

type Birds = Int
type Pole = (Birds, Birds)

test :: Maybe Pole
test = flip execStateT (0, 0) $ do
  landLeft 1
  landRight 4
  -- banana
  landLeft (-1)
  landRight (-2)

landLeft :: (Alternative m, MonadState Pole m) => Birds -> m ()
landLeft n = void $ do
  modify $ first (+ n)
  newPole <- get
  guarded isStable newPole

landRight :: (Alternative m, MonadState Pole m) => Birds -> m ()
landRight n = void $ do
  modify $ second (+ n)
  newPole <- get
  guarded isStable newPole

isStable :: Pole -> Bool
isStable (l, r) = abs (l - r) < 4

banana :: Alternative f => f ()
banana = empty

guarded :: Alternative f => (a -> Bool) -> a -> f a
guarded p x = if p x then pure x else empty
