{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module VectorApplicative where

data Nat = Z | S Nat

data Vector (n :: Nat) a where
  Nil  :: Vector Z a
  Cons :: a -> Vector n a -> Vector (S n) a

instance Show a => Show (Vector n a) where
  show Nil = "Nil"
  show (Cons x xs) = "Cons " ++ show x ++ " (" ++ show xs ++ ")"

instance Functor (Vector n) where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative (Vector Z) where
  pure _ = Nil
  _ <*> _ = Nil

instance Applicative (Vector n) => Applicative (Vector (S n)) where
  pure x = Cons x (pure x)
  Cons f fs <*> Cons x xs = Cons (f x) (fs <*> xs)

-- (+) <$> pure 1 <*> Cons 1 (Cons 2 (Cons 3 Nil))
-- >>> Cons 2 (Cons 3 (Cons 4 (Nil)))
