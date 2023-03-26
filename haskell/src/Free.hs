{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module Free where

import Control.Applicative (Const(Const))
import Control.Comonad (Comonad(extract, duplicate))
import Control.Monad (replicateM_)
import Control.Monad.Trans (MonadTrans(lift))
import Data.Functor.Identity (Identity(runIdentity, Identity))
import Data.Kind (Type)
import Data.Void (Void, absurd)

data Free f a = Pure a | Wrap (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure $ f a
  fmap f (Wrap ffree) = Wrap $ fmap (fmap f) ffree

instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure f <*> x = fmap f x
  Wrap f <*> x = Wrap $ fmap (<*> x) f

instance Functor f => Monad (Free f) where
  Pure x >>= k = k x
  Wrap f >>= k = Wrap $ fmap (>>= k) f

instance MonadTrans Free where
  lift = Wrap . fmap Pure


data Cofree f a = a :> f (Cofree f a)

unwrap :: Cofree f a -> f (Cofree f a)
unwrap (_ :> f) = f

instance Functor f => Functor (Cofree f) where
  fmap f (x :> xs) = f x :> fmap (fmap f) xs

instance Functor f => Comonad (Cofree f) where
  extract (x :> _) = x
  duplicate w@(_ :> f) = w :> fmap duplicate f


-- Church encoding
newtype F f a = F { runF :: forall r. (a -> r) -> (f r -> r) -> r }

instance Functor (F f) where
  fmap f (F g) = F $ g . (. f)

instance Applicative (F f) where
  pure a = F $ \pure' _ -> pure' a
  F f <*> F x = F $ \pure' wrap -> f (\ab -> x (pure' . ab) wrap) wrap

instance Monad (F f) where
  m >>= f = F $ \pure' wrap -> runF m (\a -> runF (f a) pure' wrap) wrap


-- Free (Const Void) a =~ a

freeConstVoidToValue :: Free (Const Void) a -> a
freeConstVoidToValue (Pure x) = x
freeConstVoidToValue (Wrap (Const void)) = absurd void

valueToFreeConstVoid :: a -> Free (Const Void) a
valueToFreeConstVoid = pure


-- Free (Const ()) a =~ Maybe a

freeConstUnitToMaybe :: Free (Const ()) a -> Maybe a
freeConstUnitToMaybe (Pure x) = Just x
freeConstUnitToMaybe (Wrap (Const ())) = Nothing

maybeToFreeConstUnit :: Maybe a -> Free (Const ()) a
maybeToFreeConstUnit Nothing = Wrap $ Const ()
maybeToFreeConstUnit (Just x) = Pure x


-- Free Identity =~ Delayed

data Delayed a = Now a | Later (Delayed a)

freeIdentityToDelayed :: Free Identity a -> Delayed a
freeIdentityToDelayed (Pure x) = Now x
freeIdentityToDelayed (Wrap f) = Later $ freeIdentityToDelayed $ runIdentity f

delayedToFreeIdentity :: Delayed a -> Free Identity a
delayedToFreeIdentity (Now x) = Pure x
delayedToFreeIdentity (Later delayed) = Wrap $ Identity $ delayedToFreeIdentity delayed


data ProcessF :: Type -> Type where
  Atomically :: IO a -> (a -> r) -> ProcessF r
  Fork       :: Process () -> r -> ProcessF r

instance Functor ProcessF where
  fmap f (Atomically m k) = Atomically m $ f . k
  fmap f (Fork p1 p2)     = Fork p1 $ f p2

type Process = Free ProcessF

atomically :: IO a -> Process a
atomically m = Wrap $ Atomically m Pure

fork :: Process () -> Process ()
fork p = Wrap $ Fork p (Pure ())

schedule :: [Process ()] -> IO ()
schedule []                           = return ()
schedule (Pure _                : ps) = schedule ps
schedule (Wrap (Atomically m k) : ps) = m >>= \x -> schedule (ps ++ [k x])
schedule (Wrap (Fork p1 p2)     : ps) = schedule $ ps ++ [p1, p2]

example :: Process ()
example = do
  fork $ replicateM_ 5 $ atomically $ putStrLn "Haskell"
  fork $ replicateM_ 6 $ atomically $ putStrLn "eXchange"
  atomically $ putStrLn "2013"
