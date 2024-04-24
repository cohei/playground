{-# LANGUAGE TypeFamilies #-}

module DurationFetcher where

import Control.Monad (MonadPlus (mplus))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Identity (Identity (Identity, runIdentity))
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Numeric.Natural (Natural)
import System.Random (randomIO)

tap :: (Monad m) => m a -> (a -> m b) -> m a
tap m k = m >>= \a -> a <$ k a

type (<+>) :: Type -> Type -> Type
type family m <+> n where
  Maybe a <+> Maybe a = Maybe a
  Maybe a <+> Identity a = Identity a
  Identity a <+> Maybe a = Identity a
  Identity a <+> Identity a = Identity a

class MaybeAdd m n where
  (<+>) :: m -> n -> m <+> n

instance MaybeAdd (Maybe a) (Maybe a) where
  (<+>) = mplus

instance MaybeAdd (Maybe a) (Identity a) where
  (<+>) = fmap . flip fromMaybe

instance MaybeAdd (Identity a) (Maybe a) where
  (<+>) = const

instance MaybeAdd (Identity a) (Identity a) where
  (<+>) = const

class (Monad m) => Cache m where
  cacheFetch :: key -> m (Maybe a)
  cacheSave :: key -> a -> m ()

cache :: (Cache m) => (k -> m a) -> k -> m a
cache f k = maybe (f k `tap` cacheSave k) pure =<< cacheFetch k

cache2 :: (Cache m) => (k1 -> k2 -> m a) -> k1 -> k2 -> m a
cache2 = curry . cache . uncurry

--
-- ドメイン領域
--

data Platform
  deriving (Eq)

type Duration = Natural

duration :: (MonadIO m, Cache m) => Platform -> Platform -> m Duration
duration = (fmap runIdentity .) . (samePlatform' <++> (redisCache . databaseCache) googleMaps' <++> hubeny')
  where
    (<++>) :: (Applicative f, MaybeAdd m n) => (a -> b -> f m) -> (a -> b -> f n) -> a -> b -> f (m <+> n)
    (<++>) = (liftA2 . liftA2 . liftA2) (<+>)

    samePlatform' :: (Applicative f) => Platform -> Platform -> f (Maybe Duration)
    samePlatform' = (pure .) . samePlatform

    googleMaps' :: (MonadIO m) => Platform -> Platform -> m (Maybe Duration)
    googleMaps' = (liftIO .) . googleMaps

    hubeny' :: (Applicative f) => Platform -> Platform -> f (Identity Duration)
    hubeny' = (pure .) . (Identity .) . hubeny

samePlatform :: Platform -> Platform -> Maybe Duration
samePlatform p1 p2 = if p1 == p2 then Just 0 else Nothing

redisCache, databaseCache :: (Cache m) => (k1 -> k2 -> m a) -> k1 -> k2 -> m a
redisCache = cache2
databaseCache = cache2

googleMaps :: Platform -> Platform -> IO (Maybe Duration)
googleMaps _ _ = (\b -> if b then Just 10 else Nothing) <$> randomIO

hubeny :: Platform -> Platform -> Duration
hubeny _ _ = 1
