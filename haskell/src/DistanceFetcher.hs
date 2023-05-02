{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module DistanceFetcher where

import Control.Arrow (Kleisli(Kleisli, runKleisli))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Default.Class ( Default(def) )
import Data.Maybe (fromMaybe)
import Numeric.Natural (Natural)
import System.Random (randomIO)

type WillPut = Bool

data Fetcher f k a =
  Fetcher
  { get :: k -> f (Maybe a)
  , willPut :: WillPut
  , put :: k -> a -> f ()
  }

instance Applicative f => Default (Fetcher f k a) where
  def =
    Fetcher
    { get = \_ -> pure Nothing
    , willPut = False
    , put = \_ _ -> pure ()
    }

-- fallback に応じて willPut を変えるため f Bool が必要
type Distancer f k a = k -> f (WillPut, a)

type Fetch f k a = Distancer f k a -> Distancer f k a

compose :: forall m k a. Monad m => Fetcher m k a -> Fetch m k a
compose Fetcher{get, willPut, put} fallback =
  unKl $ flip fromMaybe <$> fmap2 (willPut,) (kl get) <*> kl (tap <$> fallback <*> cache)
  where
    cache :: k -> (Bool, a) -> m ()
    cache k (willPut', a) = when willPut' (put k a)

tap :: Monad m => m a -> (a -> m b) -> m a
tap m k = m >>= k >> m

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap

kl :: (a -> m b) -> Kleisli m a b
kl = Kleisli

unKl :: Kleisli m a b -> a -> m b
unKl = runKleisli

--
-- ドメイン領域
--

type Platform = String
type Duration = Natural

run :: (Cache m, MonadIO m) => (Platform, Platform) -> m Duration
run = fmap snd . foldr compose hubenyDistancer fetchers
{-
run = fmap snd . appEndo (foldMap (Endo . compose) fetchers) hubenyDistancer

foldr (f :: a -> b -> b) z xs == appEndo (foldMap (Endo . f :: a -> Endo b) xs) z なので
-}

fetchers :: (MonadIO m, Cache m) => [Fetcher m (Platform, Platform) Duration]
fetchers =
  [ samePlatformFetcher
  , redisFetcher
  , databaseFetcher
  , googleMapsFetcher
  ]

samePlatformFetcher :: Applicative f => Fetcher f (Platform, Platform) Duration
samePlatformFetcher = def { get = pure . samePlatformGet }

samePlatformGet :: (Platform, Platform) -> Maybe Duration
samePlatformGet (p1, p2) = if p1 == p2 then Just 0 else Nothing

class Monad m => Cache m where
  cacheGet :: key -> m (Maybe a)
  cachePut :: key -> a -> m ()

redisFetcher :: Cache f => Fetcher f a b
redisFetcher = Fetcher { willPut = True, get = cacheGet, put = cachePut }

databaseFetcher :: Cache f => Fetcher f a b
databaseFetcher = Fetcher { willPut = True, get = cacheGet, put = cachePut }

googleMapsFetcher :: MonadIO m => Fetcher m k Duration
googleMapsFetcher = def { willPut = True, get = liftIO . googleMapsGet }

-- キャッシュ要
googleMapsGet :: k -> IO (Maybe Duration)
googleMapsGet _ = do
  b <- randomIO
  pure $ if b then Just 10 else Nothing

hubenyDistancer :: Applicative f => Distancer f (Platform, Platform) Duration
hubenyDistancer = pure . (False,) . hubenyCalculator

hubenyCalculator :: (Platform, Platform) -> Duration
hubenyCalculator (p1, p2) = fromIntegral $ length p1 + length p2
