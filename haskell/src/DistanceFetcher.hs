{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module DistanceFetcher where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Numeric.Natural (Natural)
import System.Random (randomIO)

type WillSave = Bool

data Fetcher f k a
  = Fetcher
  { -- | whether upper layer can save this fetched value
    willSave :: WillSave,
    fetch :: k -> f (Maybe a),
    save :: k -> a -> f ()
  }

fetchToFetcher :: (Applicative f) => WillSave -> (k -> f (Maybe a)) -> Fetcher f k a
fetchToFetcher willSave fetch = Fetcher {willSave, fetch, save = \_ _ -> pure ()}

-- fallback に応じて willSave を変えるため f Bool が必要
type Get f k a = k -> f (WillSave, a)

compose :: forall m k a. (Monad m) => Fetcher m k a -> Get m k a -> Get m k a
compose Fetcher {fetch, willSave, save} fallback k =
  maybe (fallback k `tap` save') (pure . (willSave,)) =<< fetch k
  where
    save' :: (WillSave, a) -> m ()
    save' (willSave', a) = when willSave' $ save k a

tap :: (Monad m) => m a -> (a -> m b) -> m a
tap m k = m >>= \a -> a <$ k a

--
-- ドメイン領域
--

type Platform = String

type Duration = Natural

run :: (Cache m, MonadIO m) => (Platform, Platform) -> m Duration
run = fmap snd . foldr compose hubenyDistanceGet fetchers

{-
run = fmap snd . appEndo (foldMap (Endo . compose) fetchers) hubenyDistanceGet

foldr (f :: a -> b -> b) z xs == appEndo (foldMap (Endo . f :: a -> Endo b) xs) z なので
-}

fetchers :: (MonadIO m, Cache m) => [Fetcher m (Platform, Platform) Duration]
fetchers =
  [ samePlatformFetcher,
    redisFetcher,
    databaseFetcher,
    googleMapsFetcher
  ]

samePlatformFetcher :: (Applicative f) => Fetcher f (Platform, Platform) Duration
samePlatformFetcher = fetchToFetcher True $ pure . uncurry samePlatformFetch

samePlatformFetch :: Platform -> Platform -> Maybe Duration
samePlatformFetch p1 p2 = if p1 == p2 then Just 0 else Nothing

class (Monad m) => Cache m where
  cacheFetch :: key -> m (Maybe a)
  cacheSave :: key -> a -> m ()

redisFetcher :: (Cache m) => Fetcher m a b
redisFetcher = cacheFetcher

databaseFetcher :: (Cache m) => Fetcher m a b
databaseFetcher = cacheFetcher

cacheFetcher :: (Cache m) => Fetcher m a b
cacheFetcher = Fetcher {willSave = True, fetch = cacheFetch, save = cacheSave}

googleMapsFetcher :: (MonadIO m) => Fetcher m k Duration
googleMapsFetcher = fetchToFetcher True $ liftIO . googleMapsFetch

-- キャッシュ要
googleMapsFetch :: k -> IO (Maybe Duration)
googleMapsFetch _ = (\b -> if b then Just 10 else Nothing) <$> randomIO

hubenyDistanceGet :: (Applicative f) => Get f (Platform, Platform) Duration
hubenyDistanceGet = pure . (False,) . uncurry hubenyCalculator

hubenyCalculator :: Platform -> Platform -> Duration
hubenyCalculator p1 p2 = fromIntegral $ length p1 + length p2
