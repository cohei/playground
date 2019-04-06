{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
-- | [I love profunctors. They're so easy.](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/profunctors)
module Profunctor where

import           Data.Bifunctor  (bimap)
import           Data.Function   (on)
import           Data.Map        (Map)
import qualified Data.Map        as M (mapWithKey)
import           Data.Maybe      (fromMaybe)
import           Data.Profunctor (Profunctor (dimap))

data Limits a b =
  Limits
  { step  :: a -> (b, b)
  , check :: a -> a -> Bool
  }

instance Profunctor Limits where
  dimap f g Limits {..} = Limits { step = bimap g g . step . f, check = check `on` f }

type Limits' a = Limits a a

maybeLimit :: a -> Limits' a -> Limits' (Maybe a)
maybeLimit def = dimap (fromMaybe def) pure

millionsLimit :: Limits' Double -> Limits' Double
millionsLimit = dimap (* 1e6) (/ 1e6)

newtype Indexed i a b = Indexed { runIndexed :: i -> a -> b }

instance Profunctor (Indexed i) where
  dimap f g = Indexed . (dimap f g .) . runIndexed

class Indexable i p where
  indexed :: p a b -> i -> a -> b

instance Indexable i (->) where
  indexed = const

instance Indexable i (Indexed i) where
  indexed = runIndexed

mapIndexable :: Indexable k p => p a b -> Map k a -> Map k b
mapIndexable = M.mapWithKey . indexed
