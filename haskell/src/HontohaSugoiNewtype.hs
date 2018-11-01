-- | <https://speakerdeck.com/konn/ben-dang-hasugoi-newtype 本当はすごい newtype>
{-# LANGUAGE DerivingVia #-}
module HontohaSugoiNewtype where

import           Control.Arrow  ((&&&))
import           Data.Coerce    (coerce)
import           Data.Monoid    (Sum (Sum))
import           Data.Semigroup (Max (Max))

aggregate :: [Int] -> (Maybe Int, Int)
aggregate = coerce . foldMap (Just . Max &&& Sum)

newtype Id = MkId Word
  deriving (Semigroup, Monoid) via (Max Word)
