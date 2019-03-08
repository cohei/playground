{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Wa.Monoid (の, と, 和, 積) where

import Data.Function ((&))

newtype MonoidInt =
  MonoidInt [Int]
  deriving (Semigroup)

instance Num MonoidInt where
  fromInteger = MonoidInt . pure . fromInteger

instance Show MonoidInt where
  show (MonoidInt [i]) = show i
  show _ = "not single value"

と :: MonoidInt -> MonoidInt -> MonoidInt
と = (<>)

の :: MonoidInt -> (MonoidInt -> MonoidInt) -> MonoidInt
の = (&)

和 :: MonoidInt -> MonoidInt
和 (MonoidInt is) = MonoidInt [sum is]

積 :: MonoidInt -> MonoidInt
積 (MonoidInt is) = MonoidInt [product is]
