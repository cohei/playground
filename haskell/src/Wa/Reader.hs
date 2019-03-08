{-# OPTIONS_GHC -Wno-missing-methods #-}
module Wa.Reader (の, と, 和, 積) where

import Control.Monad.Reader (Reader, ask, runReader, local)
import Data.Function ((&))

newtype IntWithBiOp =
  IntWithBiOp (Reader (Int -> Int -> Int) Int)

instance Num IntWithBiOp where
  fromInteger = IntWithBiOp . pure . fromInteger

instance Show IntWithBiOp where
  show (IntWithBiOp m) = show $ runReader m const

と :: IntWithBiOp -> IntWithBiOp -> IntWithBiOp
と (IntWithBiOp m1) (IntWithBiOp m2) =
  IntWithBiOp $ do
    i1 <- m1
    i2 <- m2
    biop <- ask
    pure $ biop i1 i2

の :: IntWithBiOp -> (IntWithBiOp -> IntWithBiOp) -> IntWithBiOp
の = (&)

和 :: IntWithBiOp -> IntWithBiOp
和 (IntWithBiOp m) = IntWithBiOp $ local (const (+)) m

積 :: IntWithBiOp -> IntWithBiOp
積 (IntWithBiOp m) = IntWithBiOp $ local (const (*)) m
