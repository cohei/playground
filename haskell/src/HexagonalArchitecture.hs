{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}

-- | https://blog.tai2.net/hexagonal_architexture.html
module HexagonalArchitecture (main) where

import Data.Coerce (coerce)
import Data.Functor.Identity (Identity (Identity))

newtype Currency = Currency Double
  deriving (Num, Ord, Eq)

class Applicative f => DiscountRateRepository f where
  discountRate :: Currency -> f Double

type Discounter = forall f. DiscountRateRepository f => Currency -> f Currency

discount :: Discounter
discount amount = multiply amount <$> discountRate amount
  where
    multiply :: Currency -> Double -> Currency
    multiply c r = coerce $ coerce c * r

newtype MockDiscountRateRepository a = MockDiscountRateRepository a
  deriving (Functor, Show)
  deriving (Applicative) via Identity

instance DiscountRateRepository MockDiscountRateRepository where
  discountRate amount =
    pure $
      if
        | amount <=  100 -> 0.01
        | amount <= 1000 -> 0.02
        | otherwise      -> 0.05

test :: Discounter -> MockDiscountRateRepository Bool
test discounter = do
  d1 <- discounter 100
  d2 <- discounter 200
  pure $ d1 == 5 && d2 == 10

main :: IO ()
main = print $ test discount
