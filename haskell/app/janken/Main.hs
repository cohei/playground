{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Arrow        ((&&&))
import           Control.Monad.Random (MonadRandom (getRandoms),
                                       Random (random, randomR))
import           Data.Bifunctor       (first)
import           Data.List            (group, sort)

main :: IO ()
main = do
  print . take 10 =<< jankens
  print . statistics . take 1000 =<< jankens

statistics :: Ord a => [a] -> [(a, Int)]
statistics = map (head &&& length) . group . sort

data Hand = G | C | P
          deriving (Show, Eq)

instance Ord Hand where
  compare h1 h2 | h1 == h2 = EQ
  compare G C   = GT
  compare C P   = GT
  compare P G   = GT
  compare _ _   = LT

instance Random Hand where
  randomR = undefined
  random = first hand . randomR (0, 2)
    where
      hand :: Int -> Hand
      hand = \case
        0 -> G
        1 -> C
        2 -> P
        _ -> error "only 0 to 2"

player :: MonadRandom m => m [Hand]
player = getRandoms

jankens :: (MonadRandom m) => m [Ordering]
jankens = zipWith compare <$> player <*> player
