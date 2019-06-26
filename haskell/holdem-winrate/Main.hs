module Main where

import           Data.List (group)

main :: IO ()
main = putStrLn "Hello, Haskell!"

data Suit = Spade | Heart | Club | Diamond
  deriving (Eq)

data Card =
  Card { suit :: Suit, number :: Int }
  deriving (Eq)

data Hand =
    HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | Straight
  | Flush
  | FullHouse
  | FourOfAKind
  | StraightFlush
  deriving (Eq, Ord)

-- 5枚、小さい順
newtype Card5 = Card5 [Card]

-- instance Ord Hand where
--   compare hand1 hand2 = _

hand :: Card5 -> Hand
hand cards
  | isStraight cards && isFlush cards = StraightFlush
  | isFourOfAKind cards               = FourOfAKind
  | isFullHouse cards                 = FullHouse
  | isFlush cards                     = Flush
  | isStraight cards                  = Straight
  | isThreeOfAKind cards              = ThreeOfAKind
  | isTwoPair cards                   = TwoPair
  | isOnePair cards                   = OnePair
  | otherwise                         = HighCard

isStraight :: Card5 -> Bool
isStraight (Card5 cards) = isSequence $ map number cards

isFlush :: Card5 -> Bool
isFlush (Card5 cards) = allSame $ map suit cards

isFourOfAKind :: Card5 -> Bool
isFourOfAKind (Card5 cards) =
  any ((== 4) . length) $ group $ map number cards

isFullHouse :: Card5 -> Bool
isFullHouse (Card5 cards) =
  let
    numbers = group $ map number cards
  in
    any ((== 3) . length) numbers && any ((== 2) . length) numbers

isThreeOfAKind :: Card5 -> Bool
isThreeOfAKind (Card5 cards) =
  any ((== 3) . length) $ group $ map number cards

isTwoPair :: Card5 -> Bool
isTwoPair (Card5 cards) =
  (== 2) $ length $ filter ((== 2) . length) $ group $ map number cards

isOnePair :: Card5 -> Bool
isOnePair (Card5 cards) =
  any ((== 2) . length) $ group $ map number cards

allSame :: Eq a => [a] -> Bool
allSame []     = True
allSame (x:xs) = all (x ==) xs

isSequence :: (Eq a, Num a) => [a] -> Bool
isSequence xs = all (1 ==) $ zipWith (-) (tail xs) xs
