{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import           Data.List  (group, sort)
import           Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "splitAce" $
    it "split ace" $
      splitAce (makeCard5 (spade 1) (spade 2) (spade 3) (spade 4) (spade 5)) `shouldBe` [makeCard5 (spade 1) (spade 2) (spade 3) (spade 4) (spade 5), makeCard5 (spade 14) (spade 2) (spade 3) (spade 4) (spade 5)]

  describe "hand" $ do
    it "checks straight flush, 5" $ do
      hand' (makeCard5 (spade 1) (spade 2) (spade 3) (spade 4) (spade 5)) `shouldBe` StraightFlush
    it "checks straight flush, Ace" $ do
      hand' (makeCard5 (spade 1) (spade 13) (spade 12) (spade 11) (spade 10)) `shouldBe` StraightFlush
    it "checks full house" $ do
      hand' (makeCard5 (spade 1) (heart 1) (club 1) (club 4) (spade 4)) `shouldBe` FullHouse
    it "checks high card" $ do
      hand' (makeCard5 (spade 1) (heart 9) (club 8) (diamond 4) (spade 5)) `shouldBe` HighCard

data Suit = Club | Diamond | Heart | Spade
  deriving (Eq, Ord, Show)

newtype Rank =
  Rank Int
  deriving (Eq, Ord, Enum, Show)

instance Num Rank where
  fromInteger n
    | 1 <= n && n <= 14 = Rank $ fromIntegral n
    | otherwise         = error "Invalid number"
  (+) = undefined
  (*) = undefined
  (-) = undefined
  abs = undefined
  signum = undefined

data Card =
  Card { rank :: Rank, suit :: Suit }
  deriving (Eq, Ord)

instance Show Card where
  show (Card (Rank rank) suit) = show suit ++ " # " ++ show rank

spade, heart, club, diamond :: Rank -> Card
spade   n = Card n Spade
heart   n = Card n Heart
club    n = Card n Club
diamond n = Card n Diamond

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
  deriving (Eq, Ord, Show)

-- 5枚、一意にするため、数字、次にスートの小さい順
newtype Card5 =
  Card5 [Card]
  deriving (Eq, Show)

makeCard5 :: Card -> Card -> Card -> Card -> Card -> Card5
makeCard5 c1 c2 c3 c4 c5 = Card5 $ sort [c1, c2, c3, c4, c5]

-- 役を構成する部分の強弱を比べた上で残りのカードの強弱を比べる
-- フルハウスは 3枚のほうから見る
instance Ord Card5 where
  compare cs1 cs2 = undefined

data Component =
  Component { hand_ :: Hand, handCards :: [Card], restCards :: [Card] }

splitAce :: Card5 -> [Card5]
splitAce (Card5 cards) = map (\[c1,c2,c3,c4,c5] -> makeCard5 c1 c2 c3 c4 c5) $ mapM split cards
  where
    split :: Card -> [Card]
    split (Card 1 suit) = [Card 1 suit, Card 14 suit]
    split c             = [c]

hand' :: Card5 -> Hand
hand' cards = maximum $ map hand $ splitAce cards

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

--                         役       残り
parserStraight :: [Card] -> ([Card], [Card])
parserStraight cards = undefined

-- isStraight = not . null . fst . parserStraight

isStraight :: Card5 -> Bool
isStraight (Card5 cards) = isSequence $ map rank cards

isFlush :: Card5 -> Bool
isFlush (Card5 cards) = allSame $ map suit cards

isFourOfAKind :: Card5 -> Bool
isFourOfAKind (Card5 cards) =
  any ((== 4) . length) $ group $ map rank cards

isFullHouse :: Card5 -> Bool
isFullHouse (Card5 cards) =
  let
    ranks = group $ map rank cards
  in
    any ((== 3) . length) ranks && any ((== 2) . length) ranks

isThreeOfAKind :: Card5 -> Bool
isThreeOfAKind (Card5 cards) =
  any ((== 3) . length) $ group $ map rank cards

isTwoPair :: Card5 -> Bool
isTwoPair (Card5 cards) =
  (== 2) $ length $ filter ((== 2) . length) $ group $ map rank cards

isOnePair :: Card5 -> Bool
isOnePair (Card5 cards) =
  any ((== 2) . length) $ group $ map rank cards

allSame :: Eq a => [a] -> Bool
allSame []     = True
allSame (x:xs) = all (x ==) xs

isSequence :: (Eq a, Enum a) => [a] -> Bool
isSequence xs@(x:_) = take (length xs) [x..] == xs
