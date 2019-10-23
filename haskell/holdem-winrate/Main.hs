{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns               #-}
module Main where

import           Data.Bifunctor (first)
import           Data.Foldable  (toList)
import           Data.List      (group)
import           Data.Set       (Set, (\\))
import qualified Data.Set       as S (delete, deleteFindMin, empty, filter,
                                      findMax, findMin, fromList, insert, map,
                                      minView, null, singleton, size)
import           Test.Hspec     (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "splitAce" $
    it "split ace" $
      splitAce (makeCard5 (spade 1) (spade 2) (spade 3) (spade 4) (spade 5)) `shouldBe` [makeCard5 (spade 1) (spade 2) (spade 3) (spade 4) (spade 5), makeCard5 (spade 14) (spade 2) (spade 3) (spade 4) (spade 5)]

  describe "hand" $ do
    it "checks straight flush, 5" $
      hand (makeCard5 (spade 1) (spade 2) (spade 3) (spade 4) (spade 5)) `shouldBe` StraightFlush
    it "checks straight flush, Ace" $
      hand (makeCard5 (spade 1) (spade 13) (spade 12) (spade 11) (spade 10)) `shouldBe` StraightFlush
    it "checks full house" $
      hand (makeCard5 (spade 1) (heart 1) (club 1) (club 4) (spade 4)) `shouldBe` FullHouse
    it "checks high card" $
      hand (makeCard5 (spade 1) (heart 9) (club 8) (diamond 4) (spade 5)) `shouldBe` HighCard

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

newtype Card5 =
  Card5 (Set Card)
  deriving (Eq, Show)

makeCard5 :: Card -> Card -> Card -> Card -> Card -> Card5
makeCard5 c1 c2 c3 c4 c5 =
  Card5 $ S.fromList [c1, c2, c3, c4, c5]

-- 役を構成する部分の強弱を比べた上で残りのカードの強弱を比べる
-- フルハウスは 3枚のほうから見る
instance Ord Card5 where
  compare cs1 cs2 = undefined

data Component =
  Component { hand_ :: Hand, handCards :: [Card], restCards :: [Card] }

splitAce :: Card5 -> [Card5]
splitAce (Card5 cards) = map (\[c1,c2,c3,c4,c5] -> makeCard5 c1 c2 c3 c4 c5) $ mapM split $ toList cards
  where
    split :: Card -> [Card]
    split (Card 1 suit) = [Card 1 suit, Card 14 suit]
    split c             = [c]

hand :: Card5 -> Hand
hand cards = maximum $ map handSingle $ splitAce cards

handSingle :: Card5 -> Hand
handSingle cards
  | isStraight cards && isFlush cards = StraightFlush
  | isFourOfAKind cards               = FourOfAKind
  | isFullHouse cards                 = FullHouse
  | isFlush cards                     = Flush
  | isStraight cards                  = Straight
  | isThreeOfAKind cards              = ThreeOfAKind
  | isTwoPair cards                   = TwoPair
  | isOnePair cards                   = OnePair
  | otherwise                         = HighCard

data Part =
    Kicker Rank
  | Pair Rank
  | ThreeOfAKind' Rank
  | FourOfAKind' Rank
  | Straight' Rank -- 一番大きいランク
  | Flush'
  deriving (Eq, Ord)

data Hand' =
    HighCard'' (Set Rank)
  | OnePair'' Rank (Set Rank)
  | TwoPair'' Rank Rank (Set Rank)
  | ThreeOfAKind'' Rank (Set Rank)
  | Straight'' Rank
  | Flush'' (Set Rank)
  | FullHouse'' Rank Rank
  | FourOfAKind'' Rank Rank
  | StraightFlush'' Rank

-- Set Part -> Hand'
-- Hand' は二次的なもの

-- 7 -> 5
-- 強弱の判定に必要な情報が欲しい
-- 役、最大ランク、ほかのカード = Set Part

--                                  役       残り
parserStraight :: Set Card -> Set (Part, Set Card)
parserStraight cards =
  S.map (\card5 -> (Straight' $ S.findMax $ S.map rank card5, cards \\ card5)) $
  S.filter (isSequence . S.map rank) $
  pick 5 cards

-- isStraight = not . null . fst . parserStraight

pick :: Ord a => Int -> Set a -> Set (Set a)
pick 0 xs = S.singleton S.empty
pick _ xs | S.null xs = S.empty
pick n (S.deleteFindMin -> (x, xs)) = S.map (S.insert x) (pick (n - 1) xs) <> pick n xs

isStraight :: Card5 -> Bool
isStraight (Card5 cards) = isSequence $ S.map rank cards

isFlush :: Card5 -> Bool
isFlush (Card5 cards) = S.size (S.map suit cards) == 1

parserFlush :: Set Card -> Set (Part, Set Card)
parserFlush cards =
  S.map (\card5 -> (Flush', cards \\ card5)) $
  S.filter (\card5 -> S.size (S.map suit card5) == 1) $
  pick 5 cards

isFourOfAKind :: Card5 -> Bool
isFourOfAKind (Card5 cards) =
  any ((== 4) . length) $ group $ map rank $ toList cards

isFullHouse :: Card5 -> Bool
isFullHouse (Card5 cards) =
  let
    ranks = group $ map rank $ toList cards
  in
    any ((== 3) . length) ranks && any ((== 2) . length) ranks

isThreeOfAKind :: Card5 -> Bool
isThreeOfAKind (Card5 cards) =
  any ((== 3) . length) $ group $ map rank $ toList cards

isTwoPair :: Card5 -> Bool
isTwoPair (Card5 cards) =
  (== 2) $ length $ filter ((== 2) . length) $ group $ map rank $ toList cards

isOnePair :: Card5 -> Bool
isOnePair (Card5 cards) =
  any ((== 2) . length) $ group $ map rank $ toList cards

isSequence :: (Foldable f, Eq a, Enum a) => f a -> Bool
isSequence xs =
  let
    xs'@(x:_) = toList xs
  in
    take (length xs) [x..] == xs'
