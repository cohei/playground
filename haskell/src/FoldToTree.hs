{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | https://twitter.com/gakuzzzz/status/1392783408951033857
module FoldToTree (Item (Item, level, text), Chapter, withBuilder, withFoldl) where

import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Maybe (fromMaybe)
import Data.Tree (Forest, Tree (Node, rootLabel, subForest))

data Item = Item {level :: Int, text :: String}

type Chapter = Tree String

newtype ChapterBuilder a = ChapterBuilder {toForest :: Forest (Maybe a)}

instance Semigroup (ChapterBuilder a) where
  b1 <> b2 = ChapterBuilder $ appendForest (toForest b1) (toForest b2)
    where
      appendForest :: Forest (Maybe a) -> Forest (Maybe a) -> Forest (Maybe a)
      appendForest f1 [] = f1
      appendForest [] f2 = f2
      appendForest (splitLast -> (ls, l)) (r : rs) = ls ++ merged l r ++ rs
        where
          merged t1@(Node _ forest1) (Node Nothing forest2) =
            [t1 {subForest = appendForest forest1 forest2}]
          merged _ _ = [l, r]

instance Monoid (ChapterBuilder a) where
  mempty = ChapterBuilder []

withBuilder :: [Item] -> [Chapter]
withBuilder = fromBuilder . foldMap toBuilder

toBuilder :: Item -> ChapterBuilder String
toBuilder = ChapterBuilder . pure . go 1
  where
    go :: Int -> Item -> Tree (Maybe String)
    go n i@Item {..}
      | level == n = pure $ Just text
      | otherwise = Node {rootLabel = Nothing, subForest = [go (succ n) i]}

fromBuilder :: ChapterBuilder String -> [Chapter]
fromBuilder = fromMaybe [] . fmap getCompose . sequenceA . Compose . toForest

splitLast :: [a] -> ([a], a)
splitLast xs = (init xs, last xs)

-- * foldl

withFoldl :: [Item] -> [Chapter]
withFoldl = foldl f []

f :: [Chapter] -> Item -> [Chapter]
f chapters item = go 1 chapters
  where
    go :: Int -> [Chapter] -> [Chapter]
    go _ [] = [toTree item]
    go n tst@(splitLast -> (ts, t))
      | level item == n = tst ++ [toTree item]
      | level item > n = ts ++ [t {subForest = go (succ n) (subForest t)}]
      | otherwise = error "always `level item >= n`"

toTree :: Item -> Tree String
toTree = pure . text
