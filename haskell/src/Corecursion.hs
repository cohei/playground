{-# LANGUAGE ConstraintKinds #-}
module Corecursion where

import GHC.Stack (HasCallStack)

succs :: Integer -> [Integer]
succs i = i : map succ (succs i)

data Tree a = Leaf | Tree (Tree a) a (Tree a)

depthFirstSearchRecursion :: Tree a -> [a]
depthFirstSearchRecursion Leaf = []
depthFirstSearchRecursion (Tree l x r) =
  x : depthFirstSearchRecursion l ++ depthFirstSearchRecursion r

depthFirstSearch :: Tree a -> [a]
depthFirstSearch t = go t []
  where
    go :: Tree a -> [a] -> [a]
    go Leaf xs = xs
    go (Tree l x r) xs = x : go l (go r xs)

levels :: Tree a -> [[a]]
levels t = map (map labelPartial) $ takeWhile (not . null) $ map (filter (not . isLeaf)) $ iterate (concatMap children) [t]

children :: Tree a -> [Tree a]
children Leaf = []
children (Tree l _ r) = [l, r]

isLeaf :: Tree a -> Bool
isLeaf Leaf = True
isLeaf _ = False

labelMaybe :: Tree a -> Maybe a
labelMaybe Leaf = Nothing
labelMaybe (Tree _ x _) = Just x

type Partial = HasCallStack

labelPartial :: Partial => Tree a -> a
labelPartial Leaf = error "labelPartial: Leaf"
labelPartial (Tree _ x _) = x

breadthFirstSearch :: Tree a -> [a]
breadthFirstSearch t = concat $ levels t
