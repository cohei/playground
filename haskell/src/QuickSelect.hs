module QuickSelect (quickSelect) where

import Data.List (delete, genericLength, partition)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty, toList)
import Numeric.Natural (Natural)

-- $setup
-- >>> import Data.List (sort, (!?))
-- >>> import Data.List.NonEmpty (fromList)
-- >>> import Test.QuickCheck.Instances ()

-- |
-- >>> quickSelect 3 $ fromList [0, 3, 1, 8, 4]
-- Just 4
--
-- prop> quickSelect i xs == sort (toList xs) !? fromIntegral i
quickSelect :: (Ord a) => Natural -> NonEmpty a -> Maybe a
quickSelect 0 (x :| []) = Just x
quickSelect _ (_ :| []) = Nothing
quickSelect i xs@(pivot :| _) =
  let
    (l, r) = splitAround pivot xs
    pivotIndex = genericLength l
  in
    case compare i pivotIndex of
      EQ -> Just pivot
      LT -> nonEmpty l >>= quickSelect i
      GT -> nonEmpty r >>= quickSelect (i - pivotIndex - 1)

-- | Split elements into (smaller, larger) excluding the pivot element
splitAround :: (Ord a) => a -> NonEmpty a -> ([a], [a])
splitAround pivot xs = partition (pivot >) $ delete pivot $ toList xs
