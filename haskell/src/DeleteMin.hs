-- | [空ではないリストから最小の要素を１つだけ削除Jする](https://gist.github.com/nobsun/7501963)
-- ↑の定義だとちゃんと動かない
module DeleteMin (deleteMinPara, deleteMin) where

import           Data.List.NonEmpty    (NonEmpty, toList)
import qualified Data.List.NonEmpty    as N (break)

import           Data.Functor.Base     (NonEmptyF (NonEmptyF))
import           Data.Functor.Foldable (para)

deleteMinPara :: Ord a => NonEmpty a -> [a]
deleteMinPara = snd . para f
  where
    f :: Ord a => NonEmptyF a (NonEmpty a, (a, [a])) -> (a, [a])
    f (NonEmptyF x mxs) =
      case mxs of
        Nothing -> (x, [])
        Just (xs, (mi, ys))
          | x < mi    -> (x, toList xs)
          | otherwise -> (mi, x : ys)

deleteMin :: Ord a => NonEmpty a -> [a]
deleteMin xs =
  case N.break (== minimum xs) xs of
    (_, []) -> error "`xs` must contain `minimum xs` so this must not happen"
    (ys, _:zs) -> ys ++ zs
