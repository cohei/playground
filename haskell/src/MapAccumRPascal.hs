-- | <https://qiita.com/nobsun/items/389da409cabceebd8ab7 Data.List.mapAccumR のつかいどころ>
module MapAccumRPascal where

import           Data.List (mapAccumR)

pascal :: [[Integer]]
pascal = [1] : map f pascal
  where
    f :: [Integer] -> [Integer]
    f xs = snd $ mapAccumR g xs' xs'
      where
        g :: [Integer] -> Integer -> ([Integer], Integer)
        g []       _ = error "first argument is non-empty list"
        g (y : ys) z = (ys, y + z)

        xs' :: [Integer]
        xs' = 0 : xs

nCombinations :: Int -> Int -> Integer
nCombinations = (.) (.) (.) (!!) (!!) pascal
