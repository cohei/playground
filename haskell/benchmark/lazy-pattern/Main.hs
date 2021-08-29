-- | [Why is the lazy pattern match version of splitAt function faster?](https://stackoverflow.com/questions/42150614)
module Main (main) where

import Criterion.Main (defaultMain, nf, bgroup, bench)

splitInfiniteList :: (Int -> String -> (String, String)) -> String
splitInfiniteList splitAt' = fst $ splitAt' 1000000 $ repeat 'a'

splitAtLazy :: Int -> [a] -> ([a], [a])
splitAtLazy n xs | n <= 0 = ([], xs)
splitAtLazy _ [] = ([], [])
splitAtLazy n (x:xs) =
  case splitAtLazy (n - 1) xs of
    ~(prefix, suffix) -> (x : prefix, suffix)

splitAtStrict :: Int -> [a] -> ([a], [a])
splitAtStrict n xs | n <= 0 = ([], xs)
splitAtStrict _ [] = ([], [])
splitAtStrict n (x:xs) =
  case splitAtStrict (n - 1) xs of
    (prefix, suffix) -> (x : prefix, suffix)

main :: IO ()
main =
  defaultMain
    [ bgroup "split-infinite-list"
      [ bench "Prelude" $ nf splitInfiniteList splitAt
      , bench "lazy" $ nf splitInfiniteList splitAtLazy
      , bench "strict" $ nf splitInfiniteList splitAtStrict
      ]
    ]
