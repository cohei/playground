{-# LANGUAGE OverloadedRecordDot #-}
-- | https://nunulk-blog-to-kill-time.hatenablog.com/entry/5-programming-languages-blew-my-mind
module Main (main) where

import Control.Lens ((&), filtered, (%~), singular)
import Control.Monad (zipWithM_)
import Data.Generics.Product.Fields (field)
import Data.List (maximumBy, genericTake)
import Data.Ord (comparing)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

main :: IO ()
main = do
  let bentos =
        [ Bento {name = "唐揚げ", count = 10},
          Bento {name = "唐揚げ", count = 8},
          Bento {name = "唐揚げ", count = 6}
        ]
      tryCount :: Natural
      tryCount = 5
  zipWithM_ report [1 ..] $ genericTake tryCount $ iterate snitchOne bentos

data Bento = Bento {name :: String, count :: Natural}
  deriving (Eq, Generic)

instance Show Bento where
  show bento = "(name: " ++ bento.name ++ "count: " ++ show bento.count

type Bentos = [Bento]

report :: Int -> Bentos -> IO ()
report i bentos = do
  putStrLn $ show i ++ "回目"
  mapM_ print bentos

snitchOne :: Bentos -> Bentos
snitchOne bentos = bentos & singular (traverse . filtered (== m)) . field @"count" %~ pred'
  where
    m :: Bento
    m = maximumBy (comparing count) bentos
    pred' :: Natural -> Natural
    pred' = max 0 . pred
