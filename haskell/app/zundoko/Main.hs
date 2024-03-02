module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (replicateM, (<=<))
import Control.Monad.Random (MonadRandom (getRandoms), Random (random, randomR))
import Data.Bifunctor (first)
import Data.Function (on)
import Data.List (inits, intersperse, isSuffixOf)
import Data.Maybe (listToMaybe)
import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = maybe zunDokoKiyoshi (print <=< averageZunDokoLength) =<< readArgument

readArgument :: IO (Maybe Int)
readArgument = fmap read . listToMaybe <$> getArgs

zunDokoKiyoshi :: IO ()
zunDokoKiyoshi = do
  zds <- zunDokos
  printZunDokoKiyoshi zds
  print $ length zds

printZunDokoKiyoshi :: [ZunDoko] -> IO ()
printZunDokoKiyoshi zds = do
  hSetBuffering stdout NoBuffering
  sequence_ $ intersperse rest $ map (putStr . show) zds ++ [kiyoshi]
  where
    rest = threadDelay 500000
    kiyoshi = putStrLn " \\キ・ヨ・シ！/"

data ZunDoko = Zun | Doko deriving (Eq)

instance Show ZunDoko where
  show Zun = "ズン"
  show Doko = "ドコ"

instance Random ZunDoko where
  randomR = undefined
  random = first (\b -> if b then Zun else Doko) . random

zunDokos :: (MonadRandom m) => m [ZunDoko]
zunDokos = takeUntil [Zun, Zun, Zun, Zun, Doko] <$> getRandoms

takeUntil :: (Eq a) => [a] -> [a] -> [a]
takeUntil p = head . filter (isSuffixOf p) . inits

-- expected length is about 32? want to prove
averageZunDokoLength :: (MonadRandom m) => Int -> m Double
averageZunDokoLength n = average . map length <$> replicateM n zunDokos

average :: [Int] -> Double
average = on (/) fromIntegral . sum <*> length
