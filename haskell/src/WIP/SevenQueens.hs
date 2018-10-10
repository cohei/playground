{-# LANGUAGE TupleSections #-}
module WIP.SevenQueens where

import           Control.Monad       (guard, replicateM)
import           Control.Monad.State (State, StateT, get, put)
import           Control.Monad.Trans (lift)
import           Data.Array          (Array, assocs, elems, listArray, (//))
import           Data.Bool           (bool)
import           Data.List           (nub)
import           Utility             (chop)

--type Board = [[Bool]]
type Board = Array Coor Bool
type Coor = (Int, Int) -- (tate, yoko)
type Queen = Coor
type Queens = [Coor]

data BoardState =
  BoardState { board :: Board, queens :: Queens }
  deriving Eq

instance Show BoardState where
  show =
    unlines .
    map concat .
    chop 7 .
    elems .
    (flip (//) . map (, "q ") . queens <*> fmap (bool "- " "o ") . board)

emptyBoard :: Board
emptyBoard = listArray ((1, 1), (7, 7)) $ repeat False

emptyBoardState :: BoardState
emptyBoardState = BoardState { board = emptyBoard, queens = [] }

queenPower :: Queen -> [Coor]
queenPower (n,m) = filter inRange $
                   [ (n,j) | j <- [1..7] ] ++ [ (i,m) | i <- [1..7] ] ++
                   [ (i, n+m-i) | i <- [1..(n+m-1)] ] ++ [ (i, m-n+i) | i <- [1..7] ]
    where
      inRange (i,j) = inRange' i && inRange' j
      inRange' i = 1 <= i && i <= 7

putQueen :: Queen -> BoardState -> BoardState
putQueen q (BoardState b qs) = BoardState { board = b // map (,True) (queenPower q)
                                          , queens = q:qs
                                          }

initial :: BoardState
initial = foldr putQueen emptyBoardState [(3,3), (5,4)]

puttables :: BoardState -> [Coor]
puttables = map fst . filter (not . snd) . assocs . board

allOccupied :: BoardState -> Bool
allOccupied = and . elems . board

canPuts :: [Queens]
canPuts = filter ((5 ==) . length) $ map nub $ replicateM 5 $ puttables initial

a :: IO ()
a = mapM_ print $ nub [ b | qs <- canPuts, let b = foldr putQueen initial qs, allOccupied b ]


stepNonDet :: State BoardState ()
stepNonDet = do
  _boardState <- get
  undefined

putQueenNonDeterministic :: StateT BoardState [] ()
putQueenNonDeterministic = do
  boardState <- get
  guard $ allOccupied boardState
  p <- lift $ puttables boardState
  put $ putQueen p boardState




boardChar :: [[Char]]
boardChar = chop 7 "lhikoavrqsczlpuwalnfotykajehahitsydefoptxnruzwyve"
