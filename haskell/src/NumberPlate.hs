-- | <https://gist.github.com/gaxiiiiiiiiiiii/b1e58813d9afea011c4466c1c18bc1ca>
module NumberPlate
  ( -- * solve
    solve
    -- * I/O
  , board
  , display
    -- * examples
  , easy
  , difficult
  ) where

import           Control.Monad       (join)
import           Data.Function       (on)
import           Data.List           (foldl', partition, sortOn, unfoldr, (\\))

data Cell =
  Cell
  { number :: Int
  , row    :: Int
  , column :: Int
  , block  :: Int
  }

type Board = [Cell]
type Filled = [Cell]
type Empties = [Cell]

type State = (Filled, Empties)

solve :: Board -> [Board]
solve = map fst . loop . partition isFilled
  where
    isFilled = (/= 0) . number

loop :: State -> [State]
loop state@(_, []) = [state]
loop (filled, target : empties) = nextStates >>= loop
  where
    nextStates = map (\c -> (target { number = c } : filled, empties)) candidates
    candidates = foldl' (\ns selector -> ns \\ inSame selector) [1..9] [row, column, block]

    inSame :: Eq a => (Cell -> a) -> [Int]
    inSame selector = map number (filter (((==) `on` selector) target) filled)

board :: [Int] -> Board
board = zipWith (\(r, c) n -> Cell n r c (whichBlock r c)) positions
  where
    whichBlock :: Int -> Int -> Int
    whichBlock r c = 3 * (r `div` 3) + c `div` 3

    positions :: [(Int, Int)]
    positions = join (liftA2 (,)) [0..8]

display :: Board -> IO ()
display = mapM_ (print . map number . sortOn column) . chunkOf 9 . sortOn row

chunkOf :: Int -> [a] -> [[a]]
chunkOf n = unfoldr f
  where
    f :: [a] -> Maybe ([a], [a])
    f [] = Nothing
    f xs = Just $ splitAt n xs

easy, difficult :: Board
easy = board
  [5,1,7, 6,0,0, 0,3,4,
   2,8,9, 0,0,4, 0,0,0,
   3,4,6, 2,0,5, 0,9,0,

   6,0,2, 0,0,0, 0,1,0,
   0,3,8, 0,0,6, 0,4,7,
   0,0,0, 0,0,0, 0,0,0,

   0,9,0, 0,0,0, 0,7,8,
   7,0,3, 4,0,0, 5,6,0,
   0,0,0, 0,0,0, 0,0,0]
difficult = board
  [0,0,5, 3,0,0, 0,0,0,
   8,0,0, 0,0,0, 0,2,0,
   0,7,0, 0,1,0, 5,0,0,

   4,0,0, 0,0,5, 3,0,0,
   0,1,0, 0,7,0, 0,0,6,
   0,0,3, 2,0,0, 0,8,0,

   0,6,0, 5,0,0, 0,0,9,
   0,0,4, 0,0,0, 0,3,0,
   0,0,0, 0,0,9, 7,0,0]
