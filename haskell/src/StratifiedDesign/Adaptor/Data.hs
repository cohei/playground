module StratifiedDesign.Adaptor.Data (loadStopWords) where

import Data.Coerce (coerce)
import StratifiedDesign.Domain (Word (Word))
import Prelude (IO, lines, map, readFile, (.), (<$>))

loadStopWords :: IO [Word]
loadStopWords = map coerce . lines <$> readFile "stopwords.txt"
