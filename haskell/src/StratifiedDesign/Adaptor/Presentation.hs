module StratifiedDesign.Adaptor.Presentation (askForText, displayWordCount) where

import Data.Coerce (coerce)
import Numeric.Natural (Natural)
import StratifiedDesign.Domain (Text (Text))

askForText :: IO Text
askForText = putStr "Text: " >> coerce getLine

displayWordCount :: Natural -> IO ()
displayWordCount n = putStrLn $ "Number of words: " ++ show n
