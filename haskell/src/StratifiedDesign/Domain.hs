module StratifiedDesign.Domain (Text (Text), Word (Word), countWords) where

import Data.Coerce (coerce)
import Data.List (genericLength)
import Numeric.Natural (Natural)
import Prelude (Eq, String, filter, flip, notElem, words, (.))

newtype Word = Word String
  deriving (Eq)

newtype Text = Text String

countWords :: [Word] -> Text -> Natural
countWords stopWords = genericLength . removeStopWords stopWords . extractWords

extractWords :: Text -> [Word]
extractWords = coerce words

removeStopWords :: [Word] -> [Word] -> [Word]
removeStopWords = filter . flip notElem
