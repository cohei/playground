-- |
-- [Stratified Design over Layered Design](https://medium.com/clean-code-development/stratified-design-over-layered-design-125727c7e15)
--
-- > A user enters a one line text through the console and the program determines the number of words in the text. But not all words count! Some stop words defined in the file „stopwords.txt“ should be ignored.
module StratifiedDesign.Main (main) where

import StratifiedDesign.Adaptor.Data (loadStopWords)
import StratifiedDesign.Adaptor.Presentation (askForText, displayWordCount)
import StratifiedDesign.Domain (countWords)

main :: IO ()
main = displayWordCount =<< liftA2 (flip countWords) askForText loadStopWords
