{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | <https://conscientiousprogrammer.com/blog/2015/12/15/24-days-of-hackage-2015-day-15-iospec-testing-io-and-some-quickcheck-tricks/ 24 days of Hackage, 2015: day 15: IOSpec: testing IO; and some QuickCheck tricks>
module IOSpec (spec) where

import           Control.Monad         (when)
import           Data.Coerce           (coerce)
import           Prelude               hiding (getLine, putStrLn)
import           Test.Hspec            (Spec)
import           Test.Hspec.QuickCheck (prop)
import           Test.IOSpec           (Effect (Done, Print, ReadChar), IOSpec,
                                        Teletype, evalIOSpec, getLine, putStrLn,
                                        singleThreaded)
import           Test.QuickCheck       (Arbitrary (arbitrary, shrink), suchThat)

logIn :: IOSpec Teletype ()
logIn = do
  putStrLn "% Enter password:"
  go
  putStrLn "$ Congratulations!"
  where
    go :: IOSpec Teletype ()
    go = do
      guess <- getLine
      when (guess /= "secret") $ do
        putStrLn "% Wrong password!"
        putStrLn "% Try again:"
        go

-- | Required for auto-discovery.
spec :: Spec
spec =
  prop "logIn outputs prompts until secret is guessed" $
    \(notSecretLines :: [NotSecretString]) (anyLines :: [NotNewlineString]) ->
      let
        allLines = coerce notSecretLines ++ ["secret"] ++ coerce anyLines
        expectedOutput =
          unlines $
            ["% Enter password:"] ++
            concatMap (const ["% Wrong password!", "% Try again:"]) notSecretLines ++
            ["$ Congratulations!"]
        output = takeOutput (withInput (unlines allLines) (evalIOSpec logIn singleThreaded))
      in
        output == expectedOutput

-- | User input without a newline, and not equal to "secret".
newtype NotSecretString =
  NotSecretString { getNotSecretString :: NotNewlineString }
  deriving (Show)

instance Arbitrary NotSecretString where
  arbitrary = NotSecretString <$> arbitrary `suchThat` ((/= "secret") . coerce)
  shrink = map NotSecretString
              . filter ((/= "secret") . coerce)
              . shrink
              . getNotSecretString

type NotNewlineString = [NotNewlineChar]

newtype NotNewlineChar =
  NotNewlineChar { getNotNewlineChar :: Char }
  deriving (Show)

-- | Quick hack. Ideally should write specific generator rather than
-- filtering off the default 'Char' generator.
instance Arbitrary NotNewlineChar where
  arbitrary = NotNewlineChar <$> arbitrary `suchThat` (/= '\n')
  shrink = map NotNewlineChar . filter (/= '\n') . shrink . getNotNewlineChar

takeOutput :: Effect () -> String
takeOutput (Done _)       = ""
takeOutput (Print c next) = c : takeOutput next
takeOutput _              = error "takeOutput: expects only Done, Print"

withInput :: [Char] -> Effect a -> Effect a
withInput _            (Done x)     = Done x
withInput stdin        (Print c e)  = Print c (withInput stdin e)
withInput (char:stdin) (ReadChar f) = withInput stdin (f char)
withInput _            _            = error "withInput: expects only Done, Print, ReadChar"
