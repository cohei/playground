-- https://scrapbox.io/haskell-shoen/限定継続
module DelimitedContinuation (main) where

import           Control.Monad.CC          (reset, runCCT, shift)
import           Control.Monad.Trans.Class (lift)

-- |
-- >>> main
-- Before shift
-- Before continuation
-- Returning a result
-- After shift
-- After continuation
-- 42
main :: IO ()
main = runCCT $ do
  x <- reset $ \p -> do
    lift $ putStrLn "Before shift"

    r <- shift p $ \k -> do
      lift $ putStrLn "Before continuation"
      a <- k $ lift (putStrLn "Returning a result") >> pure (42 :: Int)
      lift $ putStrLn "After continuation"
      pure a

    lift $ putStrLn "After shift"
    pure r

  lift $ print x
