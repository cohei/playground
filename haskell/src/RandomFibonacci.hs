{-# LANGUAGE RecursiveDo #-}

-- | [ランダムフィボナッチ数列をHaskellで実装する](https://lotz84.github.io/haskell-notebooks/posts/2024-09-26/)
module RandomFibonacci (randomFibs) where

import Control.Monad.IO.Class (MonadIO)
import ListT (ListT)
import ListT qualified (unfoldM)
import System.Random (randomIO)

randomFibs :: ListT IO Integer
randomFibs =
  flip ListT.unfoldM (1, 1) $ \(ai, ai1) ->
    (\ai2 -> Just (ai, (ai1, ai2))) <$> addOrSubtract ?? ai1 ?? ai

addOrSubtract :: (MonadIO m, Num a) => m (a -> a -> a)
addOrSubtract = (\b -> if b then (+) else (-)) <$> randomIO

(??) :: (Functor f) => f (a -> b) -> a -> f b
f ?? a = fmap ($ a) f
