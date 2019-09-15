{-# LANGUAGE NoMonoLocalBinds #-}
module MapAccumWithState (mapAccumL, mapAccumR) where

import           Control.Applicative.Backwards (Backwards (Backwards, forwards))
import           Control.Monad.State           (runState, state)
import           Data.Tuple                    (swap)

mapAccumL :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumL =
  process . (runState .) . traverse . (state .) . process

mapAccumR :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumR =
  process . (runState .) . (forwards .) . traverse . (Backwards .) . (state .) . process

process :: (a -> b -> (c, d)) -> (b -> a -> (d, c))
process = (.) (swap .) . flip
-- process f b a = swap $ f a b

{-
mapAccum* :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)

---

Traversable t => (a -> b -> (a, c)) -> (a -> t b -> (a, t c))

 flip functions

Traversable t => (b -> a -> (a, c)) -> (t b -> a -> (a, t c))

 swap tuples

Traversable t => (b -> a -> (c, a)) -> (t b -> a -> (t c, a))

 to `State` monad

Traversable t => (b -> State a c) -> (t b -> State a (t c))

 generalize monad

(Traversable t, Monad m) => (b -> m c) -> (t b -> m (t c))

---

traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
-}
