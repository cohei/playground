{-# LANGUAGE Arrows        #-}
{-# LANGUAGE TupleSections #-}
module AutomatonCategory where

import           Control.Arrow    (Arrow (arr), returnA)
import           Control.Category (Category (id, (.)))
import           Prelude          (($))

newtype Automaton a i o = Automaton { runAutomaton :: a i (o, Automaton a i o) }

instance Arrow a => Category (Automaton a) where
  id      = Automaton $ arr (, id)
  a2 . a1 = Automaton $ proc i -> do
    (o1, a1') <- runAutomaton a1 -< i
    (o2, a2') <- runAutomaton a2 -< o1
    returnA -< (o2, a2' . a1')
