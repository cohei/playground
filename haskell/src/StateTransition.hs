-- | [ElmでPhantom TypeとExtensible Recordを用いて型安全な状態遷移パターンを実装する](https://www.izumisy.work/entry/2020/01/04/154007)
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module StateTransition where

import           GHC.TypeLits    (Symbol)
import           Type.Membership (Member)

data Transition (a :: [Symbol]) = Transition

data Game (state :: State) = Game

data State =
    Loading
  | Ready
  | Playing
  | Paused
  | GameOver

type family Next (state :: State) :: [State] where
  Next 'Loading  = '[ 'Ready ] -- `'['Ready]` causes a parse error
  Next 'Ready    = '[ 'Playing ]
  Next 'Playing  = '[ 'Paused, 'GameOver ]
  Next 'Paused   = '[ 'Playing ]
  Next 'GameOver = '[ 'Ready ]

toReady :: Game 'Loading -> Game 'Ready
toReady _ = Game

next :: Member (Next s1) s2 => Game s1 -> Game s2
next _ = Game

{-
*StateTransition> :t next (Game :: Game 'Loading) :: Game 'Ready
next (Game :: Game 'Loading) :: Game 'Ready :: Game 'Ready

*StateTransition> :t next (Game :: Game 'Loading) :: Game 'GameOver

<interactive>:1:1: error:
    • Couldn't match type ‘'Type.Membership.Internal.Missing 'GameOver’
                     with ‘'Type.Membership.Internal.Expecting pos0’
        arising from a use of ‘next’
    • In the expression: next (Game :: Game 'Loading) :: Game 'GameOver
-}
