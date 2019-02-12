{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MagicHash      #-}
{-# LANGUAGE PolyKinds      #-}
-- | <https://qiita.com/ruicc/items/e2879c44eba77b1e7170 Levity polymorphismについて軽く>
module LevityPolymorphism where

import           GHC.Exts (Int (I#), Int#, TYPE, (+#))

class ToString (a :: TYPE r) where
  toString :: a -> String

instance ToString Int# where
  -- toString = ("I# " ++) . show . I# -- 関数は lifted なのでできない
  toString i = "I# " ++ show (I# i)

instance ToString Int where
  toString = show

class Add (a :: TYPE r) where
  add :: a -> a -> a

instance Add Int where
  add = (+)

instance Add Int# where
  add = (+#)

{-
> A levity-polymorphic type is not allowed here:
>   Type: a
>   Kind: TYPE r
> In the type of binder ‘x’

twice :: forall (a :: TYPE r). Add a => a -> a
twice x = add x x
-}

class Add a => Twice (a :: TYPE r) where
  twice :: a -> a

instance Twice Int where
  twice x = add x x

instance Twice Int# where
  twice x = add x x
