{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{- |
[TypeScript の型のみで世界一型の厳しいプログラミング](https://zenn.dev/ame_x/articles/8ec1ec35cdc392)
-}
module TypeLevelTrim (Trim) where

import GHC.TypeLits (Symbol, ConsSymbol, UnconsSymbol)

-- |
-- >>> :seti -XDataKinds
-- >>> import Data.Type.Equality ((:~:)(Refl))
-- >>> Refl :: Trim " hello " :~: "hello"
-- Refl
type Trim :: Symbol -> Symbol
type family Trim s where
  Trim s = TrimLeft (TrimRight s)

type TrimLeft :: Symbol -> Symbol
type family TrimLeft s where
  TrimLeft s = TrimLeft1 (UnconsSymbol s)

type TrimLeft1 :: Maybe (Char, Symbol) -> Symbol
type family TrimLeft1 unconsed where
  TrimLeft1 Nothing = ""
  TrimLeft1 (Just '( ' ', s )) = TrimLeft s
  TrimLeft1 (Just '( c, s )) = ConsSymbol c s

type TrimRight :: Symbol -> Symbol
type family TrimRight s where
  TrimRight s = Reverse (TrimLeft (Reverse s))

type Reverse :: Symbol -> Symbol
type family Reverse s where
  Reverse s = Reverse1 "" (UnconsSymbol s)

type Reverse1 :: Symbol -> Maybe (Char, Symbol) -> Symbol
type family Reverse1 acc unconsed where
  Reverse1 acc Nothing = acc
  Reverse1 acc (Just '(c, s)) = Reverse1 (ConsSymbol c acc) (UnconsSymbol s)
