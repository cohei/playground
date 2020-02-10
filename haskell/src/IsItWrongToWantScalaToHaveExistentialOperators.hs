-- | [「Scalaに存在演算子を求めるのは間違っているだろうか」をLens/Prismで解いてみる](http://aoino.hatenablog.com/entry/2015/07/15/164425)
module IsItWrongToWantScalaToHaveExistentialOperators
  ( A(A)
  , B(B)
  , C(C)
  , D(D)
  , E(E)
  , a
  , b
  , c
  , d
  , e
  ) where

import           Control.Lens (Lens', Prism', lens, prism')

newtype A = A Int
  deriving (Show, Eq)
newtype B = B (Maybe A)
  deriving (Show, Eq)
newtype C = C B
  deriving (Show, Eq)
newtype D = D (Maybe C)
  deriving (Show, Eq)
newtype E = E D
  deriving (Show, Eq)

a :: Lens' A Int
a = lens (\(A x) -> x) (\(A _) x -> A x)

b :: Prism' B A
b = prism' (B . Just) (\(B x) -> x)

c :: Lens' C B
c = lens (\(C x) -> x) (\(C _) x -> C x)

d :: Prism' D C
d = prism' (D . Just) (\(D x) -> x)

e :: Lens' E D
e = lens (\(E x) -> x) (\(E _) x -> E x)
