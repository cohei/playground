-- | [表現可能関手](http://bitterharvest.hatenablog.com/entry/2018/02/17/101018)
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeFamilies #-}
module Representable where

import           Data.Distributive (Distributive (distribute))
import           Data.Functor.Rep  (Representable (Rep, index, tabulate))
import           Data.Stream       (Stream, (<:>))
import qualified Data.Stream       as S (head, tail, unfold, (!!))

instance Distributive Stream where
  distribute s = fmap S.head s <:> distribute (fmap S.tail s)

instance Representable Stream where
  type Rep Stream = Int
  tabulate f = S.unfold (\i -> (f i, succ i)) 0
  index = (S.!!)
