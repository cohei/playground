module MonoidAsASingleObjectCategory (M, fromMonoid) where

import Control.Category (Category (id, (.)))
import Data.Type.Equality (trans, (:~:) (Refl))

-- | Monoid as morphisms on a single object 'a'
data M m a b = M (a :~: b) m

instance (Show m) => Show (M m a b) where
  show (M _ m) = "fromMonoid " ++ show m

fromMonoid :: m -> M m a a
fromMonoid = M Refl

-- |
-- >>> import Control.Category qualified as C ((.))
-- >>> fromMonoid [1, 2] C.. fromMonoid [3]
-- fromMonoid [1,2,3]
instance (Monoid m) => Category (M m) where
  id = fromMonoid mempty
  M refl1 m1 . M refl2 m2 = M (trans refl2 refl1) (m1 <> m2)
