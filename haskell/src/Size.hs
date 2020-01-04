{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Size (size) where

import           Data.Function (on)

size :: (Enum a, Bounded a) => p a -> Integer
size (_ :: p a) = 1 + ((-) `on` fromIntegral . fromEnum) (maxBound @a) minBound
