module RealGcdLcm (modReal, gcdReal, lcmReal) where

-- |
-- >>> modReal 2.7 1.1
-- 0.5
modReal :: RealFrac a => a -> a -> a
modReal x y = x - y * fromIntegral (floor (x / y) :: Integer)

-- |
-- >>> import Data.Ratio
-- >>> gcdReal (27 % 10) (6 % 10)
-- 3 % 10
gcdReal :: RealFrac a => a -> a -> a
gcdReal x 0 = x
gcdReal x y = gcdReal y (x `modReal` y)

-- |
-- >>> import Data.Ratio
-- >>> foldr lcmReal 1 $ map (60 %) [50..54]
-- 60 % 1
lcmReal :: RealFrac a => a -> a -> a
lcmReal x y = x * y / gcdReal x y
