module Bifold where

{-
a   -> a' -> ... -> a''
    b        bs
c'' -> c' -> ... -> c
-}

fold :: ((a, c) -> b -> (a, c)) -> (a, c) -> [b] -> (a, c)
fold _ z      []       = z
fold f (a, c) (b : bs) = (a'', c'')
  where
    (a'', c') = fold f (a', c) bs
    (a', c'') = f (a, c') b

halve :: [a] -> ([a], [a])
halve xs = (front, rear)
  where
    ((rear, _), front) = fold f ((xs, xs), []) xs

    f :: (([a], [b]), [c]) -> c -> (([a], [b]), [c])
    f ((rear'    , []   ), _     ) _ = ((rear', []), [])
    f ((_ : rear', count), front') x = ((rear', drop 2 count), x : front')
    f (([]       , _    ), _     ) _ = error "rear より count が速く減って先に空になるのでこうはならない"

diffav :: Fractional a => [a] -> [a]
diffav []     = []
diffav (x:xs) = x - average : ys
  where
    average = sum' / n
    ((sum', n), ys) = fold f ((x, 1), []) xs
    f ((s, i), zs) z = ((s + z, i + 1), (z - average) : zs)
