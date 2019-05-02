module FizzBuzz.Rule (Rule, runRule, rule, modRule) where

type Rule a = Int -> Maybe a

runRule :: Rule a -> [Either Int a]
runRule r = map (\i -> maybeToRight i $ r i) [1..]

rule :: (Int -> Bool) -> a -> Rule a
rule p x i = if p i then Just x else Nothing

modRule :: Int -> a -> Rule a
modRule n = rule (`divisible` n)

maybeToRight :: a -> Maybe b -> Either a b
maybeToRight x = maybe (Left x) Right

divisible :: Integral a => a -> a -> Bool
divisible n m = n `mod` m == 0
