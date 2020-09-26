{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

-- | Data types à la carte
-- http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf
module DataTypesALaCarte where

import Control.Lens (Iso', iso)
import Control.Monad.Free (Free (Free, Pure), foldFree, iterM, unfold, wrap)
import Data.Fix (Fix (Fix, unFix), foldFix)
import Data.Functor.Identity (Identity (runIdentity))
import Data.Functor.Sum (Sum (InL, InR))

class (Functor sub, Functor sup) => sub :<: sup where
  inject :: sub a -> sup a
  project :: sup a -> Maybe (sub a)

instance Functor f => f :<: f where
  inject = id
  project = Just

instance (Functor f, Functor g) => f :<: Sum f g where
  inject = InL
  project (InL x) = Just x
  project (InR _) = Nothing

instance {-# OVERLAPPABLE #-} (Functor f, Functor g, Functor h, f :<: h) => f :<: Sum g h where
  inject = InR . inject
  project (InL _) = Nothing
  project (InR x) = project x

injectFix :: f :<: g => f (Fix g) -> Fix g
injectFix = Fix . inject

data Val e = Val Int
  deriving (Functor)

val :: Val :<: f => Int -> Fix f
val = injectFix . Val

data Add e = Add e e
  deriving (Functor)

infixl 6 !+

(!+) :: Add :<: f => Fix f -> Fix f -> Fix f
(!+) = (injectFix .) . Add

data Mul e = Mul e e
  deriving (Functor)

infixl 7 !*

(!*) :: Mul :<: f => Fix f -> Fix f -> Fix f
(!*) = (injectFix .) . Mul

addExample :: Fix (Sum Val Add)
addExample = val 118 !+ val 1219

example :: Fix (Sum Val (Sum Add Mul))
example = val 80 !* val 5 !+ val 4

class Functor f => Eval f where
  evalAlgebra :: f Int -> Int

instance Eval Val where
  evalAlgebra (Val i) = i

instance Eval Add where
  evalAlgebra (Add x y) = x + y

instance Eval Mul where
  evalAlgebra (Mul x y) = x * y

instance (Eval f, Eval g) => Eval (Sum f g) where
  evalAlgebra (InL x) = evalAlgebra x
  evalAlgebra (InR x) = evalAlgebra x

eval :: Eval f => Fix f -> Int
eval = foldFix evalAlgebra

class Functor f => Render f where
  renderAlgebra :: f String -> String

instance Render Val where
  renderAlgebra (Val i) = show i

instance Render Add where
  renderAlgebra (Add s1 s2) = "(" ++ s1 ++ " + " ++ s2 ++ ")"

instance Render Mul where
  renderAlgebra (Mul s1 s2) = "(" ++ s1 ++ " * " ++ s2 ++ ")"

instance (Render f, Render g) => Render (Sum f g) where
  renderAlgebra (InL x) = renderAlgebra x
  renderAlgebra (InR x) = renderAlgebra x

render :: Render f => Fix f -> String
render = foldFix renderAlgebra

match :: (f :<: g) => Fix g -> Maybe (f (Fix g))
match = project . unFix

distribute :: (Add :<: f, Mul :<: f) => Fix f -> Maybe (Fix f)
distribute t = do
  Mul a b <- match t
  Add c d <- match b
  pure $ a !* c !+ a !* d

data Zero a deriving (Functor)

data One a = One deriving (Functor)

data Const a b = Const a deriving (Functor)

isoIdentityFreeZero :: Iso' (Free Zero a) (Identity a)
isoIdentityFreeZero = iso (foldFree (\case )) (unfold (Left . runIdentity))

isoMaybeFreeOne :: Iso' (Free One a) (Maybe a)
isoMaybeFreeOne = iso (foldFree (\One -> Nothing)) (unfold (maybe (Right One) Left))

isoEitherFreeConst :: Iso' (Free (Const a) b) (Either a b)
isoEitherFreeConst = iso (foldFree (\(Const a) -> Left a)) (unfold (either (Right . Const) Left))

injectFree :: f :<: g => f (Free g a) -> Free g a
injectFree = wrap . inject

data Increment a = Increment Int a
  deriving (Functor)

increment :: Increment :<: f => Int -> Free f ()
increment i = injectFree $ Increment i (pure ())

data Recall a = Recall (Int -> a)
  deriving (Functor)

recall :: Recall :<: f => Free f Int
recall = injectFree $ Recall pure

data Clear a = Clear a
  deriving (Functor)

clear :: Clear :<: f => Free f ()
clear = injectFree $ Clear (pure ())

tick :: Free (Sum Recall Increment) Int
tick = do
  y <- recall
  increment 1
  pure y

newtype Memory = Memory Int

class Functor f => Run f where
  runAlgebra :: f (Memory -> (a, Memory)) -> Memory -> (a, Memory)

instance Run Increment where
  runAlgebra (Increment k r) (Memory i) = r (Memory (i + k))

instance Run Recall where
  runAlgebra (Recall r) (Memory i) = r i (Memory i)

instance Run Clear where
  runAlgebra (Clear r) _ = r (Memory 0)

instance (Run f, Run g) => Run (Sum f g) where
  runAlgebra (InL r) = runAlgebra r
  runAlgebra (InR r) = runAlgebra r

foldFree' :: Functor f => (a -> b) -> (f b -> b) -> Free f a -> b
foldFree' pure' _ (Pure x) = pure' x
foldFree' pure' impure (Free f) = impure $ fmap (foldFree' pure' impure) f

run :: Run f => Free f a -> Memory -> (a, Memory)
run = foldFree' (,) runAlgebra

data Teletype a
  = GetChar (Char -> a)
  | PutChar Char a
  deriving (Functor)

putChar' :: Teletype :<: f => Char -> Free f ()
putChar' c = injectFree $ PutChar c $ pure ()

data FileSystem a
  = ReadFile FilePath (String -> a)
  | WriteFile FilePath String a
  deriving (Functor)

readFile' :: FileSystem :<: f => FilePath -> Free f String
readFile' fp = injectFree $ ReadFile fp pure

exec :: Exec f => Free f a -> IO a
exec = iterM execAlgebra

class Functor f => Exec f where
  execAlgebra :: f (IO a) -> IO a

instance Exec Teletype where
  execAlgebra (GetChar f) = getChar >>= f
  execAlgebra (PutChar c k) = putChar c >> k

instance Exec FileSystem where
  execAlgebra (ReadFile fp f) = readFile fp >>= f
  execAlgebra (WriteFile fp s k) = writeFile fp s >> k

instance (Exec f, Exec g) => Exec (Sum f g) where
  execAlgebra (InL x) = execAlgebra x
  execAlgebra (InR x) = execAlgebra x

cat :: FilePath -> Free (Sum Teletype FileSystem) ()
cat fp = readFile' fp >>= mapM_ putChar'
