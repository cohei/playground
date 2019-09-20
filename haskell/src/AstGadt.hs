-- | [GADTでエレガントに抽象構文木の階層を作る](https://qiita.com/kmizu/items/c26afbec1011481f40e7)
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
module AstGadt where

{-
e ::= e e       //連接
    | e '/' e   //選択
    | e '*'     //反復（0回以上）
    | e '+'     //反復（1回以上）
    | '&' e     //肯定先読み
    | '!' e     //否定先読み
    | "..."     //文字列
-}

data Category = Core | Sugar

data Expression (a :: Category) where
  Sequence :: Expression a -> Expression a -> Expression a
  Choice   :: Expression a -> Expression a -> Expression a
  Repeat0  :: Expression a -> Expression a
  Repeat1  :: Expression a -> Expression 'Sugar
  And      :: Expression a -> Expression 'Sugar
  Not      :: Expression a -> Expression a
  Alpha    :: Char -> Expression a

paren :: String -> String
paren s = "(" ++ s ++ ")"

display :: Expression 'Core -> String
display (Sequence e1 e2) = paren $ display e1 ++ " " ++ display e2
display (Choice e1 e2)   = paren $ display e1 ++ "/" ++ display e2
display (Repeat0 e)      = paren (display e) ++ "*"
display (Not e)          = "!" ++ paren (display e)
display (Alpha c)        = ['\'', c]

desugar :: Expression a -> Expression 'Core
desugar (Sequence e1 e2) = Sequence (desugar e1) (desugar e2)
desugar (Choice e1 e2)   = Choice (desugar e1) (desugar e2)
desugar (Repeat0 e)      = Repeat0 (desugar e)
desugar (Repeat1 e)      = Sequence (desugar e) (Repeat0 (desugar e))
desugar (And e)          = Not (Not (desugar e))
desugar (Not e)          = Not (desugar e)
desugar (Alpha c)        = Alpha c

expression :: Expression 'Sugar
expression = Sequence (And ab) (Repeat1 ab)
  where
    ab :: Expression a
    ab = Choice (Alpha 'a') (Alpha 'b')

test :: Bool
test = display (desugar expression) == "(!(!(('a/'b))) (('a/'b) (('a/'b))*))"
