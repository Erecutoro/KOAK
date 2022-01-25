module Data where

type Name = String

data Op =
    Add |
    Sub |
    Mul |
    Div deriving (Show)

data Expr =
    Float Double |
    BinOp Op Expr Expr |
    Var Name |
    Call Name [Expr] |
    Func Name [Expr] Expr |
    Extern Name [Expr]
    deriving (Show)