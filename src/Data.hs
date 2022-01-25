module Data where

type Name = String

data Op =
    Add |
    Sub |
    Mul |
    Div deriving (Show)

data Type =
    None |
    Double Double |
    Int Integer |
    String String

data Expr =
    Float Double |
    BinOp Op Expr Expr |
    Var Name |
    Val Name Expr |
    Call Name [Expr] |
    Func Name [Expr] Expr |
    Extern Name [Expr]
    deriving (Show)