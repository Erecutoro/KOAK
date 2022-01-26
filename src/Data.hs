module Data where

type Name = String

data Op =
    Add |
    Sub |
    Mul |
    Div deriving (Show)

data Type =
    Double Double |
    Float Double |
    Int Integer |
    None |
    String String
    deriving (Show)

data Expr =
    BinOp Op Expr Expr |
    Var Name Type |
    Type Type Expr |
    Call Name [Expr] |
    Func Name [Expr] Expr |
    Extern Name [Expr]
    deriving (Show)