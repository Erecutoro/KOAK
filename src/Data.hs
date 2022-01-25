module Data where

type Name = String

data Op =
    Add |
    Sub |
    Mul |
    Div deriving (Show)

data Type =
    Double Double |
    Int Integer |
    None |
    String String
    deriving (Show)

data Expr =
    Float Double |
    BinOp Op Expr Expr |
    Var Name |
    Val Name Expr |
    Type Type Expr |
    Call Name [Expr] |
    Func Name [Expr] Expr |
    Extern Name [Expr]
    deriving (Show)