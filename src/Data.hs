module Data where

type Name = String

data Op =
    Add |
    Sub |
    Mul |
    Div
    deriving (Show)

data Undetermined =
    Empty
    deriving (Show)

data Deco =
    None |
    DecoType String |
    Set [String]
    deriving (Show)

data Type =
    Int Integer |
    Double Double |
    String String |
    Custom
    deriving (Show)

data Expr a =
    BinOp Op (Expr a) (Expr a) |
    Var Name Type |
    Type Type (Expr a) |
    Call Name [Expr a] |
    Func Name [Expr a] (Expr a) |
    Extern Name [Expr a]
    deriving (Show)