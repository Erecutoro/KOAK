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
    Int |
    Double |
    Str |
    Custom
    deriving (Show)

data Expr a =
    BinOp (Expr a) Op (Expr a) |
    Var Name Type a |
    Call Name [Expr a] |
    Func Name [Expr a] Type |
    Extern Name [Expr a]
    deriving (Show)

data Par =
    Fun String |
    Dec String
    deriving (Show)