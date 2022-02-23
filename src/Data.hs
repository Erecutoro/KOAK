module Data where

type Name = String
type Val = String

data Undetermined =
    Empty
    deriving (Show)

data Op =
    Add |
    Sub |
    Mul |
    Div |
    Eq
    deriving (Show)

data Compare =
    Sup |
    Inf |
    Equal |
    SupEq |
    InfEq
    deriving (Show)

data Statement =
    For |
    While |
    If
    deriving (Show)

data Type =
    Int |
    Double |
    Str |
    Custom
    deriving (Show)

data Expr a =
    BinOp (Expr a) Op (Expr a) |
    Var Name Val Type a |
    Call Name [Expr a] |
    Func Name [Expr a] [Expr a] Type |
    Extern Name [Expr a] |
    State Statement (Expr a) Compare (Expr a) [Expr a]
    deriving (Show)