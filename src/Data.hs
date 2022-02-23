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
    Var Name Val Type a |
    BinOp (Expr a) Op (Expr a) a|
    Call Name [Expr a] |
    Func Name [Expr a] Type (Expr a)|
    State Statement (Expr a) Compare (Expr a) [Expr a] |
    Extern Name [Expr a]
    deriving (Show)