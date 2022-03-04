module Data where

type Name = String
type Val = String

data Undetermined =
    Empty
    deriving (Show,Eq)

data Op =
    Add |
    Sub |
    Mul |
    Div |
    Eq
    deriving (Show,Eq)

data Compare =
    Sup |
    Inf |
    Equal |
    SupEq |
    InfEq
    deriving (Show,Eq)

data Statement a =
    For (Expr a) (Expr a) Compare (Expr a) (Expr a) (Expr a) a| -- initial, left compare right, endloop, body
    While (Expr a) Compare (Expr a) (Expr a) a | -- left compare right body
    If (Expr a) Compare (Expr a) (Expr a) a | -- left compare right body
    Ifelse (Expr a) Compare (Expr a) (Expr a) (Expr a) a -- left compare right body else
    deriving (Show, Eq)

data Type =
    Int |
    Double |
    Str |
    Custom
    deriving (Show, Eq)

data Expr a =
    Var Name Val Type a |
    BinOp (Expr a) Op (Expr a) a |
    Call Name [Expr a] a |
    Func Name [Expr a] Type (Expr a) a |
    State (Statement a)
    deriving (Show, Eq)