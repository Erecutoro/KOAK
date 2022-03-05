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

data CompareState a =
    Comp (Expr a) Compare (Expr a)
    deriving (Show, Eq)

data Statement a =
    For (Expr a) (CompareState a) (Expr a) (Expr a) a| -- initial, CompareState, endloop, body
    While (CompareState a) (Expr a) a | -- CompareState body
    If (CompareState a) (Expr a) a | -- CompareState body
    Ifelse (CompareState a) (Expr a) (Expr a) a -- CompareState body else
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