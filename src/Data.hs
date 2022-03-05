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

data Statement =
    For |
    While |
    If
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
    Func Name [Expr a] Type (Expr a) a | --ne supporte pas les fontions sur plusieurs ligne
    State Statement (Expr a) Compare (Expr a) (Expr a) a -- pour l'instant if et for ne sont pas prévu
    deriving (Show, Eq)
    --Extern Name [Expr a]