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
    BinOp Type (Expr a) Op (Expr a) a |
    Call Name [Expr a] a |
    Func Name [Expr a] Type (Expr a) a | --ne supporte pas les fontions sur plusieurs ligne
    State Statement (Expr a) Compare (Expr a) (Expr a) a -- pour l'instant if et for ne sont pas prévu
    deriving (Show)
    --Extern Name [Expr a]