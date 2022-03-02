--
-- EPITECH PROJECT, 2022
-- Untitled (Workspace)
-- File description:
-- Decoration_AST
--

module Decoration_AST where

type INDENTIFIER = String

type Error = String

data EXT_TYPE =   Char 
            | Short
            | Integer  
            | Long
            | Double  
            deriving (Show)

data SymbolInfo = BinOpInfo EXT_TYPE
                | FuncInfo EXT_TYPE [EXT_TYPE]
                deriving (Show)

newtype Variable = Varinfo (INDENTIFIER, SymbolInfo) 

newtype SymbolTable = SymTab [Variable]

data Ctx = VarCtx EXT_TYPE
        | BinOpCtx EXT_TYPE EXT_TYPE EXT_TYPE
        | CallCtx EXT_TYPE
        | FuncCtx
        | StateCtx
        deriving (Show)