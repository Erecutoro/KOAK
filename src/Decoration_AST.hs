--
-- EPITECH PROJECT, 2022
-- Untitled (Workspace)
-- File description:
-- Decoration_AST
--

module Decoration_AST where

import Data

type INDENTIFIER = String

type Error = String

data SymbolInfo = BinOpInfo Type
                | FuncInfo Type [Type]
                deriving (Eq, Show)

newtype Variable = Varinfo (INDENTIFIER, SymbolInfo) 

newtype SymbolTable = SymTab [Variable]

data Ctx = VarCtx Type
        | BinOpCtx Type
        | CallCtx Type
        | FuncCtx
        | StateCtx
        deriving (Eq, Show)