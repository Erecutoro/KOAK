--
-- EPITECH PROJECT, 2022
-- Untitled (Workspace)
-- File description:
-- Decoration
--

module Decoration where

import Data
import Decoration_AST
import Parse


decorateList :: [Expr Undetermined ] -> SymbolTable -> Either Error [Expr Ctx]
decorateList [] _ = Right []
decorateList (a:as) st = decorate a st >>= (\ na -> decorateList as st >>= Right . (na:))

decorate :: Expr Undetermined -> SymbolTable -> Either Error (Expr Ctx)
decorate (Var name v t _) st
        | True = Right (Var name v t (VarCtx [Decoration_AST.Double]))
        | otherwise = Left "Var Error" 
decorate (BinOp _ a op b _) st
        | True = decorate a st >>= \na -> decorate b st >>= \nb ->Right (BinOp Data.Double na op nb (BinOpCtx [Decoration_AST.Double] [Decoration_AST.Double] [Decoration_AST.Double]))
        | otherwise = Left "Binop"
decorate (Call name a _) st
        | True = decorateList a st >>= \na -> Right (Call name na CallCtx )
        | otherwise = Left "Call"
decorate (Func name a t b _) st
        | True = decorateList a st >>= \na -> decorate b st >>= \nb -> Right (Func name na t nb FuncCtx)
        | otherwise = Left "Func"
decorate (State s a cmp b c _) st
        | True = decorate a st >>= \na -> decorate b st >>= \nb -> decorate c st >>= \nc -> Right (State s na cmp nb nc StateCtx)
        | otherwise = Left "State"

getArgument :: [Expr Ctx] -> Either Error [EXT_TYPE ]
getArgument ((Var _ _ t _):as) = case getArgument as of
    Left a -> Left a
    Right b -> Right (Decoration_AST.Double :b)
getArgument (_:as) = Left "Argument Error"
getArgument [] = Right []

setSymbolTable :: Expr Ctx -> SymbolTable -> Either Error SymbolTable 
setSymbolTable (Func name args t _ _) (SymTab st ) = case getArgument args of
    Right a -> Right (SymTab (Varinfo (name, FuncInfo Decoration_AST.Double a) : st)) 
    Left a -> Left a
setSymbolTable (BinOp t (Var name none Custom _) Eq _ _) (SymTab st ) = Right (SymTab (Varinfo (name, BinOpInfo Decoration_AST.Double): st) )
setSymbolTable _ st = Right st

startDecoration :: [Expr Undetermined] -> SymbolTable  -> Either Error [Expr Ctx]
startDecoration (a:as) st = 
    case decorate a st of
        Left x -> Left x
        Right e -> case setSymbolTable e st of
            Left err -> Left err
            Right nst -> case startDecoration as nst of
                Left errst -> Left errst
                Right x -> Right (e:x)
startDecoration [] st = Right []