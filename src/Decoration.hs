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

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

decorateList :: [Expr Undetermined ] -> SymbolTable -> Either Error [Expr Ctx]
decorateList [] _ = Right []
decorateList (a:as) st = decorate a st >>= (\ na -> decorateList as st >>= Right . (na:))

decorateArgumentList :: [Expr Undetermined] -> SymbolTable -> Either Error [Expr Ctx]
decorateArgumentList [] _ = Right []
decorateArgumentList ((Var name v t a):as) st = decorateAssignVar (Var name v t a) st >>= \na -> decorateArgumentList as st >>= \nas -> Right (na:nas)
decorateArgumentList _ _ = Left "Type Error"

setArgumentST :: [Expr Ctx] -> SymbolTable -> Either Error SymbolTable 
setArgumentST [] st =  Right st
setArgumentST ((Var name _ _ (VarCtx a)):as) (SymTab st) = setArgumentST as (SymTab st) >>= \(SymTab nst) -> Right (SymTab (Varinfo (name, BinOpInfo a):nst))
setArgumentST _ st = Right st 

findSymbol :: String -> SymbolTable -> Either Error SymbolInfo  
findSymbol _ (SymTab []) = Left "No symbol found"
findSymbol name (SymTab (Varinfo (a, sinfo):as))
    | a == name = Right sinfo
    | otherwise = findSymbol name (SymTab as)

typeToExttype :: Data.Type -> EXT_TYPE 
typeToExttype Data.Double = Decoration_AST.Double
typeToExttype Data.Int = Decoration_AST.Integer
typeToExttype _ = Decoration_AST.Double 

getType :: Expr Ctx -> Either Error EXT_TYPE 
getType (Var _ _ t _) = Right (typeToExttype t)
getType (BinOp t _ _ _ _) = Right (typeToExttype t)
getType (Call _ _ (CallCtx a)) = Right a
getType _ = Left ""

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

binOpTypage :: EXT_TYPE -> EXT_TYPE  -> Either Error EXT_TYPE 
binOpTypage Decoration_AST.Integer Decoration_AST.Integer = Right Decoration_AST.Integer 
binOpTypage Decoration_AST.Integer Decoration_AST.Double  = Right Decoration_AST.Double  
binOpTypage Decoration_AST.Double Decoration_AST.Integer = Right Decoration_AST.Double 
binOpTypage Decoration_AST.Double Decoration_AST.Double  = Right Decoration_AST.Double 
binOpTypage _ _ = Left "Type Error" 

setBinOpTypage :: Expr Ctx -> Expr Ctx -> Either Error EXT_TYPE 
setBinOpTypage a b = getType a >>= \na -> getType b >>= \nb -> binOpTypage na nb

decorateVar :: Expr Undetermined -> SymbolTable  -> Either Error (Expr Ctx)
decorateVar (Var "none" v t _) st = Right (Var [] v t (VarCtx Decoration_AST.Double))
decorateVar (Var name v t _) st = case findSymbol name st of 
    Right FuncInfo {} -> Left "Var Error - function is not a type"
    Right BinOpInfo {} -> Right (Var name v t (VarCtx Decoration_AST.Double))
    Left a -> Left a
decorateVar _ _ = Left "Var Error"

decorateAssignVar :: Expr Undetermined -> SymbolTable -> Either Error (Expr Ctx)
decorateAssignVar (Var name "none" t _) st = Right (Var name "none" t (VarCtx Decoration_AST.Double))
decorateAssignVar _ _ = Left "Variable Assignation Error"

decorateBinOp :: Expr Undetermined -> SymbolTable  -> Either Error (Expr Ctx)
decorateBinOp (BinOp _ a Eq b _) st = decorateAssignVar a st >>= \na -> decorate b st >>= \nb -> Right (BinOp Data.Double na Eq nb (BinOpCtx Decoration_AST.Double Decoration_AST.Double Decoration_AST.Double))
decorateBinOp (BinOp _ a op b _) st = decorate a st >>= \na -> decorate b st >>= \nb -> setBinOpTypage na nb >>= \nt -> Right (BinOp Data.Double na op nb (BinOpCtx nt Decoration_AST.Double Decoration_AST.Double))
decorateBinOp _ _ = Left "Binop Error"

decorateCall :: Expr Undetermined -> SymbolTable -> Either Error (Expr Ctx)
decorateCall (Call name a _) st = case findSymbol name st of
    Right BinOpInfo {} -> Left "Call Error - type is not a function"
    Right (FuncInfo t args_t) -> decorateList a st >>= \na -> Right (Call name na (CallCtx Decoration_AST.Double))
    Left a -> Left a
decorateCall _ _ = Left "Call Error"

decorateFunc :: Expr Undetermined -> SymbolTable -> Either Error (Expr Ctx)
decorateFunc (Func name a t b _) st = decorateArgumentList a st >>= \na -> setArgumentST na st >>= \nst -> decorate b nst >>= \nb -> Right (Func name na t nb FuncCtx)
decorateFunc _ _ = Left "Func Error"

decorateState :: Expr Undetermined -> SymbolTable -> Either Error (Expr Ctx)
decorateState (State s a cmp b c _) st = decorate a st >>= \na -> decorate b st >>= \nb -> decorate c st >>= \nc -> Right (State s na cmp nb nc StateCtx)
decorateState _ _ = Left "State Error"

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

decorate :: Expr Undetermined -> SymbolTable -> Either Error (Expr Ctx)
decorate a st = case a of
    Var {} -> decorateVar a st
    BinOp {} -> decorateBinOp a st
    Call {} -> decorateCall a st
    Func {} -> decorateFunc a st
    State {} -> decorateState a st

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

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
