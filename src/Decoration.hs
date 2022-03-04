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
--------------------------- Symbole Table Functions ---------------------------
-------------------------------------------------------------------------------

findSymbol :: String -> SymbolTable -> Either Error SymbolInfo  
findSymbol name (SymTab []) = Left ("\ESC[31mERROR\ESC[0m - Variable \"" ++ name ++ "\" has not been defined")
findSymbol name (SymTab (Varinfo (a, sinfo):as))
    | a == name = Right sinfo
    | otherwise = findSymbol name (SymTab as)

setSymbolTable :: Expr Ctx -> SymbolTable -> Either Error SymbolTable 
setSymbolTable (Func name args t _ _) (SymTab st ) = case getArgument args of
    Right a -> Right (SymTab (Varinfo (name, FuncInfo Double a) : st)) 
    Left a -> Left a
setSymbolTable (BinOp (Var name none Custom _) Eq _ _) (SymTab st ) = Right (SymTab (Varinfo (name, BinOpInfo Double): st) )
setSymbolTable _ st = Right st

-------------------------------------------------------------------------------
------------------------------- Decoration List -------------------------------
-------------------------------------------------------------------------------

decorateList :: [Expr Undetermined] -> SymbolTable -> Either Error [Expr Ctx]
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

getArgument :: [Expr Ctx] -> Either Error [Type ]
getArgument ((Var _ _ t _):as) = case getArgument as of
    Left a -> Left a
    Right b -> Right (Double :b)
getArgument (_:as) = Left "\ESC[31mERROR\ESC[0m - Argument is not Valid"
getArgument [] = Right []

-------------------------------------------------------------------------------
-------------------------------- Type Function --------------------------------
-------------------------------------------------------------------------------

-- typeToExttype :: Data.Type -> Type 
-- typeToExttype Data.Double = Double
-- typeToExttype Data.Int = Int
-- typeToExttype _ = Double 

getType :: Expr Ctx -> Either Error Type 
getType (Var _ _ _ (VarCtx t)) = Right t
getType (BinOp _ _ _ (BinOpCtx t)) = Right t
getType (Call _ _ (CallCtx a)) = Right a
getType _ = Left ""

binOpTypage :: Type -> Type  -> Either Error Type 
binOpTypage Int Int = Right Int 
binOpTypage Int Double  = Right Double  
binOpTypage Double Int = Right Double 
binOpTypage Double Double  = Right Double 
binOpTypage Custom _ = Right Custom 
binOpTypage _ Custom  = Right Custom 
binOpTypage _ _ = Left "\ESC[31mERROR\ESC[0m - Operation is not handled" 

setBinOpTypage :: Expr Ctx -> Expr Ctx -> Either Error Type 
setBinOpTypage a b = getType a >>= \na -> getType b >>= \nb -> binOpTypage na nb

-------------------------------------------------------------------------------
--------------------------- Decoration Sub-Function ---------------------------
-------------------------------------------------------------------------------

decorateVar :: Expr Undetermined -> SymbolTable  -> Either Error (Expr Ctx)
decorateVar (Var "none" v t _) st = Right (Var "none" v t (VarCtx t))
decorateVar (Var name v t _) st = case findSymbol name st of 
    Right FuncInfo {} -> Left "\ESC[31mERROR\ESC[0m - Function has been called as a variable"
    Right (BinOpInfo nt) -> Right (Var name v t (VarCtx nt))
    Left a -> Left a
decorateVar _ _ = Left "\ESC[31mERROR\ESC[0m - An unexpected Error has occured in a variable"

decorateAssignVar :: Expr Undetermined -> SymbolTable -> Either Error (Expr Ctx)
decorateAssignVar (Var name "none" t _) st = Right (Var name "none" t (VarCtx t))
decorateAssignVar Var {} _ = Left "\ESC[31mERROR\ESC[0m - Variable is not valid for an assignation"
decorateAssignVar _ _ = Left "\ESC[31mERROR\ESC[0m - Assignation can only be done on a variable"

decorateBinOp :: Expr Undetermined -> SymbolTable  -> Either Error (Expr Ctx)
decorateBinOp (BinOp a Eq b _) st = decorateAssignVar a st >>= \na -> decorate b st >>= \nb -> Right (BinOp na Eq nb (BinOpCtx Double))
decorateBinOp (BinOp a op b _) st = decorate a st >>= \na -> decorate b st >>= \nb -> setBinOpTypage na nb >>= \nt -> Right (BinOp na op nb (BinOpCtx nt))
decorateBinOp _ _ = Left "\ESC[31mERROR\ESC[0m - An unexpected error has occured in a Binop"

decorateCall :: Expr Undetermined -> SymbolTable -> Either Error (Expr Ctx)
decorateCall (Call name a _) st = case findSymbol name st of
    Right BinOpInfo {} -> Left "\ESC[31mERROR\ESC[0m - Variable has been called as a function"
    Right (FuncInfo t args_t) -> decorateList a st >>= \na -> Right (Call name na (CallCtx t))
    Left a -> Left ("\ESC[31mERROR\ESC[0m - Function \"" ++ name ++ "\" has not been defined")
decorateCall _ _ = Left "\ESC[31mERROR\ESC[0m - An unknown error has been detected in a function Call"

decorateFunc :: Expr Undetermined -> SymbolTable -> Either Error (Expr Ctx)
decorateFunc (Func name a t b _) st = decorateArgumentList a st >>= \na -> setArgumentST na st >>= \nst -> decorate b nst >>= \nb -> Right (Func name na t nb FuncCtx)
decorateFunc _ _ = Left "\ESC[31mERROR\ESC[0m - An unexpected error has occured in a function"

decorateState :: Expr Undetermined -> SymbolTable -> Either Error (Expr Ctx)
decorateState (State s a cmp b c _) st = decorate a st >>= \na -> decorate b st >>= \nb -> decorate c st >>= \nc -> Right (State s na cmp nb nc StateCtx)
decorateState _ _ = Left "\ESC[32m[CONGRATULATION!]\ESC[0m - You coded so badly you created an impossible error, go touch some grass now"

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
