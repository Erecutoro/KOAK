--
-- EPITECH PROJECT, 2022
-- Untitled (Workspace)
-- File description:
-- Decoration
--

import Data
import Decoration_AST

decorateList :: [Expr CONTEXT ] -> Either Error [Expr CONTEXT]
decorateList [] = Right []
decorateList (a:as) = decorate a >>= (\ na -> decorateList as >>= Right . (na:))

decorate :: Expr CONTEXT -> Either Error (Expr CONTEXT)
decorate (Var name v t ctx)
   | True = Right (Var name v t ctx)
   | otherwise  = Left "Var Error"
decorate (BinOp t a op b ctx)
   | True = decorate a >>= (\na -> decorate b >>= (\nb -> Right (BinOp t na op nb ctx)))
   | otherwise = Left "Binop Error"
decorate (Call name a ctx)
   | True = decorateList a >>= (\na -> Right (Call name na ctx))
   | otherwise  = Left "Call error"
decorate (Func name a t b ctx)
   | True = decorateList a >>= (\ na -> Right (Func name na t b ctx))
   | otherwise = Left "Func error"
decorate (State statement a cmp b c ctx)
   | True = Right (State statement a cmp b c ctx)
   | otherwise = Left "Statement error"

getContext :: Expr CONTEXT -> CONTEXT 
getContext (Var _ _ _ a) = a
getContext (BinOp _ _ _ _ a) = a
getContext (Call _ _ a) = a
getContext (Func _ _ _ _ a) = a
getContext (State _ _ _ _ _ a) = a

startDecoration :: [Expr a] -> CONTEXT -> Either Error [Expr CONTEXT]
startDecoration (a:as) ctx = 
    case decorate (a ctx) of
        Left x -> Left x
        Right e -> case startDecoration as y of
            Left x -> Left x
            Right x -> Right ((expr y) ++ x)