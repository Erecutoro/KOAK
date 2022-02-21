--
-- EPITECH PROJECT, 2022
-- Untitled (Workspace)
-- File description:
-- Decoration
--

import Data
import Decoration_AST

decorateList :: [Expr Undetermined] -> Either Error [Expr CONTEXT]
decorateList [] = Right []
decorateList (a:as) = decorate a >>= (\ na -> decorateList as >>= Right . (na:))


decorate :: Expr Undetermined -> Either Error (Expr CONTEXT)
decorate (Var name t a)
   | True = Right (Var name t (Ctx ([Varinfo (name, Short)], [Short, Integer, Long])))
   | otherwise  = Left "Var Error"
decorate (BinOp op a b)
   | True = decorate a >>= (\na -> decorate b >>= Right . BinOp op na)
   | otherwise = Left "Binop Error"
decorate (Type t a)
   | True = decorate a >>= Right . Type t
   | otherwise  = Left "Type error"
decorate (Call name a)
   | True = decorateList a >>=  Right . Call name
   | otherwise  = Left "Call error"
decorate (Func name a b)
   | True = decorateList a >>= (\ na -> decorate b >>= Right . Func name na)
   | otherwise = Left "Func error"
--   | True = Right (BinOp op (decorate a) (decorate b))