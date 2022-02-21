--
-- EPITECH PROJECT, 2022
-- Untitled (Workspace)
-- File description:
-- Decoration
--

import Data
import Decoration_AST

decorate :: Expr Undetermined -> Either Error (Expr CONTEXT)
decorate (Var name t a)
   | True = Right (Var name t (Ctx ([Varinfo (name, Short)], [Short, Integer, Long])))
   | otherwise  = Left "Var Error"
decorate (BinOp op a b)
   | True = decorate a >>= (\na -> decorate b >>= Right . BinOp op na)
   | otherwise = Left "Binop Error"
--   | True = Right (BinOp op (decorate a) (decorate b))