--
-- EPITECH PROJECT, 2022
-- Untitled (Workspace)
-- File description:
-- Decoration
--

import Data
import Decoration_AST

-- decorateList :: [Expr Undetermined] -> Either Error [Expr CONTEXT]
-- decorateList [] = Right []
-- decorateList (a:as) = decorate a >>= (\ na -> decorateList as >>= Right . (na:))

-- decorate :: Expr CONTEXT -> Either Error (Expr CONTEXT)
-- decorate (Var name t a)
--    | True = Right (Var name t (Ctx [Varinfo (name, Decoration_AST.Double )]))
--    | otherwise  = Left "Var Error"
-- decorate (BinOp a op b)
--    | True = decorate a >>= (\na -> decorate b >>= Right . BinOp na op)
--    | otherwise = Left "Binop Error"
-- decorate (Call name a)
--    | True = decorateList a >>=  Right . Call name
--    | otherwise  = Left "Call error"
-- decorate (Func name a t)
--    | True = decorateList a >>= (\ na -> Right (Func name na t))
--    | otherwise = Left "Func error"
-- decorate (Extern name a)
--    | True = decorateList a >>=  Right . Call name
--    | otherwise  = Left "Call error"

-- decorate :: Expr Undetermined -> Either Error (Expr CONTEXT, CONTEXT)
-- decorate (Var name t a)
-- 
-- getContext :: Expr CONTEXT -> CONTEXT 
-- getContext (Var _ _ a) = a
-- getContext (BinOp _ _ _)
-- 
startDecoration :: [Expr a] -> CONTEXT -> Either Error [Expr CONTEXT]
startDecoration (a:as) ctx = case decorate (a ctx) of
                                    Left x -> Left x
                                    Right (e) -> case startDecoration as y of
                                                            Left x -> Left x
                                                            Right x -> Right ((expr y) ++ x)