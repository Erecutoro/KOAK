--
-- EPITECH PROJECT, 2022
-- Untitled (Workspace)
-- File description:
-- Decoration
--

import Data
import Decoration_AST

decorate :: CONTEXT -> Expr a  -> Either () (CONTEXT, Expr [EXT_TYPE])
decorate ctx (Var name t) | True = Right (ctx, (Var name t))
                          | otherwise  = Left ()
-- decorate (Call name ((a ctx):b))