--
-- EPITECH PROJECT, 2022
-- Untitled (Workspace)
-- File description:
-- Decoration_UT
--

module Decoration_UT where

import Data
import Parse
import Decoration_AST
import Decoration
import Test.HUnit

decorationTests :: Test
decorationTests = test [assertEqual "Basic Variable Test" (Right [Var "none" "2" Custom (VarCtx Decoration_AST.Double)]) (startDecoration (callParser ["2;"]) (SymTab [])),
                        assertEqual "Basic BinOp Test" (Right [BinOp Data.Double (Var "none" "2" Custom (VarCtx Decoration_AST.Double)) Add (Var "none" "2" Custom (VarCtx Decoration_AST.Double)) (BinOpCtx Decoration_AST.Double Decoration_AST.Double Decoration_AST.Double)]) (startDecoration (callParser ["2 + 2;"]) (SymTab []))]