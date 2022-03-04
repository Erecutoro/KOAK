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
decorationTests = test [assertEqual "Basic Variable Test" (Right [Var "none" "2" Custom (VarCtx Double)]) (startDecoration (callParser ["2;"]) (SymTab [])),
                        assertEqual "Basic BinOp Test" (Right [BinOp (Var "none" "2" Custom (VarCtx Double)) Add (Var "none" "2" Custom (VarCtx Double)) (BinOpCtx Double )]) (startDecoration (callParser ["2 + 2;"]) (SymTab [])),
                        assertEqual "Basic Function Test" (Right [Func "test" [Var "x" "none" Double (VarCtx Double)] Double (BinOp (Var "x" "none" Custom (VarCtx Double)) Add (Var "none" "2.0" Custom (VarCtx Double)) (BinOpCtx Double)) FuncCtx]) (startDecoration (callParser ["def test(x : double): double x + 2.0;"]) (SymTab [])),
                        assertEqual "Variable assignation Test" (Right [BinOp  (Var "y" "none" Custom (VarCtx Double)) Eq (Var "none" "2" (VarCtx Double)) (BinOpCtx Double)]) (startDecoration (callParser ["y = 2;"]) (SymTab []))]