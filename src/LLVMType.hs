--
-- EPITECH PROJECT, 2022
-- KOAK_Parser
-- File description:
-- LLVMType
--

module LLVMType where

import LLVM.AST as AST

int :: AST.Type
int = IntegerType 32

void :: AST.Type
void = VoidType