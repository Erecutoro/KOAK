--
-- EPITECH PROJECT, 2022
-- KOAK_Parser
-- File description:
-- Converter
--

module LLVM.Converter where

import LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import LLVM.AST.Type

import Data
import Decoration_AST
import LLVMType
import LLVM.ParseLLVM

getCtxCType :: Ctx -> Val -> C.Constant
getCtxCType ctx val = case ctx of
                  VarCtx t -> case t of
                            Int -> C.Int 32 $ read val
                            Double -> C.Float $ llvmFloat val
                            _ -> C.Int 32 84
                  _ -> C.Int 32 84

getCType :: Data.Type -> Ctx -> Val -> C.Constant
getCType t ctx "none" = case t of
                 Data.Int -> C.Int 32 0
                 Data.Double -> C.Float $ llvmFloat "0"
                 _ -> getCtxCType ctx "0"
getCType t ctx val = case t of
                 Data.Int -> C.Int 32 $ read val
                 Data.Double -> C.Float $ llvmFloat val
                 _ -> getCtxCType ctx val

getType :: Data.Type -> AST.Type
getType t = case t of 
                  Data.Int -> int
                  Data.Double -> double
                  _ -> int

getLLVMType :: Data.Type -> LLVM.AST.Type.Type
getLLVMType t = case t of
                Data.Double -> double
                Data.Int -> int
                _ -> int