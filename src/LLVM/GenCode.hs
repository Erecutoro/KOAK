--
-- EPITECH PROJECT, 2022
-- KOAK_Parser
-- File description:
-- genCode
--

{-# LANGUAGE OverloadedStrings #-}

module LLVM.GenCode where

import LLVM.AST
import Control.Monad.Except
import Data.ByteString.Char8 as BS
import Data.ByteString.Short

import LLVMType
import Data
import Decoration_AST
import LLVM.ParseLLVM

-------------------------------------GET----------------------------------------

getLLVMVar :: Expr a -> Operand
getLLVMVar var = case var of
             Var n val t mv -> LocalReference (getType t) (Name $ toShort $ BS.pack n)
             _ -> LocalReference VoidType $ Name $ toShort $ BS.pack "none"

getVar :: Expr a -> (Data.Name, Val, Data.Type, a)
getVar var = case var of
             Var n val t mv -> (n, val, t, mv)

--------------------------------------OP----------------------------------------

genAdd :: Expr a -> Expr b -> Instruction
genAdd a b = LLVM.AST.Add False False (getLLVMVar a) (getLLVMVar b) []

genSub :: Expr a -> Expr b -> Instruction
genSub a b = LLVM.AST.Sub False False (getLLVMVar a) (getLLVMVar b) []

genMul :: Expr a -> Expr b -> Instruction
genMul a b = LLVM.AST.Mul False False (getLLVMVar a) (getLLVMVar b) []

genDiv :: Expr a -> Expr b -> Instruction
genDiv a b = LLVM.AST.UDiv False (getLLVMVar a) (getLLVMVar b) []

genBinOp :: Op -> Expr a -> Expr b -> Instruction
genBinOp op a b = case op of
                  Data.Add -> genAdd a b
                  Data.Sub -> genSub a b
                  Data.Mul -> genMul a b
                  Data.Div -> genDiv a b

--------------------------------------FUNC--------------------------------------

genCall :: Data.Name -> [Expr a] -> Instruction
genCall n arg = genAdd (Prelude.head arg) (Prelude.head arg)

--------------------------------------GEN---------------------------------------

tmp :: Instruction
tmp = LLVM.AST.Add False False (LocalReference int (Name $ toShort $ BS.pack "null")) (LocalReference int (Name $ toShort $ BS.pack "null")) []

eval :: Expr CONTEXT -> Named Instruction
eval ctx = case ctx of
           Data.BinOp t a op b _ -> case op of
                               Data.Eq -> mkName n := genBinOp op a b
                               _ -> mkName "none" := genBinOp op a b -- <- will throw 100%: redefinition of "none" variable
                               where
                                   (n, val, t, mv) = getVar a
           Data.Call n arg t -> mkName n := genCall n arg
           _ -> Name "none" := tmp

genBlocks :: Expr CONTEXT -> ShortByteString -> BasicBlock
genBlocks ctx name = BasicBlock (Name name) [eval ctx] (Do $ Ret (Just (LocalReference int (Name $ toShort $ BS.pack "a"))) [])
