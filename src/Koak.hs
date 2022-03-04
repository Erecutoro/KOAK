--
-- EPITECH PROJECT, 2022
-- KOAK_Parser
-- File description:
-- koak
--

module Koak where

import  System.Process

import LLVM.AST as AST
import LLVM.AST.Global
import LLVM.Context
import LLVM.Module
import qualified LLVM.AST.Constant as C

import Data.ByteString.Short
import Data.ByteString.Char8 as BS

import LLVM.ParseLLVM
import LLVM.GenCode
import Data
import Decoration_AST
import LLVM.Converter
import LLVM.LLVMType

genModule :: [Definition] -> AST.Module
genModule def = defaultModule
      { moduleName = toShort $ BS.pack "basic"
      , moduleDefinitions = def
      }

initFunc :: (Data.Name, [Expr Ctx], Data.Type, Expr Ctx, Ctx) -> Definition
initFunc (n, arg, ret, body, ctx)  = GlobalDefinition functionDefaults
  {
    name = mkName n
    , parameters = (parseArgs arg, False)
    , returnType = getType ret
    , basicBlocks = [genBlocks body n ret]
  }

genMainBlock :: [Expr Ctx] -> BasicBlock
genMainBlock [] = BasicBlock (mkName "main") [] ret
    where
        ret = Do $ Ret (Just $ ConstantOperand (C.Int 32 0)) []
genMainBlock [ctx] = BasicBlock (mkName "main") [ins] ret
    where
        (n, ins) = eval ctx
        ret = Do $ Ret (Just $ ConstantOperand (C.Int 32 0)) []
genMainBlock (ctx:a) = BasicBlock (mkName "main") [ins] ret
    where
        (n, ins) = eval ctx
        ret = Do $ Ret (Just $ ConstantOperand (C.Int 32 0)) []

initMain :: [Expr Ctx] -> Definition
initMain ctx = GlobalDefinition functionDefaults {
    name = mkName "main"
    , parameters = ([], False)
    , returnType = int
    , basicBlocks = [genMainBlock ctx]
  }

genFunc :: [Expr Ctx] -> [Expr Ctx] -> [Definition]
genFunc [] [] = []
genFunc [] y = [initMain y]
genFunc [x] y = case x of
    Func n arg ret body ctx -> initFunc (n, arg, ret, body, ctx) : genFunc [] y
    _ -> genFunc [] (x:y)
genFunc (x:xs) y = case x of
    Func n arg ret body ctx -> initFunc (n, arg, ret, body, ctx) : genFunc xs y
    _ -> genFunc xs (x:y)

koak :: [Expr Ctx] -> IO ()
koak expr = withContext $ \context ->
  withModuleFromAST context mod $ \mod ->
      writeLLVMAssemblyToFile (File "tmp.ll") mod >>
        callCommand "llc-9 -filetype=obj tmp.ll -o tmp.o; clang tmp.o -o a.out"
  where
    mod = genModule $ genFunc expr []
