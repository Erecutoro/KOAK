--
-- EPITECH PROJECT, 2022
-- KOAK_Parser
-- File description:
-- koak
--

module Koak where

import LLVM.AST as AST
import LLVM.AST.Global
import LLVM.Context
import LLVM.Module

import Data.ByteString.Short
import Data.ByteString.Char8 as BS

import LLVM.ParseLLVM
import LLVM.GenCode
import Data
import Decoration_AST
import LLVM.Converter

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

genMainBlock :: [Expr Ctx] -> [BasicBlock]
genMainBlock ctx = []

initMain :: [Expr Ctx] -> Definition
initMain ctx = GlobalDefinition functionDefaults {
    name = mkName ".Main"
    , parameters = ([], False)
    , returnType = getType Data.Int
    , basicBlocks = genMainBlock ctx
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
      writeLLVMAssemblyToFile (File "tmp.ll") mod
  where
    mod = genModule $ genFunc expr []
