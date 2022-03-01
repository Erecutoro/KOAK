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

genModule :: [Definition] -> AST.Module
genModule def = defaultModule
      { moduleName = toShort $ BS.pack "basic"
      , moduleDefinitions = def
      }

initFunc :: Expr Ctx -> Definition
initFunc expr = GlobalDefinition functionDefaults
  {
    name = Name $ toShort $ BS.pack n
    , parameters = (parseArgs arg, False)
    , returnType = getType ret
    , basicBlocks = [genBlocks body (toShort $ BS.pack n)]
  }
  where
    (n, arg, ret, body, ctx) = getFunc expr

-- Remove all non func

genFunc :: [Expr Ctx] -> [Definition]
genFunc [] = []
genFunc [x] = [initFunc x]
genFunc (x:xs) = initFunc x : genFunc xs

koak :: [Expr Ctx] -> IO ()
koak expr = withContext $ \context ->
  withModuleFromAST context mod $ \mod ->
      writeLLVMAssemblyToFile (File "tmp.ll") mod
  where
    mod = genModule $ genFunc expr