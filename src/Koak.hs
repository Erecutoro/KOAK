--
-- EPITECH PROJECT, 2022
-- KOAK_Parser
-- File description:
-- koak
--

module Koak where

import LLVM.AST as AST
import LLVM.Context
import LLVM.Module

import Data.ByteString.Short
import Data.ByteString.Char8 as BS

import LLVM.ParseLLVM

toLLVM :: [Definition] -> AST.Module
-- toLLVM def = newModule
toLLVM def = defaultModule
      { moduleName = toShort $ BS.pack "basic"
      , moduleDefinitions = def
      }

genFunc :: [(String, String)] -> [Definition]
genFunc [] = []
genFunc [x] = [LLVM.ParseLLVM.parseFunc x]
genFunc (x:xs) = LLVM.ParseLLVM.parseFunc x : genFunc xs

koak :: String -> IO ()
koak str = withContext $ \context ->
  withModuleFromAST context mod $ \mod ->
      writeLLVMAssemblyToFile (File ".test.ll") mod
  where
    mod = toLLVM $ genFunc [("add(a:double, b:int)", str)]