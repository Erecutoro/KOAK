module Main where

import System.IO
import System.Environment
import System.Exit

import LLVM.AST as AST
import LLVM.Context
import LLVM.Module

import Data.ByteString.Short
import Data.ByteString.Char8 as BS

import ParseLLVM

int :: AST.Type
int = IntegerType 32

toLLVM :: [Definition] -> AST.Module
toLLVM def = defaultModule
      { moduleName = toShort $ BS.pack "basic"
      , moduleDefinitions = def
      }

-- NEED [FUNCTION] HERE instead of String
genFunc :: [String] -> [Definition]
genFunc [] = []
genFunc [x] = [parseFunc x]
genFunc (x:xs) = parseFunc x : genFunc xs

parser :: String -> IO()
parser str = withContext $ \context ->
    do
  llvm <- withModuleFromAST context (toLLVM $ genFunc [str]) moduleLLVMAssembly
  BS.putStrLn llvm -- ast

main :: IO()
main = do
    args <- getArgs
    case args of
      [fileName] -> System.IO.readFile fileName >>= parser
      _ -> exitWith $ ExitFailure 84



------------------------------ Example -----------------------------------------

-- defSub :: Definition
-- defSub = GlobalDefinition functionDefaults
--   { name = Name 'sub'
--   , parameters =
--       ( [ Parameter int (Name 'a') []
--         , Parameter int (Name 'b') [] ]
--       , False )
--   , returnType = int
--   , basicBlocks = [body]
--   }
--   where
--     body = BasicBlock
--         (Name 'entry')
--         [ Name 'result' :=
--             Sub False  -- no signed wrap
--                 False  -- no unsigned wrap
--                 (LocalReference int (Name 'a'))
--                 (LocalReference int (Name 'b'))
--                 []]
--         (Do $ Ret (Just (LocalReference int (Name 'result'))) [])

-- defAdd :: Definition
-- defAdd = GlobalDefinition functionDefaults
--   { name = Name 'add'
--   , parameters =
--       ( [ Parameter int (Name 'a') []
--         , Parameter int (Name 'b') [] ]
--       , False )
--   , returnType = int
--   , basicBlocks = [body]
--   }
--   where
--     body = BasicBlock
--         (Name 'entry')
--         [ Name 'result' :=
--             Add False  -- no signed wrap
--                 False  -- no unsigned wrap
--                 (LocalReference int (Name 'a'))
--                 (LocalReference int (Name 'b'))
--                 []]
--         (Do $ Ret (Just (LocalReference int (Name 'result'))) [])

-- module_ :: AST.Module
-- module_ = defaultModule
--   { moduleName = 'basic'
--   , moduleDefinitions = [defAdd, defSub]
--   }

-- toLLVM :: AST.Module -> IO ()
-- toLLVM mod = withContext $ \contex -> 
--     do
--         llvm <- withModuleFromAST contex mod moduleLLVMAssembly
--         BS.putStrLn llvm

-- main :: IO ()
-- main = toLLVM module_
