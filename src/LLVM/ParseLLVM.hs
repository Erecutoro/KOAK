--
-- EPITECH PROJECT, 2022
-- KOAK_Parser
-- File description:
-- ParseLLVM
--

module LLVM.ParseLLVM where

import LLVMType
import LLVM.AST as AST
import LLVM.AST.Type

import Data.ByteString.Char8 as BS
import Data.ByteString.Short

import Data
import Decoration_AST

parseArgs :: [Expr a] -> [Parameter]
parseArgs [] = []
parseArgs (x:xs) = case x of
                   Var n v t a -> case t of
                                  Data.Int -> Parameter int (Name $ toShort $ BS.pack n) [] : parseArgs xs
                                  Data.Double -> Parameter double (Name $ toShort $ BS.pack n) [] : parseArgs xs
                                  _ -> []
                   _ -> []

getType :: Data.Type -> AST.Type
getType t = case t of 
                  Data.Int -> int
                  Data.Double -> double
                  _ -> int

getFunc :: Expr Ctx -> (Data.Name, [Expr Ctx], Data.Type, Expr Ctx, Ctx)
getFunc expr = case expr of
               Func n arg ret body ctx -> (n, arg, ret, body, ctx)
