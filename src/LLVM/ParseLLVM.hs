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
import qualified LLVM.AST.Float as F

import Data
import Decoration_AST

parseArgs :: [Expr a] -> [Parameter]
parseArgs [] = []
parseArgs (x:xs) = case x of
    Var n v t a -> case t of
        Data.Int -> Parameter int (mkName n) [] : parseArgs xs
        Data.Double -> Parameter double (mkName n) [] : parseArgs xs
        _ -> []
    _ -> []

llvmFloat :: String -> F.SomeFloat
llvmFloat val = F.Double (read val :: Double)

getFunc :: Expr Ctx -> (Data.Name, [Expr Ctx], Data.Type, Expr Ctx, Ctx)
getFunc expr = case expr of
               Func n arg ret body ctx -> (n, arg, ret, body, ctx)
