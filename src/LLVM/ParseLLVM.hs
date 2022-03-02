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
import qualified LLVM.AST.Constant as C
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

getCType :: Data.Type -> Val -> C.Constant
getCType t val = case t of
                 Data.Int -> C.Int 32 $ read val
                 Data.Double -> C.Float $ llvmFloat val
                 _ -> C.Int 32 3

getType :: Data.Type -> AST.Type
getType t = case t of 
                  Data.Int -> int
                  Data.Double -> double
                  _ -> int

getFunc :: Expr Ctx -> (Data.Name, [Expr Ctx], Data.Type, Expr Ctx, Ctx)
getFunc expr = case expr of
               Func n arg ret body ctx -> (n, arg, ret, body, ctx)
