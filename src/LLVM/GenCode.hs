--
-- EPITECH PROJECT, 2022
-- KOAK_Parser
-- File description:
-- genCode
--

module LLVM.GenCode where

import LLVM.AST
import Control.Monad.Except

import LLVMType
import Data
import Decoration_AST
import LLVM.ParseLLVM

-------------------------------------GET----------------------------------------

getLLVMVar :: Expr Ctx -> Operand
getLLVMVar var = case var of
                 Var n val t mv -> if n == "none"
                                   then ConstantOperand (getCType t mv val)
                                   else LocalReference (getType t) (mkName n)

getVar :: Expr Ctx -> (Data.Name, Val, Data.Type, Ctx)
getVar var = case var of
             Var n val t mv -> (n, val, t, mv)

getCtxType :: Ctx -> Data.Type
getCtxType a = case a of
               VarCtx t -> case t of
                           Decoration_AST.Short -> Data.Int
                           Decoration_AST.Integer -> Data.Int
                           Decoration_AST.Long -> Data.Int
                           Decoration_AST.Double -> Data.Double
                           Decoration_AST.Char -> Data.Custom
               _ -> Data.Custom

getExprType :: Expr Ctx -> Data.Type
getExprType a = case a of
                Var _ _ t ctx -> case t of
                                 Custom -> getCtxType ctx
                                 _ -> t
                Func _ _ t _ _ -> t
                _ -> Data.Custom

------------------------------------CHECK---------------------------------------

checkType :: Expr Ctx -> Expr Ctx -> Data.Type -> Data.Type
checkType a b t = case getExprType a of
                  Data.Double -> case getExprType b of
                                 Data.Double -> Data.Double
                                 _ -> Data.Custom
                  Data.Int -> case getExprType b of
                                 Data.Int -> Data.Int
                                 _ -> Data.Int
                  _ -> Data.Custom

--------------------------------------OP----------------------------------------

genAdd :: Expr Ctx -> Expr Ctx -> Instruction
genAdd a b = case checkType a b Data.Double of
             Data.Double -> LLVM.AST.FAdd
                noFastMathFlags (getLLVMVar a) (getLLVMVar b) []
             Data.Int -> LLVM.AST.Add
                False False (getLLVMVar a) (getLLVMVar b) []

genSub :: Expr Ctx -> Expr Ctx -> Instruction
genSub a b = case checkType a b Data.Double of
             Data.Double-> LLVM.AST.FSub
                noFastMathFlags (getLLVMVar a) (getLLVMVar b) []
             Data.Int -> LLVM.AST.Sub
                False False (getLLVMVar a) (getLLVMVar b) []

genMul :: Expr Ctx -> Expr Ctx -> Instruction
genMul a b = case checkType a b Data.Double of
             Data.Double -> LLVM.AST.FMul
                noFastMathFlags (getLLVMVar a) (getLLVMVar b) []
             Data.Int -> LLVM.AST.Mul
                 False False (getLLVMVar a) (getLLVMVar b) []

genDiv :: Expr Ctx -> Expr Ctx -> Instruction
genDiv a b = case checkType a b Data.Double of
             Data.Double -> LLVM.AST.FDiv
                noFastMathFlags (getLLVMVar a) (getLLVMVar b) []
             Data.Int -> LLVM.AST.UDiv False (getLLVMVar a) (getLLVMVar b) []

genBinOp :: Op -> Expr Ctx -> Expr Ctx -> Instruction
genBinOp op a b = case op of
                  Data.Add -> genAdd a b
                  Data.Sub -> genSub a b
                  Data.Mul -> genMul a b
                  Data.Div -> genDiv a b

                  Data.Eq -> genAdd a b

--------------------------------------FUNC--------------------------------------

genCall :: Data.Name -> [Expr Ctx] -> Instruction
genCall n arg = genAdd (Prelude.head arg) (Prelude.head arg)

--------------------------------------GEN---------------------------------------

tmp :: Instruction
tmp = LLVM.AST.Add False False (LocalReference int (mkName "null")) (LocalReference int (mkName "null")) []

eval :: Expr Ctx -> (String, Named Instruction)
eval ctx = case ctx of
           Data.BinOp t a op b _ -> case op of
                               Data.Eq -> (n, mkName n := genBinOp op a b)
                               _ -> ("def", mkName "def" := genBinOp op a b) -- <- will throw 100%: redefinition of "none" variable
                               where
                                   (n, val, t, mv) = getVar a
           Data.Call n arg t -> (n, mkName n := genCall n arg)
           _ -> ("none", mkName "none" := tmp)

evalRet :: String -> Named Terminator
evalRet n = Do $ Ret (Just $ LocalReference int (mkName n)) []

genBlocks :: Expr Ctx -> String -> BasicBlock
genBlocks ctx name = BasicBlock (mkName name) [ins] ret
    where
        (n, ins) = eval ctx
        ret = evalRet n
