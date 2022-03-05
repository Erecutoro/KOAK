--
-- EPITECH PROJECT, 2022
-- KOAK_Parser
-- File description:
-- genCode
--

module LLVM.GenCode where

import LLVM.AST as AST
import LLVM.AST.Attribute
import qualified LLVM.AST.CallingConvention as CC
import LLVM.AST.Constant
import LLVM.AST.AddrSpace

import Data
import Decoration_AST
import LLVM.LLVMType
import LLVM.Converter
import LLVM.AST.Global

-------------------------------------GET----------------------------------------

genDefaultVar :: Data.Type -> Operand
genDefaultVar t = ConstantOperand $ getCType t (VarCtx t) "0"

getLLVMVar :: Expr Ctx -> Operand
getLLVMVar var = case var of
                 Var n val t ctx -> if n == "none" 
                                    then ConstantOperand (getCType t ctx val)
                                    else LocalReference (getType t) (mkName n)

getVar :: Expr Ctx -> (Data.Name, Val, Data.Type, Ctx)
getVar var = case var of
             Var n val t mv -> (n, val, t, mv)

getCtxType :: Ctx -> Data.Type
getCtxType a = case a of
               CallCtx t -> t
               VarCtx t -> t
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
             Data.Double -> AST.FAdd
                noFastMathFlags (getLLVMVar a) (getLLVMVar b) []
             Data.Int -> AST.Add
                False False (getLLVMVar a) (getLLVMVar b) []

genSub :: Expr Ctx -> Expr Ctx -> Instruction
genSub a b = case checkType a b Data.Double of
             Data.Double-> AST.FSub
                noFastMathFlags (getLLVMVar a) (getLLVMVar b) []
             Data.Int -> AST.Sub
                False False (getLLVMVar a) (getLLVMVar b) []

genMul :: Expr Ctx -> Expr Ctx -> Instruction
genMul a b = case checkType a b Data.Double of
             Data.Double -> AST.FMul
                noFastMathFlags (getLLVMVar a) (getLLVMVar b) []
             Data.Int -> AST.Mul
                 False False (getLLVMVar a) (getLLVMVar b) []

genDiv :: Expr Ctx -> Expr Ctx -> Instruction
genDiv a b = case checkType a b Data.Double of
             Data.Double -> AST.FDiv
                noFastMathFlags (getLLVMVar a) (getLLVMVar b) []
             Data.Int -> AST.UDiv False (getLLVMVar a) (getLLVMVar b) []

genEq :: Expr Ctx -> Expr Ctx -> Instruction
genEq a b = case getExprType b of
            Data.Double -> AST.FAdd 
               noFastMathFlags (getLLVMVar b) (genDefaultVar Data.Double) []
            Data.Int -> AST.Add 
               False False (getLLVMVar b) (genDefaultVar Data.Int) []

genBinOp :: Op -> Expr Ctx -> Expr Ctx -> Instruction
genBinOp op a b = case op of
                  Data.Add -> genAdd a b
                  Data.Sub -> genSub a b
                  Data.Mul -> genMul a b
                  Data.Div -> genDiv a b
                  Data.Eq -> genEq a b

--------------------------------------CALL--------------------------------------

genCallArg :: [Expr Ctx] -> [(Operand, [ParameterAttribute])]
genCallArg [] = []
genCallArg [x] = [(getLLVMVar x, [])]
genCallArg (x:xs) = (getLLVMVar x, []) : genCallArg xs

genLLVMFuncArg :: [Expr Ctx] -> [AST.Type]
genLLVMFuncArg = map (getType . getExprType)

genCall :: Data.Name -> [Expr Ctx] -> AST.Type -> Instruction
genCall n arg t = AST.Call
         (Just NoTail)
         CC.C
         []
         (Right $ ConstantOperand $ GlobalReference
            (PointerType (FunctionType t (genLLVMFuncArg arg) False) (AddrSpace 0))
            (mkName n))
         (genCallArg arg)
         [] []

-------------------------------------START--------------------------------------

evalBinOp :: Expr Ctx -> Expr Ctx -> [(String, Named Instruction)]
evalBinOp a b = []

eval :: Expr Ctx -> [(String, Named Instruction)]
eval (Data.BinOp a Data.Eq b _) = evalBinOp a b

eval (Data.BinOp a op b _) = [("def", mkName "def" := genBinOp op a b)]
eval (Data.Call n arg t) = [(n, mkName n := genCall n arg (getLLVMType $ getCtxType t))]
eval a@(Data.Var n v t ctx) = [("def", mkName "def" := genEq a a)]

eval (Data.Func n arg t body ctx) = []
eval (Data.State dunno a cmp b body ctx) = []

evalRet :: String -> AST.Type -> Named Terminator
evalRet n t = Do $ Ret (Just $ LocalReference t (mkName n)) []

genBlocks :: Expr Ctx -> String -> Data.Type -> BasicBlock
genBlocks ctx name t = BasicBlock (mkName name) [ins] ret
    where
        [(n, ins)] = eval ctx
        ret = evalRet n (getLLVMType t)
