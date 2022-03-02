module Main where

import System.IO
import System.Environment
import System.Exit

import Parse
import Data
import Decoration_AST
import Koak

ast :: [Expr Ctx]
ast = [Func "test" [Var "x" "none" Data.Double (VarCtx [Decoration_AST.Double])] Data.Double (BinOp Data.Double (Var "x" "none" Custom (VarCtx [Decoration_AST.Double])) Data.Mul (Var "none" "2.0" Data.Double (VarCtx [Decoration_AST.Double])) (BinOpCtx [Decoration_AST.Double] [Decoration_AST.Double] [Decoration_AST.Double])) FuncCtx]

main :: IO ()
main = koak ast
    -- do
    -- args <- getArgs
    -- files <- mapM readFile args
    -- let commands = callParser (mySplit files)
    -- print commands

myDelim :: String -> [String]
myDelim [] = []
myDelim (a:as)
    | a == '\n' = "" : myDelim as
    | otherwise = (a : head (myDelim as)) : tail (myDelim as)

mySplit :: [String] -> [String]
mySplit [] = []
mySplit (a:as) = (myDelim a) ++ (mySplit as)
