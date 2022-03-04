module Main where

import System.IO
import System.Environment
import System.Exit

import Parse
import Data
import Decoration_AST
import Decoration
import Koak

main :: IO ()
main = do
    args <- getArgs
    files <- mapM readFile args
    let commands = callParser (mySplit files)
    case startDecoration commands (SymTab []) of
        Right a -> koak a
        _ -> exitWith $ ExitFailure 84

myDelim :: String -> [String]
myDelim [] = []
myDelim (a:as)
    | a == '\n' = "" : myDelim as
    | otherwise = (a : head (myDelim as)) : tail (myDelim as)

mySplit :: [String] -> [String]
mySplit [] = []
mySplit (a:as) = (myDelim a) ++ (mySplit as)
