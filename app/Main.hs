module Main where

import System.Environment
import System.Exit

import Parse
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

myDelim :: Char -> String -> [String]
myDelim c xs = case break (== c) xs of
    (y, _ : ys) -> y : myDelim c ys
    (y, []) -> [y]

mySplit :: [String] -> [String]
mySplit [] = []
mySplit (a:as) = (myDelim '\n' a) ++ (mySplit as)
