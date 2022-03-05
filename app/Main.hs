module Main where

import System.Exit
import System.IO
import System.Environment
import Parse
import Data
import Decoration

main :: IO ()
main = do
    args <- getArgs
    files <- mapM readFile args
    let commands = callParser (mySplit files)
    print commands

myDelim :: Char -> String -> [String]
myDelim c xs = case break (== c) xs of
    (y, _ : ys) -> y : myDelim c ys
    (y, []) -> [y]

mySplit :: [String] -> [String]
mySplit [] = []
mySplit (a:as) = (myDelim '\n' a) ++ (mySplit as)