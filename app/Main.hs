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
    print files
    print commands

myDelim :: String -> [String]
myDelim [] = []
myDelim (a:as)
    | a == '\n' = "" : myDelim as
    | otherwise = (a : head (myDelim as)) : tail (myDelim as)

mySplit :: [String] -> [String]
mySplit [] = []
mySplit (a:as) = (myDelim a) ++ (mySplit as)