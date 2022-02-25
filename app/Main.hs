module Main where

import System.Exit
import System.IO
import System.Environment
import Parse
import Decoration

main :: IO ()
main = do
    args <- getArgs
    files <- mapM readFile args
    let commands = callRidwhite files
    print files
    print commands

callRidwhite :: [String] -> [String]
callRidWhite _ = []
callRidwhite (a:as) = ridLine (ridWhitespace a) : callRidWhite as

ridWhitespace :: String -> String
ridWhitespace [] = []
ridWhitespace file = filter (\xs -> (xs /=' ')) file

ridLine :: String -> String
ridLine [] = []
ridLine file = filter (\xs -> (xs /='\n')) file