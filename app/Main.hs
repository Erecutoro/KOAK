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
callRidwhite = map (ridLine . ridWhitespace)

ridWhitespace :: String -> String
ridWhitespace = filter (\xs -> (xs /=' '))

ridLine :: String -> String
ridLine = filter (\xs -> (xs /='\n'))