--
-- EPITECH PROJECT, 2022
-- KOAK_Parser
-- File description:
-- ParseLLVM
--

module ParseLLVM where

import Parse
import LLVMType

import LLVM.AST as AST
import LLVM.AST.Type
import LLVM.AST.Global

import Data.ByteString.Char8 as BS
import Data.ByteString.Short


getFuncArgsEnd :: String -> String
getFuncArgsEnd [] = []
getFuncArgsEnd (x:xs) = if x == ')'
                        then []
                        else x:getFuncArgsEnd xs

getFuncArgs :: String -> String
getFuncArgs [] = []
getFuncArgs (x:xs) = if x == '('
                 then getFuncArgsEnd xs
                 else getFuncArgs xs

parseArgs :: String -> [Parameter]
parseArgs [] = []
parseArgs str@(x:xs) = case varType of
                       Just varType -> case fst varType of
                                      "int" -> Parameter int (Name $ toShort $ BS.pack $ fst name) [] : parseArgs (snd name)
                                      "double" -> Parameter LLVM.AST.Type.double (Name $ toShort $ BS.pack $ fst name) [] : parseArgs (snd name)
                                      _ -> parseArgs xs
                       Nothing -> []
                       -- THROW ERROR HERE ?
  where
      Just name = runParser (parseSome (parseAnyChar (['a' .. 'z'] ++ ['A' .. 'Z']))) str
      varType = if Prelude.head (snd name) == ':'
                then runParser (parseSome (parseAnyChar (['a'..'z'] ++ ['A'..'Z']))) (Prelude.tail $ snd name)
                else Just ("Error", "")

parseFunc :: String -> Definition
parseFunc str = GlobalDefinition functionDefaults
  {
    name = Name $ toShort $ BS.pack $ fst fName
    , parameters = (parseArgs $ getFuncArgs str, False)
  }
  where
    Just fName = runParser (parseSome ( parseAnyChar (['a'..'z'] ++ ['A'..'Z']))) str
