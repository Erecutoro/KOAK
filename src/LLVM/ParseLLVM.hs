--
-- EPITECH PROJECT, 2022
-- KOAK_Parser
-- File description:
-- ParseLLVM
--

module LLVM.ParseLLVM where
import LLVMType
import LLVM.AST as AST
import LLVM.AST.Type
import LLVM.AST.Global

import Data.ByteString.Char8 as BS
import Data.ByteString.Short

import Parse
import LLVM.GenCode

getFuncArgsEnd :: String -> String
getFuncArgsEnd [] = []
getFuncArgsEnd (x:xs) = if x == ')' && x /= ':'
                        then []
                        else x:getFuncArgsEnd xs

getFuncArgs :: String -> String
getFuncArgs [] = []
getFuncArgs (x:xs) = if x == '('
                 then getFuncArgsEnd xs
                 else getFuncArgs xs

getAlpha :: String -> Maybe (String, String)
getAlpha = runParser (parseSome (parseAnyChar (['a' .. 'z'] ++ ['A' .. 'Z'])))

getVarName :: String -> (String, String)
getVarName str@(x:xs) = case getAlpha str of
              Just a -> a
              Nothing -> ("", xs)

getVarType :: (String, String) -> Maybe (String, String)
getVarType name = if fst name /= "" && snd name /= [] && Prelude.head (snd name) == ':'
              then getAlpha (Prelude.tail $ snd name)
              else Just ("Unknow", "")

parseArgs :: String -> [Parameter]
parseArgs [] = []
parseArgs str@(x:xs) = case getVarType name of
                       Just varType -> case fst varType of
                                      "int" -> Parameter int (Name $ toShort $ BS.pack $ fst name) [] : parseArgs (snd name)
                                      "double" -> Parameter LLVM.AST.Type.double (Name $ toShort $ BS.pack $ fst name) [] : parseArgs (snd name)
                                      _ -> parseArgs xs
                       Nothing -> []
                       -- THROW ERROR HERE ?
  where
    name = getVarName str

getReturnType :: String -> Type
getReturnType [] = int -- THROW ERROR HERE ?
getReturnType (x:xs) = if x == ':'
                       then 
                          case fst test of
                            "int" -> int
                            "double" -> double
                            _ -> int -- THROW ERROR HERE ?
                       else getReturnType xs
  where
    test = case getAlpha xs of
           Just a -> a
           Nothing -> ("", "")

parseFunc :: (String, String) -> Definition
parseFunc (proto, func) = GlobalDefinition functionDefaults
  {
    name = Name fName
    , parameters = (parseArgs $ getFuncArgs $ snd funcName, False)
    , returnType = getReturnType $ snd funcName
    , basicBlocks = [genBlocks func fName]
  }
  where
    Just funcName = getAlpha proto
    fName = toShort $ BS.pack $ fst funcName
