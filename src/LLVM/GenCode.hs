--
-- EPITECH PROJECT, 2022
-- KOAK_Parser
-- File description:
-- genCode
--

module LLVM.GenCode where

import LLVM.AST
import Control.Monad.Except
import Data.ByteString.Char8 as BS
import Data.ByteString.Short

import LLVMType

--------------------------------------OP----------------------------------------




-------------------------------------START--------------------------------------

genBlocks :: String -> ShortByteString -> BasicBlock
genBlocks str name = BasicBlock (Name name) [] (Do $ Ret (Just (LocalReference int (Name $ toShort $ BS.pack "a"))) [])
