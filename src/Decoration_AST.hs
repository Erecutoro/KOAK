--
-- EPITECH PROJECT, 2022
-- Untitled (Workspace)
-- File description:
-- Decoration_AST
--

module Decoration_AST where

type INDENTIFIER = String

data EXT_TYPE =   Char 
            | Short  
            | Int  
            | Long  
            | Double  
            | Float 
            deriving (Show)


newtype VARIABLE = Varinfo (INDENTIFIER, EXT_TYPE) 

newtype CONTEXT = Ctx [VARIABLE]

type DIGIT_TYPE = [EXT_TYPE] -- used for digits types in assignation or evalexpr