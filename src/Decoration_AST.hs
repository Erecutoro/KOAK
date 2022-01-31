--
-- EPITECH PROJECT, 2022
-- Untitled (Workspace)
-- File description:
-- Decoration_AST
--

type INDENTIFIER = String

data TYPE =   Char Char 
            | Short Integer 
            | Int Integer 
            | Long Integer 
            | Double Double 
            | Float Float 
            deriving (Show)

type VARIABLE = (INDENTIFIER, TYPE)

type DIGIT_TYPE = [TYPE]