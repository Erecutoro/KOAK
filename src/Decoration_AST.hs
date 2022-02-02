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


data VARIABLE =   Single (INDENTIFIER, TYPE) -- used for a variable
                | Multiple [(INDENTIFIER, TYPE)] -- used for a function/for loop
                | None -- just in case
                deriving (Show)

type DIGIT_TYPE = [TYPE] -- used for digits types in assignation or evalexpr