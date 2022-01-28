--
-- EPITECH PROJECT, 2022
-- Untitled (Workspace)
-- File description:
-- Decoration_AST
--

-- data SHORT = CHAR Char
--             | VSHORT Integer

-- data INT = INTEGER Integer
--          | SHORT SHORT

type NAME = String 

type WEIGHT = Integer

data EXTENDED_TYPE a b = CHAR (WEIGHT, b) a
                    | SHORT (WEIGHT, b) a 
                    | INT (WEIGHT, b) a 
                    | LONG (WEIGHT, b) a
                    | DOUBLE (WEIGHT, b) a 
                    | FLOAT (WEIGHT, b) a 
                    | STRING (WEIGHT, b) a
                    deriving (Show)

