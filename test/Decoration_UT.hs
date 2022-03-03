--
-- EPITECH PROJECT, 2022
-- Untitled (Workspace)
-- File description:
-- Decoration_UT
--

import Data
import Parse
import Decoration_AST
import Decoration
import Test.HUnit

decorationTests :: Test
decorationTests = test [assertEqual "Basic Variable function" "a" "a"]