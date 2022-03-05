--
-- EPITECH PROJECT, 2021
-- Untitled (Workspace)
-- File description:
-- Parse
--

module Parse where

import Text.Read
import Control.Applicative
import GHC.Base (Float)
import Data

newtype Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

instance Applicative Parser where
    pure a = Parser $ \ str -> Just (a, str)
    p1 <*> p2 = Parser func where 
        func str = case runParser p1 str of
            Just (r1, str1) -> case runParser p2 str1 of
                Just (r2, str2) -> Just (r1 r2, str2)
                Nothing -> Nothing
            Nothing -> Nothing

instance Functor Parser where
    fmap fct parser = Parser func where
                        func str = case runParser parser str of
                                    Nothing -> Nothing
                                    Just (a, string) -> Just (fct a, string)

instance Alternative Parser where
    empty = Parser func where
                func str = Nothing
    p1 <|> p2 = Parser func where
                    func str = case runParser p1 str of
                        Just (r, str1) -> Just (r, str1)
                        Nothing -> runParser p2 str

parseChar :: Char -> Parser Char
parseChar a = Parser func where
              func [] = Nothing
              func (x:xs)
                    | a == x = Just (x, xs)
                    | otherwise = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar a = Parser func where
                    func [] = Nothing
                    func (b:bs)
                        | b `elem` a = Just(b, bs)
                        | otherwise = Nothing

parseOr :: Parser a -> Parser a -> Parser a
parseOr (Parser b) (Parser c) = Parser func where
                func x = case b x of
                    Just (r, xr) -> Just (r, xr)
                    Nothing -> c x

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd (Parser a) (Parser b) = Parser func where
                func x = case a x of
                    Just (r1, xs) -> case b xs of
                        Just (r2, xr) -> Just ((r1, r2), xr)
                        Nothing -> Nothing
                    Nothing -> Nothing

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith function (Parser a) (Parser b) = Parser func where 
                func x = case a x of
                    Just (r1, xs) -> case b xs of
                        Just (r2, xr) -> Just (function r1 r2, xr)
                        Nothing -> Nothing
                    Nothing -> Nothing

parseMany :: Parser a -> Parser [a]
parseMany (Parser a) = Parser func where
                    func [] = Just ([], [])
                    func (x:xs) = case a (x:xs) of
                        Just (r, xr) -> Just ([r] ++ jr, jx)
                            where Just (jr, jx) = runParser (parseMany (Parser a)) xs
                        Nothing -> Just ([], (x:xs))

parseSome :: Parser a -> Parser [a]
parseSome (Parser a) = Parser func where
                    func [] = Nothing
                    func (x:xs) = case a (x:xs) of
                        Just (r, xr) -> Just ([r] ++ jr, jx)
                            where Just (jr, jx) = runParser (parseMany (Parser a)) xs
                        Nothing -> Nothing

parseUInt :: Parser Int
parseUInt = Parser func where
                    func x = case runParser (parseSome (parseAnyChar ['0'..'9'])) x of
                        Just (r, xr) -> Just (read r :: Int, xr)
                        Nothing -> Nothing

parseInt :: Parser Int
parseInt = Parser func where
                    func x = case runParser (parseChar '-') x of
                        Just (_, xr1) -> case runParser parseUInt xr1 of
                            Just (r, xr2) -> Just (-r, xr2)
                            Nothing -> Nothing
                        Nothing -> runParser parseUInt x

parseFloat :: Parser Float
parseFloat = Parser func where
                    func x = case runParser parseInt x of
                            Just (r, xr1) -> case runParser (parseChar '.') xr1 of
                                Just (_, pr) -> case runParser (parseSome (parseAnyChar ['0'..'9'])) pr of
                                    Just (r2, xr2) -> Just (read (show r ++ "." ++ r2)::Float, xr2)
                                    Nothing -> Nothing
                                Nothing -> Just (fromIntegral r::Float, xr1)
                            Nothing -> Nothing

parseTuple :: Parser a -> Parser (a,a)
parseTuple (Parser a) = Parser func where
                    func x = case runParser (parseChar '(') x of
                        Just (_, par1) -> case a par1 of
                            Just (r1, x1) -> case runParser (parseChar ',') x1 of
                                Just (_, vir) -> case a vir of
                                    Just (r2, par2) -> case runParser (parseChar ')') par2 of
                                        Just (_, rest) -> Just ((r1, r2), rest)
                                        Nothing -> Nothing
                                    Nothing -> Nothing
                                Nothing -> Nothing
                            Nothing -> Nothing
                        Nothing -> Nothing

parseSpace :: Parser a -> Parser a
parseSpace = func where
        func str = parseMany (parseAnyChar [' ', '\t']) *> str <* parseMany (parseAnyChar [' ', '\t'])

---------------------------------------------Parser Koak------------------------------------------------

parseString :: Parser String
parseString = parseChar '\"' *> parseSome (parseAnyChar (['A'..'Z'] ++ ['a'..'z'])) <* parseChar '\"'

parseStr :: Parser String
parseStr = parseSome (parseAnyChar (['A'..'Z'] ++ ['a'..'z']))

parseNum :: Parser String
parseNum = parseSome (parseAnyChar ['0'..'9'])

------------------------------------------------------------

parseArgend :: String -> String -> String -> Maybe (String, String)
parseArgend [] c arg = Just (arg, c)
parseArgend (a:as) (b:bs) arg
            | a == b = parseArgend as bs (arg ++ [a])
            | otherwise = Nothing

parseArg :: String -> Parser String
parseArg str = Parser func where
    func [] = Nothing
    func a = parseArgend str a []

------------------------------------------------------------

parseType :: Parser Type
parseType = (pure Int <* parseArg "int") <|> (pure Double <* parseArg "double") <|> (pure Str <* parseArg "string") <|> (pure Custom)

parseNone :: Parser String
parseNone = Parser func where func a = Just("none", a)

parseName :: Parser Name
parseName = parseSpace parseStr <* parseChar ':' <|> parseStr <|> parseNone

parseVarInt :: Parser (Expr Undetermined)
parseVarInt = Var <$> parseSpace parseName <*> parseNum <*> parseSpace (pure Int) <*> pure Empty

parseVarFloat :: Parser (Expr Undetermined)
parseVarFloat = Var <$> parseSpace parseName <*> (parseAndWith (\x y -> x ++ y) (parseAndWith (\ x y -> x ++ [y]) parseNum (parseAnyChar ['.'])) parseNum) <*> parseSpace (pure Double) <*> pure Empty

parseVarString :: Parser (Expr Undetermined)
parseVarString = Var <$> parseSpace parseName <*> parseString <*> parseSpace (pure Str) <*> pure Empty 

parseVarNone :: Parser (Expr Undetermined)
parseVarNone = Var <$> parseSpace parseName <*> parseNone <*> parseSpace (pure Custom) <*> pure Empty

parseVar :: Parser (Expr Undetermined)
parseVar = parseVarFloat <|> parseVarInt <|> parseVarString <|> parseVarNone

parseArguments :: Parser (Expr Undetermined)
parseArguments = Var <$> parseSpace parseName <*> parseNone <*> parseSpace parseType <*> pure Empty

------------------------------------------------------------

parseAdd :: Parser Op
parseAdd = pure Add <* parseChar '+'

parseSub :: Parser Op
parseSub = pure Sub <* parseChar '-'

parseMul :: Parser Op
parseMul = pure Mul <* parseChar '*'

parseDiv :: Parser Op
parseDiv = pure Div <* parseChar '/'

parseEq :: Parser Op
parseEq = pure Eq <* parseChar '='

parseOp :: Parser Op
parseOp = parseAdd <|> parseSub <|> parseMul <|> parseDiv <|> parseEq

parseSubBinOp :: Parser (Expr Undetermined)
parseSubBinOp = parseCall <|> parseVar

parseBinOp :: Parser (Expr Undetermined)
parseBinOp = BinOp <$> parseSubBinOp <*> parseSpace parseOp <*> parseExpr <*> pure Empty

------------------------------------------------------------

parseSubCall :: Parser [Expr Undetermined]
parseSubCall = ((:) <$> (parseSpace parseExpr <* parseSpace (parseChar ',')) <*> parseSubCall)
            <|> (\a -> [a]) <$> (parseSpace parseExpr <* parseSpace (parseChar ')'))

parseCall :: Parser (Expr Undetermined)
parseCall = Call <$> (parseStr <* parseChar '(') <*> parseSubCall <*> pure Empty

------------------------------------------------------------

parseFuncArg :: Parser [Expr Undetermined]
parseFuncArg =  ((:) <$> (parseSpace parseArguments <* parseSpace (parseChar ',')) <*> parseSubCall)
            <|> (\a -> [a]) <$> (parseSpace parseArguments <* parseSpace (parseChar ')'))

parseFunc :: Parser (Expr Undetermined)
parseFunc = Func <$> name <*> param <*> return_type <*> parseSpace parseExpr <*> pure Empty
            where name = parseArg "def" *> parseSpace parseStr
                  param = parseChar '(' *> parseFuncArg
                  return_type = parseSpace (parseChar ':') *> parseType

------------------------------------------------------------

parseSup :: Parser Compare
parseSup = Sup <$ parseChar '>'

parseInf :: Parser Compare
parseInf = Inf <$ parseChar '<'

parseEqual :: Parser Compare
parseEqual = Equal <$ parseArg "=="

parseSupEq :: Parser Compare
parseSupEq = SupEq <$ parseArg ">="

parseInfEq :: Parser Compare
parseInfEq = InfEq  <$ parseArg "<="

parseCompare :: Parser Compare
parseCompare = parseSup <|> parseInf <|> parseEqual <|> parseSupEq <|> parseInfEq

parseStatement :: Parser Statement
parseStatement = While <$ parseArg "while"

parseState :: Parser (Expr Undetermined)
parseState = State <$> parseStatement <*> parseSpace parseExpr <*> condition <*> parseSpace parseExpr
                   <*> body <*> pure Empty
             where condition = parseCompare
                   body = parseArg "do" *> parseSpace parseExpr

------------------------------------------------------------

parseExpr :: Parser (Expr Undetermined)
parseExpr = parseState <|> parseFunc <|> parseBinOp <|> parseCall  <|> parseVar

------------------------------------------------------------

--abandonned
--parseExtern :: Parser (Expr Undetermined)
--parseExtern = Extern <$> parseName <* parseChar '(' <*> parseSfunc

------------------------------------------------------------

callParser :: [String] -> [Expr Undetermined]
callParser [] = []
callParser (x:xs) = case runParser (parseExpr <* parseChar ';') x of
                        Just (a,_) -> a : callParser xs
                        Nothing -> []