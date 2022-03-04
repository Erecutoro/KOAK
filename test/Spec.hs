import Data
import Parse
import Decoration_UT
import Test.HUnit

main :: IO Counts
main = do
        -- runTestTT myTest
        -- runTestTT dataTest
        runTestTT decorationTests

myTest :: Test
myTest = test [ assertEqual "Basic Test of right parserChar" (Just ('a',"bc")) (runParser (parseChar 'a') "abc"),
                assertEqual "Basic Test of parserChar returning Nothing" (Nothing) (runParser (parseChar 'z') "abcd"),
                assertEqual "Basic Test of right parserAnyChar" (Just ('a',"bcd")) (runParser (parseAnyChar "bca") "abcd"),
                assertEqual "Basic Test of parserAnyChar returning Nothing" (Nothing) (runParser (parseAnyChar "xyz") "abcd"),
                assertEqual "Basic Test of right parseOr" (Just('a',"bcd")) (runParser (parseOr (parseChar 'a') (parseChar 'b')) "abcd"),
                assertEqual "Basic Test of parseOr returning Nothing" (Nothing) (runParser (parseOr (parseChar 'a') (parseChar 'b')) "xyz"),
                assertEqual "Basic Test of right parseAnd" (Just(('a','b'),"cd")) (runParser (parseAnd (parseChar 'a') (parseChar 'b')) "abcd"),
                assertEqual "Basic Test of parseAnd returning Nothing" (Nothing) (runParser (parseAnd (parseChar 'a') (parseChar 'b')) "acd"),
                assertEqual "Basic Test of right parseAndWith" (Just("ab","cd")) (runParser (parseAndWith (\x y -> [x,y]) (parseChar 'a') (parseChar 'b')) "abcd"),
                assertEqual "Basic Test of right parseMany" (Just("    ","foobar")) (runParser (parseMany (parseChar ' ')) "    foobar"),
                assertEqual "Basic Test of right parseSome" (Just("42","foobar")) (runParser (parseSome (parseAnyChar ['0'..'9'])) "42foobar"),
                assertEqual "Basic Test of parseSome returning Nothing" (Nothing) (runParser (parseSome (parseAnyChar ['0'..'9'])) "foobar42"),
                assertEqual "Basic Test of right parseInt" (Just(42, "")) (runParser parseInt "42"),
                assertEqual "Basic Test of right parserTuple" (Just((123,456),"foo bar")) (runParser (parseTuple parseInt)  "(123,456)foo bar")]

dataTest :: Test
dataTest = test [ assertEqual "Data test of Expr Var" [Var "y" "none" Double Empty] (callParser ["y:double;"]),
                  assertEqual "Data test of Expr Var" [Var "y" "none" Double Empty, Var "x" "none" Custom Empty] (callParser ["y:double;", "x;"]),
                  assertEqual "Data test of Expr BinOp Eq" [BinOp Custom (Var "y" "none" Custom Empty) Eq (Var "none" "0" Custom Empty) Empty] (callParser ["y = 0;"]),
                  assertEqual "Data test of Expr BinOp Add" [BinOp Custom (Var "y" "none" Custom Empty) Add (Var "none" "0" Custom Empty) Empty] (callParser ["y + 0;"]),
                  assertEqual "Data test of Expr BinOp Sub" [BinOp Custom (Var "y" "none" Custom Empty) Sub (Var "none" "0" Custom Empty) Empty] (callParser ["y - 0;"]),
                  assertEqual "Data test of Expr BinOp Mul" [BinOp Custom (Var "y" "none" Custom Empty) Mul (Var "none" "0" Custom Empty) Empty] (callParser ["y * 0;"]),
                  assertEqual "Data test of Expr BinOp Div" [BinOp Custom (Var "y" "none" Custom Empty) Div (Var "none" "0" Custom Empty) Empty] (callParser ["y / 0;"]),
                  assertEqual "Data test of Expr Call" [Call "test" [BinOp Custom (Var "y" "none" Custom Empty) Add (Var "none" "3" Custom Empty) Empty] Empty] (callParser ["test(y+3);"]),
                  assertEqual "Data test of Expr Func" [Func "test" [Var "x" "none" Double Empty,Var "y" "none" Int Empty] Double
                                                        (BinOp Custom (Var "y" "none" Custom Empty) Mul
                                                        (BinOp Custom (Var "x" "none" Custom Empty) Add
                                                        (Call "call" [BinOp Custom (Var "x" "none" Custom Empty) Div
                                                        (Var "none" "3" Custom Empty) Empty] Empty) Empty) Empty) Empty]
                                                        (callParser ["def test(x:double, y:int):double y * x + call(x / 3);"]),
                  assertEqual "Data test of Expr State While" [Data.State While (Var "y" "none" Custom Empty) Inf (Var "none" "10" Custom Empty)
                                                                (BinOp Custom (Var "y" "none" Custom Empty) Eq
                                                                (BinOp Custom (Var "y" "none" Custom Empty) Mul
                                                                (Var "none" "3" Custom Empty) Empty) Empty) Empty]
                                                                (callParser ["while y < 10 do y = y * 3;"])]