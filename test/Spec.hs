import Parse
import Test.HUnit

main :: IO Counts
main = runTestTT myTest

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