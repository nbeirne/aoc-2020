import Text.ParserCombinators.Parsec
import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Data.Char

data Tree = Value Int
          | Add Tree Tree
          | Mult Tree Tree
          deriving Show

eval :: Tree -> Int
eval (Value v) = v
eval (Add left right) = (eval left) + (eval right)
eval (Mult left right) = (eval left) * (eval right)

solve :: [Tree] -> Int
solve = sum . map eval

-- parsing tools

parseNumber = many1 digit >>= return . Value . read

brackets :: Parser a -> Parser a
brackets = between (char '(') (char ')')

parseAdd  = do { string "+"; return Add }
parseMult = do { string "*"; return Mult }

-- parse 1
parseOp =   parseAdd <|> parseMult
parseExpr1 = parseFactor1 `chainl1` parseOp
parseFactor1 = brackets parseExpr1 <|> parseNumber


parseLine1 l = v
  where (Right v) = parse parseExpr1 "(none)" (filter (not . isSpace) l)

parse1 = map parseLine1 . lines


-- parse 2

parseExpr2 = parseTerm2 `chainl1` parseMult
parseTerm2 = parseFactor2 `chainl1` parseAdd
parseFactor2 = brackets parseExpr2 <|> parseNumber

parseLine2 l = v
  where (Right v) = parse parseExpr2 "(none)" (filter (not . isSpace) l)

parse2 = map parseLine2 . lines


-- test 

test1a = 0
test2a = 0

test11 = ("11", 71,    "1 + 2 * 3 + 4 * 5 + 6")
test12 = ("12", 51,    "1 + (2 * 3) + (4 * (5 + 6))")
test13 = ("13", 26,    "2 * 3 + (4 * 5)")
test14 = ("14", 437,   "5 + (8 * 3 + 9 + 3 * 4 * 3)")
test15 = ("15", 12240, "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
test16 = ("16", 13632, "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")

test21 = ("21", 231,    "1 + 2 * 3 + 4 * 5 + 6")
test22 = ("22", 51,     "1 + (2 * 3) + (4 * (5 + 6))")
test23 = ("23", 46,     "2 * 3 + (4 * 5)")
test24 = ("24", 1445,   "5 + (8 * 3 + 9 + 3 * 4 * 3)")
test25 = ("25", 669060, "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
test26 = ("26", 23340,  "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")

-- boiler plate

--run func = undefined
--runLoading file func = readFile file >>= return . run func
--runInput f = runLoading "input" f

testT func (name, expected, input) = test name expected (func input)
test name expected result = do
  if result == expected then 
                   putStrLn $ "test "++ name ++ " success. ("++show result++")"
                   else
                   putStrLn $ "test "++ name ++ " failure. "++show result++" expected "++show expected


testAll = do
  --input <- readFile "test2"
  --let t1a = run solve1 input
  --let t2a = run solve2 input
  --putStrLn $ "test1 " ++ show t1a
  --putStrLn $ "test2 " ++ show t2a
  testT (solve . parse1) test11
  testT (solve . parse1) test12
  testT (solve . parse1) test13
  testT (solve . parse1) test14
  testT (solve . parse1) test15
  testT (solve . parse1) test16
  testT (solve . parse2) test21
  testT (solve . parse2) test22
  testT (solve . parse2) test23
  testT (solve . parse2) test24
  testT (solve . parse2) test25
  testT (solve . parse2) test26
  --test "1" test1a t1a
  --test "2" test2a t2a
  return ()

solveAll = do
  input <- readFile "input"
  let a1 = (solve . parse1) input
  let a2 = (solve . parse2) input
  putStrLn $ "solve1: " ++ show a1
  putStrLn $ "solve2: " ++ show a2

main = testAll >> putStrLn "" >> solveAll
--
