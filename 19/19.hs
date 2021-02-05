import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as M

import Text.ParserCombinators.Parsec
--import Text.Parsec
--import Text.Parsec.Prim
--import Text.Parsec.Combinator

import Data.List.Split
import Data.Either

-- list of numbered rules
-- rule may be: string | (ruleref, ruleref) |  (rule, rule)
-- there are no loops in the rules

data Rule = Concat [Int] | Or [Int] [Int] | Str Char deriving Show
--data Rules = IntMap Rule 
type Rules = IntMap Rule

eval :: Rules -> Rule -> String -> Bool
eval rules (Str c) (s:sx) = (s == c) 
eval rules (Concat []) _ = True
eval rules (Concat (r:rs)) = eval rules 

runRules :: Rules -> Rule -> Parser ()
runRules rules (Str c) = char c >> return ()
runRules rules (Concat []) = return ()
--runRules rules (Concat (r:rx)) = (&&) <$> runRules rules r' <*> runRules rules (Concat rx)
--  where r' = rules ! r
runRules rules (Concat (r:rx)) = ((runRules rules (rules ! r))) >> (try $ runRules rules (Concat rx))
runRules rules (Or a b) = (try $ runRules rules (Concat a))
                     <|>  (try $ runRules rules (Concat b))


r :: Rules -> String -> Either ParseError ()
r rules = parse (runRules rules (rules ! 0)) "(unknown)"



solve1 :: (IntMap Rule, [String]) -> Int
solve1 (rules,strs) = length $ rights $ map r strs
  where r s = parse (runRules rules (rules ! 0) <* eof) "(unknown)" s

--solve2 :: (IntMap Rule, [String]) -> Int
--solve2 = solve1

-- 244 - too low

-- parse
parseRule :: String -> (Int,Rule)
parseRule s = (,) n $ if length ch == 3 then Str (ch !! 1 !! 0) else 
                if length ors == 2 then Or (ors' !! 0) (ors' !! 1) else Concat (map read (words $ sp !! 1))
  where sp = splitOn ": " s
        n = read $ sp !! 0
        ors = splitOn " | " (sp !! 1)
        ch = splitOn "\"" (sp !! 1)
        ors' = map (map read . words) ors
        

parseLine :: String -> String
parseLine s = s

parse' :: String -> (IntMap Rule, [String])
parse' str = (M.fromList rules, strs)
  where ls = lines str 
        rules = map parseRule $ splitOn [""] ls !! 0
        strs = splitOn [""] ls !! 1


-- test 


r1 = M.fromList [(0, Concat [1,2]), (1, Str 'a'), (2, Or [1, 3] [3, 1]), (3, Str 'b')]

r2 = M.fromList [(0, Concat [4,1,5]), 
                 (1, Or [2,3] [3,2]), 
                 (2, Or [4,4] [5,5]), 
                 (3, Or [4,5] [5,4]),
                 (4, Str 'a'),
                 (5, Str 'b')]

r3 = M.fromList [ (0, Or [11,1] [1,12]) -- always works in first case. 
                , (1, Or [11,12] [12,11]) -- in [2,12] then [11,12]
                , (11, Str 'a')
                , (12, Str 'b')]


solve2 :: (IntMap Rule, [String]) -> Int
solve2 (rules,strs) = length $ filter id $ map (ps rules) strs 

ps rules s = (/=) [] $ rights [result | n <- [1..length s], m <- [1..length s], 
        let parser = count n (runRules rules r42) >> count m (runRules rules r42) >> count m (runRules rules r31) >> eof,
        let result = parse parser "()" s
            ]
  where r42 = rules ! 42
        r31 = rules ! 31

-- (42+)(42^n)(31^n) where n > 1
r4 = M.fromList [ (0, Concat [8, 31]) -- issue: aab does not parse. When we see 'b' we are in rule 8
                , (8, Or [42] [42, 8])
                , (42, Str 'a')
                , (31, Str 'b')]

bal :: Parser () -- rule 
bal = do
  ((try $ char 'a' >> char 'b') <|> (char 'a' >> bal >> char 'b')) >> return ()
  
aaas :: Parser () -- rule 8
aaas = do
  (try $ (char 'a' >> return ())) <|> (char 'a' >> aaas >> return ())

tst :: Parser ()
tst = do
  aaas >> bal >> return ()

tst2 :: Parser ()
tst2 = do
  aaas >> char 'b' >> return ()


-- a*(ab)
test1a = 2
test2a = 3
test22a = 12


-- boiler plate

run func = func . parse'
runLoading file func = readFile file >>= return . run func
runInput f = runLoading "input" f

testCase func (name, expected, input) = test name expected (func input)
test name expected result = do
  if result == expected then 
                   putStrLn $ "test "++ name ++ " success. ("++show result++")"
                   else
                   putStrLn $ "test "++ name ++ " failure. "++show result++" expected "++show expected


testAll = do
  input <- readFile "test"
  let t1a = run solve1 input
  t2a <- readFile "test2" >>= return . run solve1
  t22a <- readFile "test22" >>= return . run solve2
  putStrLn $ "test1 " ++ show t1a
  putStrLn $ "test2 " ++ show t2a
  putStrLn $ "test3 " ++ show t22a
  test "1" test1a t1a
  test "2" test2a t2a
  test "2" test22a t22a
  return ()

solveAll = do
  input <- readFile "input"
  let a1 = run solve1 input
  ----let a2 = run solve2 input
  a2 <- readFile "input2" >>= return . run solve2
  putStrLn $ "solve1: " ++ show a1
  putStrLn $ "solve2: " ++ show a2

main = testAll >> putStrLn "" >> solveAll

i2r = do
  f <- readFile "test22"
  let (rules, _) = parse' f
  return rules

