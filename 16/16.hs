import Data.List.Split
import Data.List

type Ticket = [Int]

type Range = (Int,Int)
data Rule = Rule { name :: String, range1 :: Range, range2 :: Range } deriving (Show, Eq)
type Rules = [Rule]

isInRange :: Range -> Int -> Bool
isInRange (min,max) i = i >= min && i <= max

removeValidValues :: Rule -> Ticket -> Ticket
removeValidValues (Rule _ range1 range2) ticket = 
  filter (\e -> not (isInRange range1 e || isInRange range2 e)) ticket

evalRules1 rules ticket = foldr removeValidValues ticket rules

solve1 (rules, _, tickets) = sum $ map (sum . evalRules1 rules) tickets


-- part 2 

ruleIsValid :: [Int] -> Rule -> Bool
ruleIsValid col (Rule _ range1 range2) = all (\e -> (isInRange range1 e || isInRange range2 e)) col

matchingRules :: [Int] -> [Rule] -> [Rule]
matchingRules col rules = filter (ruleIsValid col) rules

removeMatchingRules :: Rules -> ([Int], Rules) -> ([Int], Rules)
removeMatchingRules find (c,rules) = (c, filter (not . flip elem find) rules)

solveRec :: [([Int], Rules)] -> [(Rule, [Int])]
solveRec [] = [] 
solveRec lst = result ++ solveRec notMatching
  where result = map (\(col,r:[]) -> (r,col)) $ filter ((==) 1 . length . snd) lst
        notMatching = map (removeMatchingRules $ map fst result) $ filter ((/=) 1 . length . snd) lst

solve2 (rules, tic, tickets) = product $ map (\(_,Just i) -> tic !! i) $ filter (\(n,_) -> "departure" `isInfixOf` n) indexes
  where validTickets = filter (\t -> evalRules1 rules t == []) tickets
        cols = transpose validTickets
        matches = map (\c -> (c, matchingRules c rules)) cols
        solutions = solveRec matches
        indexes = map (\((Rule name _ _),n:_) -> (name,elemIndex n (tickets !! 0))) solutions

solveRec2 :: [([Int], Rules)] -> [([Int], Rules)]
solveRec2 [] = [] 
solveRec2 lst = result ++ solveRec2 notMatching
  where result = filter ((==) 1 . length . snd) lst
        notMatching = map (removeMatchingRules $ map snd result) $ filter ((/=) 1 . length . snd) lst

solve22 (rules, tic, tickets) = product $ map (\(_,Just i) -> tic !! i) $ filter (\(n,_) -> "departure" `isInfixOf` n) indexes
  where validTickets = filter (\t -> evalRules1 rules t == []) tickets
        cols = transpose validTickets
        matches = map (\c -> (c, matchingRules c rules)) cols
        solutions = solveRec matches
        indexes = map (\((Rule name _ _),n:_) -> (name,elemIndex n (tickets !! 0))) solutions



-- parse

parseRange :: String -> Range
parseRange s = ((split !! 0),(split !! 1))
  where split = (map read $ splitOn "-" s) :: [Int]

parseRule :: String -> Rule
parseRule s = Rule (split !! 0) (parseRange (nums !! 0)) (parseRange (nums !! 1))
  where split = splitOn ": " s
        nums = splitOn " or " $ split !! 1
  
parseTicket :: String -> Ticket
parseTicket = map read . splitOn ","

parse :: String -> (Rules, Ticket, [Ticket])
parse s = (rules, ticket, tickets)
  where l = splitOn [""] $ lines s
        rules = map parseRule $ l !! 0
        ticket = parseTicket (l !! 1 !! 1)
        tickets = map parseTicket (drop 1 $ l !! 2)

-- test 

test1a = 71
test2a = 0

-- boiler plate

run func = func . parse
runLoading file func = readFile file >>= return . run func
runInput f = runLoading "input" f

test func name expected input = do
  let a = run func input
  if a == expected then 
                   putStrLn $ "test "++ name ++ " success. ("++show a++")"
                   else
                   putStrLn $ "test "++ name ++ " failure. "++show a++" expected "++show expected


testAll = do
  input <- readFile "test"
  putStrLn $ "test1 " ++ show (run solve1 input)
  putStrLn $ "test2 " ++ show (run solve2 input)
  --test solve1 "1" test1a input
  --test solve2 "2" test2a input
  return ()

solveAll = do
  input <- readFile "input"
  let a1 = run solve1 input
  let a2 = run solve2 input
  putStrLn $ "solve1: " ++ show a1
  putStrLn $ "solve2: " ++ show a2

main = testAll >> putStrLn "" >> solveAll

