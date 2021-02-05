import Data.List.Split
import Data.Set (Set, (\\))
import qualified Data.Set as S


score = sum . map (\(a,b) -> a*b) . zip [1..] . reverse

eval1 :: [Int] -> [Int] -> Int
eval1 [] b = score b
eval1 a [] = score a
eval1 (a:as) (b:bs)
  | a > b = eval1 (as ++ [a, b]) bs
  | a < b = eval1 as (bs ++ [b, a])
  | otherwise = undefined

solve1 (a,b) = eval1 a b


eval2 p1@(a:as) p2@(b:bs) prev
  | (p1,p2) `S.member` prev = (p1,[]) -- player 1 wins
  | length p1 > a && length p2 > b = if (snd subgame == []) then aWin else bWin
  | a > b = aWin
  | a < b = bWin
    where aWin = eval2 (as++[a,b]) bs prev'
          bWin = eval2 as (bs++[b,a]) prev'
          prev' = S.insert (p1,p2) prev
          subgame = eval2 (take a as) (take b bs) S.empty
eval2 a b prev = (a,b)

solve2 (p1,p2) = score a + score b
  where (a,b) = eval2 p1 p2 S.empty

-- parse

parseLine :: String -> String
parseLine s = s

parse :: String -> ([Int],[Int])
parse str = (p1,p2)
  where s = splitOn [""] $ lines str
        p1 = map read $ drop 1 (s !! 0) 
        p2 = map read $ drop 1 (s !! 1)

-- test
test1 = eval1 [9,2,6,3,1] [5,8,4,7,10]
test1a = 306
test2 = solve2 ([9,2,6,3,1],[5,8,4,7,10])
test2a = 291

-- boiler plate

run func = func . parse
runLoading file func = readFile file >>= return . run func
runInput f = runLoading "input" f

test name expected result = do
  if result == expected then 
                   putStrLn $ "test "++ name ++ " success. ("++show result++")"
                   else
                   putStrLn $ "test "++ name ++ " failure. "++show result++" expected "++show expected


testAll = do
  --input <- readFile "test"
  let t1a = test1
  let t2a = test2
  putStrLn $ "test1 " ++ show t1a
  putStrLn $ "test2 " ++ show t2a
  test "1" test1a t1a
  test "2" test2a t2a
  return ()

solveAll = do
  input <- readFile "input"
  let a1 = run solve1 input
  let a2 = run solve2 input
  putStrLn $ "solve1: " ++ show a1
  putStrLn $ "solve2: " ++ show a2

main = testAll >> putStrLn "" >> solveAll

