import Text.Read
import Data.List.Split
import Data.List

nextTime a b = if a `mod` b == 0 then a else ((a `div` b)+1) * b

--solve1 :: (Int,[Maybe Int]) -> Int
solve1 (time,lst) = id * (arrival-time)
  where lst' = [(next,n) | Just n <- lst, let next = nextTime time n]
        (arrival,id) = minimum lst'

--solve2 :: [String] -> Int
--solve2 input@(time,_) = solve1 input
solve2 (_,lst) = fst $ foldl fnd (0,1) $ lst'
  where lst' = [(n,i) | (Just n, i) <- zip lst [0..]]

fnd :: (Int,Int) -> (Int,Int) -> (Int,Int)
fnd (val,jump) (n,idx) = (head (filter (isOk idx n) [val, val+jump..])  , val*jump)

isOk :: Int -> Int -> Int -> Bool
isOk n t curr = n > 0 && ((curr + t) `mod` n) == 0


check2 lst m = b
  where gls = map (\(n,t) -> (n,t+m)) lst
        b = all (\(n, t) -> t `mod` n == 0) gls


--check2 b g = b `mod` g == 0

-- parse

parse :: String -> (Int,[Maybe Int])
parse s = (read $ lst !! 0, map readMaybe $ splitOn "," (lst !! 1))
  where lst = lines s

-- test 

test1a = 295

test2 = [Just 7, Just 13, Nothing, Nothing, Just 59, Nothing, Just 31, Just 19]
test2a = 1068781

test2_1 = [Just 17,Nothing,Just 13,Just 19]
test2_2 = [Just 67, Just 7, Just 59, Just 61]
test2_3 = [Just 67, Nothing, Just 7, Just 59, Just 61]
test2_4 = [Just 67, Just 7, Nothing, Just 59, Just 61]
test2_5 = [Just 1789, Just 37, Just 47, Just 1889]

test2_1a = 3417
test2_2a = 754018
test2_3a = 779210
test2_4a = 1261476
test2_5a = 1202161486
-- boiler plate

run func = func . parse
runLoading file func = readFile file >>= return . run func
runInput f = runLoading "input" f

test func name expected input = do
  let a = func input
  if a == expected then 
                   putStrLn $ "test "++ name ++ " success. ("++show a++")"
                   else
                   putStrLn $ "test "++ name ++ " failure. got "++show a++" but expected "++show expected


testAll = do
  input <- readFile "test"
  --putStrLn $ "test1 " ++ show (run solve1 input)
  --putStrLn $ "test2 " ++ show (run solve2 input)
  putStrLn $ show (run solve2 input)
  --test solve1 "1" test1a input
  --test solve2 "2" test2a input
  test solve2 "  1" test2a (1, test2)
  test solve2 "2_1" test2_1a (1, test2_1)
  test solve2 "2_2" test2_2a (1, test2_2)
  test solve2 "2_3" test2_3a (1, test2_3)
  test solve2 "2_4" test2_4a (1, test2_4)
  test solve2 "2_5" test2_5a (1, test2_5)
  return ()

solveAll = do
  input <- readFile "input"
  let a1 = run solve1 input
  let a2 = run solve2 input
  putStrLn $ "solve1: " ++ show a1
  putStrLn $ "solve2: " ++ show a2

main = testAll >> putStrLn "" >> solveAll

