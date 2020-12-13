import Data.Sort

solve1 lst = n1 * n3
  where 
        (n1,n3) = s1 0 lst (0,1) -- last adapter is always 3 higher
        s1 val (x:xs) (n1, n3)
          | x-val == 1 = s1 x xs (n1+1,n3)
          | x-val == 2 = s1 x xs (n1,n3)
          | x-val == 3 = s1 x xs (n1,n3+1)
          | otherwise  = (n1,n3+1)
        s1 val [] r = r


-- part 2
-- this is ugly but the approach:
-- 1. segment list into sections separated by 3. We can remove either number if their diff is 3, so we can split the problem into section + multiply the result
-- 2. Once we have segments, run an algorithm (s2) to get a list of possible combinations from the first elem to the lat elem (inclusive of both sides) where the difference of any two pairs is not greater than 3. 
-- 4. Get tthe length of possible combos for each segment, then multiply them. 

solve2 lst = product $ map (length .getSeg.reverse) $ segment lst 
  where segments = segment lst
        getSeg arr = s2 (head arr) (tail arr)

segment :: [Int] -> [[Int]]
segment = foldl foldf [[0]]
  where foldf a b 
          | diff == 3 = ([b]:a)
          | diff < 3 = ((b:head a):tail a)
          | otherwise = a -- throw away results when the diff > 3
          where diff = b-(head $ head a)

s2 val ([]) = [[val]]
s2 val (x:[]) = if x-val <= 3 then [[val,x]] else []
s2 val (x:xs)
  | x-val <= 2 = right ++ left
  | x-val == 3 = left -- we can't discard val
  | otherwise  = [] -- the jump is too much
  where left = map (val:) (s2 x xs) -- case where we include val
        right = (s2 val xs) -- case where we exclude val

-- parse

parseLine :: String -> Int
parseLine = read

parse :: String -> [Int]
parse = sort . map parseLine . lines

-- test 

--test1 = sort [16,10,15,5,1,11,7,19,6,12,4] -- 35
test1 = sort [28,33,18,42,31,14,46,20,48,47,24,23,49,45,19,38,39,11,1,32,25,35,8,17,7,9,4,2,34,10,3] -- 220
test1a = 220



test2 = sort [16,10,15,5,1,11,7,19,6,12,4,30] -- 35
test2a = 8
test2b = 19208

-- boiler plate

run f = do
  c <- readFile "input"
  --mapM_ putStrLn $ map (show . parse) $ lines c
  let a = f $ parse c
  --putStrLn $ show $ a
  return a

main = do
  let t1 = solve1 test1
  let t2 = solve2 test2
  let t2b = solve2 test1
  putStrLn $ "answer 1 correct: " ++ show (t1 == test1a) ++ " ("++ show t1 ++", expecting: "++ show test1a++")"
  putStrLn $ "answer 2 correct: " ++ show (t2 == test2a) ++ " ("++ show t2 ++", expecting: "++ show test2a++")"
  putStrLn $ "answer 2 correct: " ++ show (t2b == test2b) ++ " ("++ show t2b ++", expecting: "++ show test2b++")"
  putStrLn ""
  a1 <- run solve1
  a2 <- run solve2
  putStrLn $ "solve1: " ++ show a1
  putStrLn $ "solve2: " ++ show a2
  putStrLn $ "solve2: " ++ show t2
  return ()
