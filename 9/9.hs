import Data.List


solve1' pre@(_:px) (x:xs) = if isNotAllowed then x else solve1' (px++[x]) (xs)
  where isNotAllowed = [a+b | a <- pre, b <- pre, a+b == x] == []

solve1 :: [Int] -> Int
solve1 lst = solve1' (take 25 lst) (drop 25 lst) 


takeRange start len = take len . drop start

--solve2 :: [Int] -> [Int]

--solve2 lst = map (\range -> (minimum range + maximum range)) $ filter (\range -> sum range == invalidN) ranges 
--  where invalidN = solve1 lst
--        len = length lst
--        ranges = [range | s <- [0..len], r <- [2..len-s], let range = take r $ drop s lst]

solve2' arr@(_:xs) value = if s == value then maximum a + minimum a else solve2' xs value
  where (s,a) = foldl (\(s,arr) b -> if s == value then (s,arr) else (b+s, b:arr)) (0,[]) arr

solve2 lst = solve2' lst $ solve1 lst

-- parse

parseLine :: String -> Int
parseLine = read

parse :: String -> [Int]
parse = map parseLine . lines

-- test 

test1 = [35,20,15,25,47,40,62,55,65,95,102,117,150,182,127,219,299,277,309,576]
test1a = 127

test2 = [35,20,15,25,47,40,62,55,65,95,102,117,150,182,127,219,299,277,309,576]
test2a = 62

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
  --putStrLn $ "answer 1 correct: " ++ show (t1 == test1a) ++ " ("++ show t1 ++", expecting: "++ show test1a++")"
  --putStrLn $ "answer 2: " ++ show t2 
  --putStrLn $ "answer 2 correct: " ++ show (t2 == test2a) ++ " ("++ show t2 ++", expecting: "++ show test2a++")"
  --putStrLn ""
  a1 <- run solve1
  a2 <- run solve2
  putStrLn $ "solve1: " ++ show a1
  putStrLn $ "solve2: " ++ show a2
  return ()
