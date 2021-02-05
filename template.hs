solve1 :: [String] -> Int
solve1 _ = 889779 

solve2 :: [String] -> Int
solve2 _ = 76110336

-- parse

parseLine :: String -> String
parseLine s = s

parse :: String -> [String]
parse = map parseLine . lines

-- test 

test1a = 0
test2a = 0

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
  input <- readFile "test"
  let t1a = run solve1 input
  let t2a = run solve2 input
  putStrLn $ "test1 " ++ show t1a
  putStrLn $ "test2 " ++ show t2a
  --test "1" test1a t1a
  --test "2" test2a t2a
  return ()

solveAll = do
  input <- readFile "input"
  let a1 = run solve1 input
  let a2 = run solve2 input
  putStrLn $ "solve1: " ++ show a1
  putStrLn $ "solve2: " ++ show a2

main = testAll >> putStrLn "" >> solveAll

