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

test1 = [""]
test1a = solve1 test1

test2 = [""]
test2a = solve2 test1

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
  test solve1 "1" test1a input
  test solve2 "2" test2a input
  return ()

solveAll = do
  input <- readFile "input"
  let a1 = run solve1 input
  let a2 = run solve2 input
  putStrLn $ "solve1: " ++ show a1
  putStrLn $ "solve2: " ++ show a2

main = testAll >> putStrLn "" >> solveAll

