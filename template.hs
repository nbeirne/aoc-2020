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

run f = do
  c <- readFile "input"
  --mapM_ putStrLn $ map (show . parse) $ lines c
  let a = f $ parse c
  --putStrLn $ show $ a
  return a

main = do
  let t1 = solve1 test1
  let t2 = solve2 test2
  putStrLn $ "answer 1 correct: " ++ show (t1 == test1a) ++ " ("++ show t1 ++", expecting: "++ show test1a++")"
  putStrLn $ "answer 2 correct: " ++ show (t2 == test2a) ++ " ("++ show t2 ++", expecting: "++ show test2a++")"
  putStrLn ""
  a1 <- run solve1
  a2 <- run solve2
  putStrLn $ "solve1: " ++ show a1
  putStrLn $ "solve2: " ++ show a2
  return ()
