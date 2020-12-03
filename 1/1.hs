find1 :: [Int] -> Int
find1 lines = head [a*b | a <- lines, b <- reverse lines, a+b == 2020]

find2 :: [Int] -> Int
find2 lines = head [a*b*c | a <- lines, b <- lines, c <- lines, a+b+c == 2020]

parse :: String -> Int
parse = read

load f = do
  c <- readFile "input1"
  --mapM_ putStrLn $ map (show . parse) $ lines c
  return $ f $ map read $ lines c

main = undefined
