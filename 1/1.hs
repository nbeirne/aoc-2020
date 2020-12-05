-- naive solution

solve1 :: [Int] -> Int
solve1 lines = head [a*b | a <- lines, b <- reverse lines, a+b == 2020]

solve2 :: [Int] -> Int
solve2 lines = head [a*b*c | a <- lines, b <- lines, c <- lines, a+b+c == 2020]

-- more efficient answer. solve1 and solve2 work for the challenge, but this is making those answers better
pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = [(x, y) | y <- xs] ++ pairs xs
 
solve1' :: [Int] -> Int
solve1' list = head [a*b | (a,b) <- pairs list, a+b == 2020]

-- parse

parse :: String -> Int
parse = read

-- boiler plate

run f = do
  c <- readFile "input"
  --mapM_ putStrLn $ map (show . parse) $ lines c
  putStrLn $ show $ f $ map parse $ lines c

main = run solve1 >> run solve2 >> return ()

