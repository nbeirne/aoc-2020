
-- part 1, original solution

findr' :: [String] -> Int -> Int  
findr' [] a = a
findr' (s:sx) a = findr' (map (drop 3) sx) (if (head s) == '#' then a+1 else a)

find1' :: [String] -> Int
find1' s = findr' (map cycle s) 0

-- part 2, solves 1 and 2

findr :: Int -> Int -> [String] -> Int
findr r d [] = 0
findr r d (s:sx) = (if (head s) == '#' then 1 else 0) + (findr r d $ map (drop r) $ drop d (s:sx))
  
solve1 :: [String] -> Int
solve1 s = findr 3 1 (map cycle s)

solve2 :: [String] -> Int
solve2 s = (findr 1 1 s') * (findr 3 1 s') * (findr 5 1 s') * (findr 7 1 s') * (findr 1 2 s')
  where s' = map cycle s

-- parse

parse s = s

-- boiler plate

run f = do
  c <- readFile "input"
  --mapM_ putStrLn $ map (show . parse) $ lines c
  putStrLn $ show $ f $ map parse $ lines c

main = run solve1 >> run solve2 >> return ()

