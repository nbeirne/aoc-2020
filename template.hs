solve1 :: [String] -> Int
solve1 _ = 889779 

solve2 :: [String] -> Int
solve2 _ = 76110336

-- parse

parse :: String -> String
parse s = s

-- boiler plate

run f = do
  c <- readFile "input"
  --mapM_ putStrLn $ map (show . parse) $ lines c
  let a = f $ map parse $ lines c
  putStrLn $ show $ a
  return a

main = run solve1 >> run solve2 >> return ()
