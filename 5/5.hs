import Data.List

seatId s = (row s)*8 + col s
  where bin c = if c == 'F' || c == 'L' then 0 else 1
        row = convert . reverse . take 7 . map bin
        col = convert . reverse . drop 7 . map bin
        convert (x:xs) = x + 2*convert xs -- binary conversion
        convert [] = 0

solve1 = maximum . map seatId

solve2 lst = head $ [id | (id,id') <- zip [head lst'..last lst'] lst', id/=id']
  where lst' = sort $ map seatId lst

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
