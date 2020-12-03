import Data.List.Split

count x = length . filter (==x)

isValid1 :: (Int,Int,Char,String) -> Bool
isValid1 (fst,lst,c,s) = any id [b | n <- [fst..lst], let b = (length $ filter (==c) s) == n]

isValid2 :: (Int,Int,Char,String) -> Bool
isValid2 (fst,lst,c,s) = (c1 == c && c2 /= c) || (c1 /= c && c2 == c)
  where c1 = s !! (fst-1)
        c2 = s !! (lst-1)

find f a = length $ [e | e <- a, let (_,_,_,s) = e, f e]

solve1 = find isValid1
solve2 = find isValid2

-- parse

parse :: String -> (Int,Int,Char,String) 
parse s = (read $ sn !! 0, read $ sn !! 1, s2 !! 1 !! 0, s1 !! 1)
  where s1 = splitOn ": " s
        s2 = splitOn " " $ s1 !! 0
        sn = splitOn "-" $ s2 !! 0


-- boiler plate

run f = do
  c <- readFile "input"
  --mapM_ putStrLn $ map (show . parse) $ lines c
  putStrLn $ show $ f $ map parse $ lines c

main = run solve1 >> run solve2 >> return ()

