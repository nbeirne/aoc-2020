import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as Set

-- part 1

solve1 :: [[String]] -> Int
solve1 = sum . map (Set.size . Set.unions . map Set.fromList)

-- part 2

-- similar to Set.unions ([[1,2,3],[1,2,4]]  --> [1,2])
intersections set = foldl Set.intersection (head set) set

solve2 :: [[String]] -> Int
solve2 = sum . map (Set.size . intersections . map Set.fromList)
  

-- parse

parse :: [String] -> [[String]]
parse = splitOn [""]

-- boiler plate

run f = do
  c <- readFile "input"
  --mapM_ putStrLn $ map (show . parse) $ lines c
  let a = f $ parse $ lines c
  putStrLn $ show $ a
  return a

main = run solve1 >> run solve2 >> return ()
