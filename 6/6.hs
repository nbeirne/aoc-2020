import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as Set

-- part 1

solve1 :: [[Set Char]] -> Int
solve1 = sum . map (Set.size . Set.unions)

-- part 2

-- similar to Set.unions ([[1,2,3],[1,2,4]]  --> [1,2])
intersections set = foldl Set.intersection (head set) set

solve2 :: [[Set Char]] -> Int
solve2 = sum . map (Set.size . intersections)
  
-- parse

parse :: [String] -> [[Set Char]]
parse = splitOn [Set.empty] . map Set.fromList

-- boiler plate

run f = do
  c <- readFile "input"
  --mapM_ putStrLn $ map (show . parse) $ lines c
  let a = f $ parse $ lines c
  putStrLn $ show $ a
  return a

main = run solve1 >> run solve2 >> return ()
