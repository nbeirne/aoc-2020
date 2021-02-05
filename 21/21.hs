import Data.List (sort, intersperse)
import Data.List.Split
import Data.Set (Set, (\\))
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

type Dict = Map String (Set String)

addToDict :: Set String -> String -> Dict -> Dict
addToDict possible allergen dict
  | allergen `M.member` dict = M.insert allergen ((dict M.! allergen) `S.intersection` possible) dict
  | otherwise = M.insert allergen possible dict

processLine :: (Set String, Set String) -> Dict -> Dict -- for the fold function
processLine (words,allergens) dict = foldr (addToDict words) dict allergens

reduceDict :: Dict -> Dict
reduceDict dict = M.union singleWords dict'
  where singleWords = M.filter ((==1) . S.size) dict 
        singles = S.fromList $ M.elems $ M.map (S.elemAt 0) singleWords -- map single words to a set of 'unknowns'
        dict' = M.map (\s -> s \\ singles) dict -- remove single words

iter :: Dict -> Dict
iter d 
  | all ((==1) . length) (M.elems d) = d
  | otherwise = iter $ reduceDict d

solve1 lst = length $ filter (flip elem safe) $ concat (map (S.elems . fst) lst) -- take our solution and produce the result
  where words = S.unions $ map fst lst
        allergens = S.unions $ M.elems $ iter (foldr processLine M.empty lst) :: Set String
        safe = words \\ allergens -- safe ones

solve2 lst = concat $ intersperse "," $ map snd $ sort allergens
  where allergens = M.assocs $ M.map (S.elemAt 0) $ iter (foldr processLine M.empty lst) 

-- parse

parseLine :: String -> (Set String, Set String) -- list of words and ingredients
parseLine s = (S.fromList $ words (i!!0), S.fromList $ words (i!!1))
  where i = splitOn "|" s

parse = map parseLine . lines

-- test 

test1a = 0
test2a = 0

-- boiler plate

run func = func . parse
runLoading file func = readFile file >>= return . run func
runInput f = runLoading "input" f

test name expected result = do
  if result == expected then 
                   putStrLn $ "test "++ name ++ " success. ("++show result++")"
                   else
                   putStrLn $ "test "++ name ++ " failure. "++show result++" expected "++show expected


testAll = do
  input <- readFile "test"
  let t1a = run solve1 input
  let t2a = run solve2 input
  putStrLn $ "test1 " ++ show t1a
  putStrLn $ "test2 " ++ show t2a
  --test "1" test1a t1a
  --test "2" test2a t2a
  return ()

solveAll = do
  input <- readFile "input"
  let a1 = run solve1 input
  let a2 = run solve2 input
  putStrLn $ "solve1: " ++ show a1
  putStrLn $ "solve2: " ++ show a2

main = testAll >> putStrLn "" >> solveAll

