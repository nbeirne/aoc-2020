import qualified Data.IntMap.Strict as M
import Data.IntMap.Strict (IntMap)
import Data.Maybe
import Data.List

nextIter :: (IntMap Int,Int) -> Int -> (IntMap Int,Int)
nextIter (m, last) turn 
  | next == 0 = (M.insert last turn m, next)
  | otherwise = (M.update (\_ -> Just turn)  last m, next)
  where next = fromMaybe 0 $ fmap ((-) turn) $ M.lookup last m

s1 end lst = snd $ foldl' nextIter (m,last lst) [(length lst)..(end-1)]
  where m = M.fromList $ zip (lst) [1,2..]

solve1 lst = s1 2020 lst 
solve2 lst = s1 30000000 lst
--solve2 lst = s1 lst [(length lst)..(29999999)]

-- parse

parse = id

-- test 

input = [20,0,1,11,6,3]

test1 = [0,3,6]
test1a = 436

test2 = [0,3,6]
test2a = 175594

-- boiler plate

run func = func . parse
runLoading file func = readFile file >>= return . run func
runInput f = runLoading "input" f

test func name expected input = do
  let a = run func input
  if a == expected then 
                   putStrLn $ "test "++ name ++ " success. ("++show a++")"
                   else
                   putStrLn $ "test "++ name ++ " failure. "++show a++" expected "++show expected


testAll = do
  --input <- readFile "test"
  putStrLn $ "test1 " ++ show (run solve1 test1)
  putStrLn $ "test2 " ++ show (run solve2 test2)
  --test solve1 "1" test1a input
  --test solve2 "2" test2a input
  return ()

solveAll = do
  let input = [20,0,1,11,6,3]
  --input <- readFile "input"
  let a1 = run solve1 input
  let a2 = run solve2 input
  putStrLn $ "solve1: " ++ show a1
  putStrLn $ "solve2: " ++ show a2

main = testAll >> putStrLn "" >> solveAll

