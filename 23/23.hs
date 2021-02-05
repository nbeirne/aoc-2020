import Data.List
import Data.List.Split
import qualified Data.IntMap.Strict as M

dest max 1 taken = if max `elem` taken then dest (max-1) 1 taken else max
dest max curr taken = if (curr-1) `elem` taken then dest max (curr-1) taken else curr-1
-- key: 0 1 2 3 4 5 6 7 8 9 
-- nxt: x 3 5 2 6 4 7 8 9 1

-- key: 0 1 2 3 4 5 6 7 8 9 
-- nxt: x 3 5 4 6 8 7 2 9 1
--            *   *   *


-- called with current=5
-- n1 = 4
-- n2 = 6
-- n3 = 7
-- n4 = 8
--
-- dst = 3 -- todo: find 
--
-- dN = map[dst]      (2)
-- map[dst] = n1      (map[3] = 4)
-- map[n3] = dN       (map[7] = 2) 
-- map[current] = n4  (map[5] = 8)


takeTurn' max (current,m) = 
  let n1 = m M.! current
      n2 = m M.! n1
      n3 = m M.! n2
      n4 = m M.! n3
      d = dest max current [n1,n2,n3]
      dN = m M.! d
      --m' = M.union (M.fromList [(d,n1),(n3,dN),(current,n4)]) m -- update "next" of the destination, and the last number in the sequence, and remove the lst its current position
      --m' = M.adjust (const n1) d $ M.adjust (const dN) n3 $ M.adjust (const n4) current $ m 
      m' = M.update (const $ Just n1) d $ M.update (const $ Just dN) n3 $ M.update (const $ Just n4) current $ m 
      --m' = M.insert n1 d $ M.insert dN n3 $ M.insert n4 current $ m 
  in (n4,m')

-- (3) 8  9  1  2  5  4  6  7
--
-- ->
-- 0 1 2 3 4 5 6 7 8 9
-- x 2 5 8 6 4 7 3 9 1
--
-- 2 5 4

makeMap :: [Int] -> (Int, M.IntMap Int)
makeMap lst = (head lst, M.fromList $ zip lst (drop 1 $ cycle lst))

unmakeMap :: (Int, M.IntMap Int) -> [Int]
unmakeMap (_, m) = take (M.size m) $ drop 1 $ iterate (\n -> m M.! n) 1


-- (5) 4  6  7  8  9  1  3  2   -->   (8) 9  1  3  4  6 7  2  5
--
-- curr =take 5 off
-- taken=take 4,6,7 off
-- rest =     8,9,1,3,2
--
-- calculate dest -- 5-1=4 (taken off). Try 3. 
-- dest = 3
--
-- m = splitWhen (==dest) rest                          --- EXPENSIVE? usually n
-- m = [[8,9,1],[2]]
-- 
-- r = m[0] ++ dest ++ taken ++ m[2] ++ curr            --- EXPENSIVE always n
-- r = 8 9 1 3 4 6 7 2 5
--
--
-- iterative 
-- 3 2 5 4 6 7 8 9 1
--     ^
--
--
--
-- 
-- 1: 2
-- 2: 3
-- 3: 1
--
-- (current): (next)


solve1 :: [Int] -> Int
solve1 lst = read $ concat $ map show solution'
  where solution = unmakeMap $ iterate (takeTurn' 9) (id $! makeMap lst) !! 100
        solution' = reverse $ drop 1 $ reverse solution

  --where solution = unmakeMap $ foldr (\_ a -> takeTurn' 9 a) (makeMap lst) [0..99]
        --solution' = reverse $ drop 1 $ reverse solution 

iter' :: (a -> a) -> a -> Int -> a
iter' _ a 0 = a
iter' f a n = iter' f (f a) (n-1)

--solve2 :: [Int] -> Int
solve2 n turns lst = product $ take 2 solution
    --where solution = unmakeMap $ iterate (takeTurn' n) lst' !! turns
    --where solution = unmakeMap $ iter' (takeTurn' n) lst' turns
    where solution = unmakeMap $ foldl' (\a _ -> takeTurn' n a) lst' [1..turns]
          solution' = take (4) solution
          --solution' = read $ concat $ map show $ reverse $ drop 1 $ reverse solution :: Int
          lst' = id $! makeMap $ lst ++ [10..n]
          --lst' = makeMap lst

n     = 1000000
turns = 10000000

-- parse

parse :: String -> [Int]
parse = map (read.(:[]))

-- test 

t1 = "389125467"
test1a = 0
test2a = 0

input = "398254716"

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
  putStrLn $ "test1 " ++ show (run solve1 t1)
  putStrLn $ "test2 " ++ show (run (solve2 n turns) t1)
  --test solve1 "1" test1a t1
  --test solve2 "2" test2a t1
  return ()

solveAll = do
  let a1 = run solve1 input
  let a2 = run (solve2 n turns) input
  putStrLn $ "solve1: " ++ show a1
  putStrLn $ "solve2: " ++ show a2

main = testAll >> putStrLn "" >> solveAll

