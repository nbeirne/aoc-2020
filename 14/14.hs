import Data.Bits
import Data.List
import Data.List.Split
import qualified Data.Array as A
import qualified Numeric as N
import qualified Data.Sequence as S
import Data.Foldable (toList)
import Data.Char (intToDigit)
import qualified Data.Map as Map

type Mask = ([Int],[Int]) -- zeros, ones
data Ins = Mask Mask | Mem Int Int deriving Show
data State = State { mask :: Mask, mem :: Map.Map Int Int } deriving Show

sempty n = State ([],[]) Map.empty

performWrites map writes = (Map.fromList writes) `Map.union` map

showBin n = N.showIntAtBase 2 intToDigit n ""

doMask :: Mask -> Int -> Int
doMask (zeros,ones) val = (val .&. (binToDec zeros)) .|. (binToDec ones)

eval :: Ins -> State -> State
eval (Mask mask) (State _ mem) = State mask mem
eval (Mem idx val) (State mask mem) = State mask mem'
  where val' = doMask mask val
        mem' = performWrites mem [(idx,val')]



solve1 ins = sum mem
  where (State _ mem) = foldl (flip eval) (sempty 100000) ins


-- part 2


getCombos :: Mask -> [Mask]
getCombos mask@(zeros,ones) = map (\(zeros',ones') -> (reverse zeros', arrayOr ones (reverse ones'))) allMasks
  where allMasks = combos ([],[]) $ floatings mask

arrayOr :: [Int] -> [Int] -> [Int]
arrayOr a b = map (\(a,b) -> a .|. b) $ zip a b

-- get floating digits from ones and zeros (array of T/F values)
floatings ((x:xs),(y:ys)) = (if x/=y then True else False):floatings (xs,ys)
floatings _ = []

-- takes a list of "floating" numbers where true means its floating, and produces all combos
combos :: Mask -> [Bool] -> [Mask]
combos (zeros,ones) (False:xs) = combos (1:zeros,0:ones) xs
combos (zeros,ones) (True:xs) = combos (0:zeros, 0:ones) xs ++ combos (1:zeros,1:ones) xs
combos m [] = [m]


eval2 :: Ins -> State -> State
eval2 (Mask mask) (State _ mem) = State mask mem
eval2 (Mem idx val) (State mask mem) = State mask mem'
  where mem' = performWrites mem writes
        writes = zip (map (flip doMask idx) $ getCombos mask) [val,val..]

solve2 ins = Map.size mem
  where (State _ mem) = foldl (flip eval2) (sempty (47782878967)) ins

-- parse

binToDec :: [Int] -> Int
binToDec = sum . map (2^) . findIndices (==1) . reverse

parseMask :: String -> Ins
parseMask s = Mask (zeros, ones)
  where zeros = map (\c -> if c == '0' then 0 else 1) s
        ones = map (\c -> if c == '1' then 1 else 0) s

parseLine s = if k == "mask" then parseMask v else Mem (read idx) (read v)
  where (k:v:[]) = splitOn " = " s
        (i:rst:_) = splitOn "[" k
        (idx:_) = splitOn "]" rst

parse :: String -> [Ins]
parse = map parseLine . lines

-- test 

test1a = 165
test2a = 0

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
  input <- readFile "test"
  putStrLn $ "test1 " ++ show (run solve1 input)
  putStrLn $ "test2 " ++ show (run solve2 input)
  --test solve1 "1" test1a input
  --test solve2 "2" test2a input
  return ()

solveAll = do
  input <- readFile "input"
  let a1 = run solve1 input
  let a2 = run solve2 input
  putStrLn $ "solve1: " ++ show a1
  putStrLn $ "solve2: " ++ show a2

main = testAll >> putStrLn "" >> solveAll

