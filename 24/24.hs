import Data.List
import Data.Set (Set)
import qualified Data.Set as S

data Dir = E | SE | SW | W | NW | NE deriving (Eq,Show)
type Point = (Int,Int)

go :: Dir -> Point -> Point
go E (x,y)  = (x+1,y)
go NE (x,y) = (x+1,y+1)
go SE (x,y) = (x,  y-1)

go W (x,y)  = (x-1,y)
go NW (x,y) = (x,  y+1)
go SW (x,y) = (x-1,y-1)

eval :: Point -> [Dir] -> Point
eval = foldr go 

findFlipped :: Eq a => a -> [a] -> [a]
findFlipped a lst
  | a `elem` lst = filter (/=a) lst
  | otherwise    = a:lst -- add a

solve1 :: [[Dir]] -> Int
solve1 lst = 
  let points = map (eval (0,0)) lst
   in length $ foldr findFlipped [] points

neighbors :: Point -> Set Point
neighbors p = S.fromList [p' | d <- [E,NE,SE,W,NW,SW], let p' = go d p]

isBlack :: Set Point -> Point -> Bool
isBlack pts pt
  | pt `S.member` pts = adj == 1 || adj == 2
  | otherwise = adj == 2
  where adj = S.size $ pts `S.intersection` neighbors pt -- get adjacent black tiles to point

iter :: Set Point -> Set Point -- list of black tiles
iter pts = S.filter (isBlack pts) allPoints
  where allPoints = S.unions $ S.map neighbors pts -- set of points to check

--solve2 :: [[Dir]] -> Int
solve2 lst = (map length $ iterate iter points) !! 100
  where points = S.fromList $ foldr findFlipped [] $ map (eval (0,0)) lst

-- parse

parseLine :: String -> [Dir]
parseLine [] = []
parseLine ('e':xs)     = E:parseLine xs
parseLine ('n':'e':xs) = NE:parseLine xs
parseLine ('s':'e':xs) = SE:parseLine xs
parseLine ('w':xs)     = W:parseLine xs
parseLine ('n':'w':xs) = NW:parseLine xs
parseLine ('s':'w':xs) = SW:parseLine xs

parse :: String -> [[Dir]]
parse = map parseLine . lines

-- test 

test1a = 10
test2a = 2208

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
  test solve1 "1" test1a input
  --test solve2 "2" test2a input
  return ()

solveAll = do
  input <- readFile "input"
  let a1 = run solve1 input
  let a2 = run solve2 input
  putStrLn $ "solve1: " ++ show a1
  putStrLn $ "solve2: " ++ show a2

main = testAll >> putStrLn "" >> solveAll

