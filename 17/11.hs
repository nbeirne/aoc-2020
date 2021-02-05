{-# LANGUAGE MultiParamTypeClasses #-}

import Data.Set (Set)
import qualified Data.Set as S
----
--class CellularPoint p where
--  neighbors :: p -> [p]
--
--newtype Point2d = Point2d (Int,Int) deriving (Eq, Ord, Show)
--
--instance CellularPoint Point2d where
--  neighbors (Point2d (px,py)) = [Point2d (px + x, py + y) | x <- [1,0,-1], y <- [1,0,-1], (x,y) /= (0,0)]
--
--
--type Board = Set Point2d
--
--data Cell = E | O | F deriving (Show)
--data Board2d = Board2d [[Cell]]
--
--
--data BK c = BK [[c]]
--
--class CellularBoard b p where
--  points :: b -> [p]
--
--instance CellularBoard (BK c) (Point2d) where
--  points (BK b) = [Point2d (x,y) | y <- [0..length b], x <- [0..length (b !! 0)]]


--instance CellularBoard Board2d where
--pointsb (Board2d b) = [Point2d (x,y) | y <- [0..length b], x <- [0..length (b !! y)]]


-- takes a cell and surounding cells and returns a new cell state

type Point = (Int,Int)
type Cell = Int
type Neighbour = [Point]
type Board = [[Cell]]

points :: Board -> [(Cell,Point)]
points = undefined

toBoard :: [(Cell,Point)] -> Board
toBoard = undefined


neighbours :: Point -> Neighbour
neighbours = undefined

iterateCell :: Board -> (Cell,Neighbour) -> (Cell,Point)
iterateCell = undefined


iter :: Board -> Board
iter board = toBoard b'
  where pts = points board -- :: [(Cell,Point)]
        nb = map (\(c,p) -> (c,neighbours p)) pts -- [(Cell,[Point])]
        b' = map (iterateCell board) nb -- [(Cell,Point)]



solve1 _ = 0
solve2 _ = 0



-- set-based operations
--shouldKeep True nbs
--  | nbs == 2 || nbs == 3 = True
--  | otherwise = False
--shouldKeep False nbs
--  | nbs == 3 = True
--  | otherwise = False
--
--keepPoint :: (Ord p, CellularPoint p) => Set p -> p -> Bool
--keepPoint board point = shouldKeep isActive (length activeNeighbors)
--  where isActive = point `S.member` board
--        activeNeighbors = board `S.intersection` (S.fromList (neighbors point))
--
--iter :: (Ord p, CellularPoint p) => Set p -> Set p
--iter b = S.filter (keepPoint b) allPoints
--  where allPoints = S.unions (S.map (S.fromList . neighbors) b)
--
---- array-based operations
--changePoint :: [[Cell]] -> Cell -> Cell
--changePoint board point = E
--
--iterA :: [[Cell]] -> [[Cell]]
--iterA b = b
--
--
-- parse

--parse = S.unions . map buildLine . zip [0..] . map parseLine . lines
--buildLine (y,arr) = S.fromList [Point2d p | (p,v) <- zip [(x,y) | x <- [0..]] arr, v]
--parseLine = map (\c -> if c == '.' then False else True)
--
---- test 
--
--test1a = 37
--test2a = 26
--
---- boiler plate
--
run func = func . parse
runLoading file func = readFile file >>= return . run func
runInput f = runLoading "input" f

test name expected result = do
  if result == expected then 
                   putStrLn $ "test "++ name ++ " success. ("++show result++")"
                   else
                   putStrLn $ "test "++ name ++ " failure. "++show result++" expected "++show expected


testAll = do
  input <- readFile "test2"
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
--
