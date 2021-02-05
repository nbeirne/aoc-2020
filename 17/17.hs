import Data.Set (Set)
import qualified Data.Set as S

type Point3d = (Int,Int,Int)
type Point4d = (Int,Int,Int,Int)
type Board3d = Set Point3d
type Board4d = Set Point4d

--class Cellular (b p) where
--  neighbors :: p -> [p]

class CellularPoint p where
  neighbors :: p -> [p]

newtype Point3 = Point3 (Int,Int,Int)
instance CellularPoint Point3 where
  neighbors (Point3 (px,py,pz)) = [Point3 (px + x,py + y, pz + z) | x <- [1,0,-1], y <- [1,0,-1], z <- [1,0,-1], (x,y,z) /= (0,0,0)]

newtype Point4 = Point4 (Int,Int,Int,Int)
instance CellularPoint Point4 where
  neighbors (Point4 (px,py,pz,pw)) = [Point4 (px + x,py + y, pz + z, pw + w) | x <- [1,0,-1], y <- [1,0,-1], z <- [1,0,-1], w <- [1,0,-1], (x,y,z,w) /= (0,0,0,0)]


keepPoint :: (Ord p, CellularPoint p) => Set p -> p -> Bool
keepPoint board point = shouldKeep isActive (length activeNeighbors)
  where isActive = point `S.member` board
        activeNeighbors = board `S.intersection` (S.fromList (neighbors point))

iter :: (Ord p, CellularPoint p) => Set p -> Set p
iter b = S.filter (keepPoint b) allPoints
  where allPoints = S.unions (S.map (S.fromList . neighbors) b)

-- TODO: make set of points which are 'True' instead of map

shouldKeep True nbs
  | nbs == 2 || nbs == 3 = True
  | otherwise = False
shouldKeep False nbs
  | nbs == 3 = True
  | otherwise = False


neighbors3d :: Point3d -> Set Point3d
neighbors3d (px,py,pz) = S.fromList [(px + x,py + y, pz + z) | x <- [1,0,-1], y <- [1,0,-1], z <- [1,0,-1], (x,y,z) /= (0,0,0)]

keepPoint3d :: Board3d -> Point3d -> Bool
keepPoint3d board point = shouldKeep isActive (length activeNeighbors)
  where isActive = point `S.member` board
        activeNeighbors = board `S.intersection` neighbors3d point

iter3d :: Board3d -> Board3d
iter3d b = S.filter (keepPoint3d b) allPoints
  where allPoints = S.unions (S.map neighbors3d b)
  
solve1 :: Board3d -> Int
solve1 = length . iter3d . iter3d . iter3d . iter3d . iter3d . iter3d 




neighbors4d :: Point4d -> Set Point4d

neighbors4d (px,py,pz,pw) = S.fromList [(px + x,py + y, pz + z, pw + w) | x <- [1,0,-1], y <- [1,0,-1], z <- [1,0,-1], w <- [1,0,-1], (x,y,z,w) /= (0,0,0,0)]

keepPoint4d :: Board4d -> Point4d -> Bool
keepPoint4d board point = shouldKeep isActive (length activeNeighbors)
  where isActive = point `S.member` board
        activeNeighbors = board `S.intersection` neighbors4d point

iter4d :: Board4d -> Board4d
iter4d b = S.filter (keepPoint4d b) allPoints
  where allPoints = S.unions (S.map neighbors4d b)

solve2 :: Board3d -> Int
solve2 b = length $ iter4d $ iter4d $ iter4d $ iter4d $ iter4d $ iter4d b'
  where b' = S.map (\(x,y,z) -> (x,y,z,0)) b

-- parse

buildLine (y,arr) = S.fromList [p | (p,v) <- zip [(x,y,0) | x <- [0..]] arr, v]
parseLine = map (\c -> if c == '.' then False else True)

parse :: String -> Board3d
parse = S.unions . map buildLine . zip [0..] . map parseLine . lines

-- test 

test1a = 112

test2a = 848

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
  test solve2 "2" test2a input
  return ()

solveAll = do
  input <- readFile "input"
  let a1 = run solve1 input
  let a2 = run solve2 input
  putStrLn $ "solve1: " ++ show a1
  putStrLn $ "solve2: " ++ show a2

main = testAll >> putStrLn "" >> solveAll

