

data Dir = N | S | E | W deriving Show

data State = State { pos :: (Int,Int), wp :: (Int,Int), dir:: Dir } deriving Show
start = State (0,0) (10,1) E 

move :: Dir -> Int -> (Int,Int) -> (Int,Int)
move N m (x,y) = (x,y+m)
move E m (x,y) = (x+m,y)
move S m (x,y) = (x,y-m)
move W m (x,y) = (x-m,y)

rotate :: Int -> Dir -> Dir
rotate 0 d = d
rotate amount N = rotate (amount - 90) E  -- always rotate R
rotate amount E = rotate (amount - 90) S 
rotate amount S = rotate (amount - 90) W
rotate amount W = rotate (amount - 90) N

eval :: String -> State -> State
eval ('F':xs) (State pos wp d)   = State (move d (read xs) pos) wp d
eval ('R':xs) s@(State pos wp d) = State pos wp (rotate (read xs) d)
eval ('L':xs) s@(State pos wp d) = State pos wp (rotate (360-(read xs)) d)
eval ('N':xs) (State pos wp d)   = State (move N (read xs) pos) wp d
eval ('E':xs) (State pos wp d)   = State (move E (read xs) pos) wp d
eval ('S':xs) (State pos wp d)   = State (move S (read xs) pos) wp d
eval ('W':xs) (State pos wp d)   = State (move W (read xs) pos) wp d


rotateWp :: Int -> (Int,Int) -> (Int,Int)
rotateWp 90  (x,y) = (y, (-1) * x)
rotateWp 180 (x,y) = (-x,-y)
rotateWp 270 (x,y) = ((-1) * y, x)

solve1 lines = (abs x) + (abs y)
  where state = foldl (\s i-> (eval i (head s)):s) [(start)] lines
        (State (x,y) _ _) = head state

eval2 :: String -> State -> State
eval2 ('F':xs) (State (x,y) wp@(wx,wy) d) = State (x + (read xs)*wx, y + (read xs)*wy) wp d
eval2 ('R':xs) s@(State pos wp d) = State pos (rotateWp (read xs) wp) d
eval2 ('L':xs) s@(State pos wp d) = State pos (rotateWp (360 - read xs) wp) d
eval2 ('N':xs) (State pos wp d)   = State pos (move N (read xs) wp) d
eval2 ('E':xs) (State pos wp d)   = State pos (move E (read xs) wp) d
eval2 ('S':xs) (State pos wp d)   = State pos (move S (read xs) wp) d
eval2 ('W':xs) (State pos wp d)   = State pos (move W (read xs) wp) d


solve2 lines = (abs x) + (abs y)
  where state = foldl (\s i-> (eval2 i (head s)):s) [(start)] lines
        (State (x,y) _ _) = head state


-- parse

parseLine :: String -> String
parseLine s = s

parse :: String -> [String]
parse = map parseLine . lines

-- test 

test1a = 25
test2a = 286

-- boiler plate

run func = func . parse
runLoading file func = readFile file >>= return . run func
runInput f = runLoading "input" f

test func name expected input = do
  let a = run func input
  if a == expected then 
                   putStrLn $ "test "++ name ++ " success ("++show a++")"
                   else
                   putStrLn $ "test "++ name ++ " failure, "++show a++" expected "++show expected


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
