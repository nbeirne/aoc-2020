
data End = Loop Int | End Int deriving (Show,Eq)

data Op = 
    Nop Int
  | Acc Int
  | Jmp Int

data MState = MState { index :: Int,
                       value :: Int,
                       visited :: [Int]
                     }
mstateempty = MState 0 0 []

-- part 1

nextState :: Op -> MState -> MState
nextState (Nop _) (MState i a v) = (MState (i+1) a (i:v))
nextState (Acc b) (MState i a v) = (MState (i+1) (a+b) (i:v))
nextState (Jmp j) (MState i a v) = (MState (i+j) a (i:v))

eval :: [Op] -> MState -> End 
eval ops state@(MState i a v) 
  | i >= length ops = End a
  | i `elem` v = Loop a
  | otherwise = eval ops $ nextState op state
  where op = ops !! i

solve1 ops = eval ops mstateempty

-- part 2

swap (Nop n) = Jmp n
swap (Jmp n) = Nop n
swap o = o

transform fst (op:rest) = case a of
      Loop a -> transform (fst ++ [op]) rest
      a -> a
    where a = eval (fst ++ [swap op] ++ rest) mstateempty

solve2 = transform []

-- parse

readN :: String -> Int
readN ('+':xs) = read xs
readN xs = read xs

parseLine :: [String] -> Op
parseLine ("nop":n:[]) = Nop $ readN n
parseLine ("acc":n:[]) = Acc $ readN n
parseLine ("jmp":n:[]) = Jmp $ readN n

parse :: String -> [Op]
parse = map (parseLine . words) . lines

-- test 

test1 = [Nop 0, Acc 1, Jmp 4, Acc 3, Jmp (-3), Acc (-99), Acc 1, Jmp (-4), Acc 6]
test1a = Loop 5

test2 = [Nop 0, Acc 1, Jmp 4, Acc 3, Jmp (-3), Acc (-99), Acc 1, Jmp (-4), Acc 6]
test2a = End 8


-- boiler plate

run f = do
  c <- readFile "input"
  --mapM_ putStrLn $ map (show . parse) $ lines c
  let a = f $ parse c
  --putStrLn $ show $ a
  return a

main = do
  let t1 = solve1 test1
  let t2 = solve2 test2
  putStrLn $ "answer 1 is correct: " ++ show (t1 == test1a) ++ " ("++ show t1 ++")"
  putStrLn $ "answer 2 is correct: " ++ show (t2 == test2a) ++ " ("++ show t2 ++")"
  putStrLn ""
  a1 <- run solve1
  a2 <- run solve2
  putStrLn $ "solve1: " ++ show a1
  putStrLn $ "solve2: " ++ show a2
  return ()
