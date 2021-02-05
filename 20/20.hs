import Data.List
import Data.List.Split
import Text.Regex
import Text.Regex.Base

--data Tile = Tile { id :: Int, top :: String, right :: String, bottom :: String, left :: String } deriving (Eq, Show)
--rotations (Tile id top right bottom left) = [ Tile id top right bottom left
--                                            , Tile id (reverse left) top (reverse right) bottom 
--                                            , Tile id (reverse bottom) (reverse left) (reverse top) (reverse right)
--                                            , Tile id right (reverse bottom) left (reverse top)]
--flipT (Tile id top right bottom left) = Tile id bottom (reverse right) top (reverse left)

data Tile = Tile { id :: Int, elem :: [String] } deriving (Eq, Show)

top (Tile _ s) = head s
bottom (Tile _ s) = last s
left (Tile _ s) = head $ transpose s
right (Tile _ s) = last $ transpose s

rotations s = [ s
               , (reverse $ transpose s)
               , (map reverse $ transpose s)
               , (map reverse $ reverse $ s)]

symmetries s = rotations s ++ rotations (reverse s)

symmetries' (Tile id s) = map (Tile id) (symmetries s)

canFit :: [Tile] -> Tile -> Int -> Bool
canFit [] _ _ = True
canFit tiles tile width = True
  && (length tiles `mod` width == 0 || left tile == right leftTile)
  && (upIdx < 0 || upIdx >= length tiles || top tile == (bottom $ tiles !! upIdx))
 where idx = length tiles -- 
       leftTile = head tiles
       upIdx = width-1

compareId :: Tile -> Tile -> Bool
compareId a b = Main.id a == Main.id b

findSolution' :: [Tile] -> [Tile] -> Int -> [[Tile]]
findSolution' current [] _ = [current]
findSolution' current remaining width = [s 
                                        | tile <- possible
                                        , s <- findSolution' (tile:current) (filter (not . compareId tile) remaining) width]
  where possible = [tile' | tile <- remaining, tile' <- symmetries' tile, canFit current tile' width]

findSolution tiles = map (transpose . splitEvery width . reverse) $ findSolution' [] tiles width
  where width = round $ sqrt $ fromIntegral $ length tiles

--solve1 :: [Tile] -> Int
solve1 lst = a*b*c*d
  where ids = map (map (\(Tile id _) -> id)) $ head $ findSolution lst
        a = (ids !! 0) !! 0
        b = (ids !! 0) !! width
        c = (ids !! width) !! width
        d = (ids !! width) !! 0
        width = (round $ sqrt $ fromIntegral $ length lst) - 1

fixTile :: [String] -> [String]
fixTile = (init.tail) . map (init.tail)

concatLine :: [[String]] -> [String]
concatLine = map concat . transpose

--solve2 :: [Tile] -> [String]
solve2 lst = (length $ filter (=='#') $ concat fixed) - (s*15)
  where tiles = map (map (\(Tile _ s) -> s)) $ head $ findSolution lst :: [[[String]]]
        ids = map (map (\(Tile id _) -> id)) $ head $ findSolution lst
        width = (round $ sqrt $ fromIntegral $ length lst) - 1
        fixed = concat $ map (concatLine.(map (fixTile.transpose))) tiles :: [String]
        syms = symmetries fixed :: [[String]]
        s = sum $ map findMonster syms -- in ex its two


monster = [ mkRegex "^..................\\#."
          , mkRegex "^#....##....##....###"
          , mkRegex "^.#..#..#..#..#..#..."]
monstern = 15

fld = [ ".####...#####..#...###.."
      , "#####..#..#.#.####..#.#."
      , ".#.#...#.###...#.##.##.."
      , "#.#.##.###.#.##.##.#####"
      , "..##.###.####..#.####.##"
      , "...#.#..##.##...#..#..##"
      , "#.##.#..#.#..#..##.#.#.."
      , ".###.##.....#...###.#..."
      , "#.####.#.#....##.#..#.#."
      , "##...#..#....#..#...####"
      , "..#.##...###..#.#####..#"
      , "....#.##.#.#####....#..."
      , "..##.##.###.....#.##..#."
      , "#...#...###..####....##."
      , ".#.##...#.##.#.#.###...#"
      , "#.###.#..####...##..#..."
      , "#.###...#.##...#.######."
      , ".###.###.#######..#####."
      , "..##.#..#..#.#######.###"
      , "#.#..##.########..#..##."
      , "#.#####..#.#...##..#...."
      , "#....##..#.#########..##"
      , "#...#.....#..##...###.##"
      , "#..###....##.#...##.##.#"]


findMonster s@(_:xs) = findMonster' xs (s !! 0) (xs !! 1) (xs !! 2)

findMonster' [] _ _ _ = 0 -- we dont have enough rows left
findMonster' (x:xs) l0@(_:l0s) l1@(_:l1s) l2@(_:l2s)
  | matchRegex (monster !! 0) l0 /=Nothing && matchRegex (monster !! 1) l1 /=Nothing && matchRegex (monster !! 2) l2 /=Nothing = 1 + r
  | otherwise = r
  where r = findMonster' (x:xs) l0s l1s l2s
findMonster' (a:b:c:xs) [] [] [] = findMonster' (b:c:xs) a b c -- move down by one
findMonster' _ _ _ _ = 0 -- end recursion

-- parse

parseId :: String -> Int
parseId s = read n
  where n = (splitOn " " ((splitOn ":" s) !! 0)) !! 1

parseTile :: [String] -> Tile
parseTile lines = Tile id tile --(head tile) (last trans) (last tile) (head trans)
  where id = parseId (lines !! 0)
        tile = drop 1 lines
        trans = transpose tile

parse :: String -> [Tile]
parse s = map parseTile tiles
  where tiles = splitOn [""] $ lines s

-- test 

test1a = 20899048083289
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


t0 = Tile 0 ["...", "...", "..."]
t1 = Tile 1 [".##", ".##", ".##"] 
t2 = Tile 2 ["...", "...", "###"] 
t3 = Tile 3 [".##", ".##", "###"] 

testAll = do
  input <- readFile "test"
  let t1a = run solve1 input
  let t2a = run solve2 input
  putStrLn $ "test1 " ++ show t1a
  putStrLn $ "test2 " ++ show t2a
  --mapM_ (putStrLn.show) t2a
  --test "1" test1a t1a
  --test "2" test2a t2a
  return ()

solveAll = do
  input <- readFile "input"
  let a1 = run solve1 input
  let a2 = run solve2 input
  --putStrLn $ "solve1: " ++ show a1
  putStrLn $ "solve2: " ++ show a2

main = testAll >> putStrLn "" >> solveAll

