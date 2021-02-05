import Data.List

-- transform a subject number
-- value = 1, then LS times do:
--      value = value * subject
--      value = value % 20201227
--
-- handshake:
--    cpublic = transform 7 cloopsize       -- input 1
--    dprivate = transform 7 dloopsize      -- input 2
--
--    transmit two public keys (puzzle input)
--
--    enc = transform (dpublic) cloopsize = transform cpublic (dloopsize)
--
--
--

transforms :: Int -> [Int]
transforms subject = iterate (\v -> (v*subject) `mod` 20201227) 1 

transform subject loopsize = foldl' (\v _ -> (v*subject) `mod` 20201227) 1  [1..loopsize] -- dont use transforms because tail recursion

eqor c d = (||) <$> (==c) <*> (==d)

solve1 :: Int -> Int -> Int
solve1 cpublic dpublic = 
  let (idx, found) = head $ filter (eqor cpublic dpublic . snd) $ zip [0..] $ transforms 7 -- take first one we find
  in transform (if found == cpublic then dpublic else cpublic) idx
      

solve2 :: Int -> Int -> Int
solve2 cpublic dpublic = 889779 

-- test 
         -- 2885889
         -- 15992361
cpublic   = 11404017
dpublic   = 13768789


t_cpublic = 5764801
t_dpublic = 17807724

t_cloop = 8
t_dloop = 11

t_enc =  14897079

-- boiler plate

test func name expected input = do
  let a = func input
  if a == expected then 
                   putStrLn $ "test "++ name ++ " success. ("++show a++")"
                   else
                   putStrLn $ "test "++ name ++ " failure. "++show a++" expected "++show expected


testAll = do
  putStrLn $ "test1 " ++ show (solve1 t_cpublic t_dpublic)
  putStrLn $ "test2 " ++ show (solve2 t_cpublic t_dpublic)
  --test solve1 "1" test1a input
  --test solve2 "2" test2a input
  return ()

solveAll = do
  let a1 = solve1 cpublic dpublic
  let a2 = solve2 cpublic dpublic
  putStrLn $ "solve1: " ++ show a1
  putStrLn $ "solve2: " ++ show a2

main = testAll >> putStrLn "" >> solveAll

