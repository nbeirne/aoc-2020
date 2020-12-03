These are my not-so-good solutions to advent of code. Written in Haskell.

## Boiler Plate
I use the haskell interpreter `ghci` to find solutions. I change `parse` every day, and have a function which can be passed into `load`. 

```
solve1 :: [String] -> Int
solve1 _ = 889779 

solve2 :: [String] -> Int
solve2 _ = 76110336

-- parse

parse :: String -> String
parse s = s

-- boiler plate

run f = do
  c <- readFile "input"
  --mapM_ putStrLn $ map (show . parse) $ lines c
  putStrLn $ show $ f $ map parse $ lines c

main = run solve1 >> run solve2 >> return ()
```

Usage:
```
✔ ~/dev/aoc/1
09:38 $ ghci
GHCi, version 8.10.1: https://www.haskell.org/ghc/  :? for help
Prelude> :l 1
[1 of 1] Compiling Main             ( 1.hs, interpreted )
Ok, one module loaded.
*Main> run solve1
889779
*Main> run solve2
76110336
*Main>
```
