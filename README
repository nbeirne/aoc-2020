These are my not-so-good solutions to advent of code. 

## Boiler Plate
I use the haskell interpreter `ghci` to find solutions. I change `parse` every day, and have a function which can be passed into `load`. 

```
find1 :: String -> Int
find1 _ = 1

parse :: String -> String
parse s = s

load f = do
  c <- readFile "input"
  return $ f $ map parse $ lines c
  --mapM_ putStrLn $ map (show . parse) $ lines c

```

Usage:
```
✔ ~/dev/aoc/1
09:38 $ ghci
GHCi, version 8.10.1: https://www.haskell.org/ghc/  :? for help
Prelude> :l 1
[1 of 1] Compiling Main             ( 1.hs, interpreted )
Ok, one module loaded.
*Main> load find1
889779
*Main> load find2
76110336
*Main>
```
