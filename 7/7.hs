--import Data.Graph
import Text.ParserCombinators.Parsec
import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Control.Monad.Trans.Either as ET
import Control.Monad.Trans
import qualified Data.Set as Set

data Vertex = Vertex {
    name :: String,
    edges :: [(String,Int)]
} deriving Show


-- part 1

vertexForName vtx n = head [v | v <- vtx, name v == n]


isChildOf child (Vertex _ e) = child `elem` (map fst e)


-- get parents of the "goal" and recurse until there are no more parents
allParents :: [Vertex] -> String -> Set.Set String
allParents vtxs goal = Set.fromList $ directParents ++ [a | parent <- directParents, a <- Set.toList $ allParents vtxs parent]
  where directParents = map name $ filter (isChildOf goal) vtxs


solve1 vtxs = length $ allParents vtxs "shiny gold"

-- part 2

countAllChildren vtxs current = 1 + sum [cnt * (snd c) | c <- children, let cnt = countAllChildren vtxs (fst c)]
  where vtx = vertexForName vtxs current
        children = edges vtx

solve2 vtxs = countAllChildren vtxs "shiny gold" - 1


-- parse

inputParse :: GenParser Char st [Vertex]
inputParse = do
  result <- many vertex
  eof
  return (result)

vertex :: GenParser Char st Vertex
vertex = do
  key <- bag
  spaces >> string "contain" >> spaces
  e <- bagList <|> (string "no other bags" >> return [])
  char '.'
  char '\n'
  --return $ map (\(e,n) -> Edge key e n) e
  return $ Vertex (key) (e)

bag :: GenParser Char st String
bag = do
  attr <- many1 letter
  spaces
  color <- many1 letter
  string " bag" 
  optional $ char 's' 
  return (attr++" "++color)

bagList :: GenParser Char st [(String,Int)]
bagList = do
  n <- read <$> many1 digit -- apparently I can't parse an integer directly??
  spaces
  bag <- bag
  remaining <- remainingBagList
  return ((bag,n):remaining)

remainingBagList :: GenParser Char st [(String,Int)]
remainingBagList = do
  (string "," >> space >> bagList)
  <|> (return [])
  --return [(0,"hello")]



parse' :: String -> Either ParseError [Vertex]
parse' = parse inputParse "(unknown)"

-- boiler plate

run f = do
  r <- runEitherT $ run' f
  putStrLn $ either show show r
  return ()

run' f = do
  c <- lift $ readFile "input"
  a <- return $ parse' c
  --lift $ putStrLn $ show a -- show parse result
  hoistEither $ fmap f a

--main = run solve1 >> return ()
main = run solve1 >> run solve2 >> return ()
