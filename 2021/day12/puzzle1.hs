#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2021/day/12
-- Your goal is to find the number of distinct paths that start at start, end at end,
-- and don't visit small caves more than once. There are two types of caves: big caves
-- (written in uppercase, like A) and small caves (written in lowercase, like b).

-- How many paths through this cave system are there that visit small caves at most once?

import Data.Char (isLower, isUpper)
import Data.List (concatMap, deleteFirstsBy, foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe)
import System.Exit (exitFailure)
import Text.ParserCombinators.Parsec (Parser, alphaNum, char, many1, parse, sepEndBy1)

data Node = Small String | Large String deriving (Eq, Ord, Show)

type Graph = M.Map Node [Node]

nodeParser :: Parser Node
nodeParser = makeNode <$> many1 alphaNum

edgeParser :: Parser (Node, Node)
edgeParser = do
  a <- nodeParser
  _ <- char '-'
  b <- nodeParser
  return (a, b)

graphParser :: Parser Graph
graphParser = makeGraph <$> sepEndBy1 edgeParser (char '\n')

makeGraph :: [(Node, Node)] -> Graph
makeGraph = foldl' addNode M.empty
  where
    addNode g (n1, n2) = addNode' (addNode' g (n1, n2)) (n2, n1)
    addNode' g (n1, n2) = M.alter (Just . (n2 :) . fromMaybe []) n1 g

makeNode :: String -> Node
makeNode name | all isUpper name = Large name
makeNode name | all isLower name = Small name
makeNode _ = error "invalid node"

connected :: Graph -> Node -> [Node]
connected g k = fromJust $ M.lookup k g

pathes :: Graph -> Node -> Node -> [[Node]]
pathes g start end = pathes' g [start] end
  where
    next path = connected g $ head path
    pathes' g path end | head path == end = [reverse path]
    pathes' g path end = concatMap (\x -> pathes' g (x : path) end) (unvisited path)
    unvisited path = deleteFirstsBy visited (next path) path
    visited (Large _) (Large _) = False
    visited n1 n2 = n1 == n2

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  graph <- parseInput graphParser
  print $ length $ pathes graph (Small "start") (Small "end")
