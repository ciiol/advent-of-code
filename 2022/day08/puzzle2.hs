#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2022/day/8#part2
-- The Elves have launched a quadcopter to generate a map with the height
-- of each tree (your puzzle input).

-- Each tree is represented as a single digit whose value is its height,
-- where 0 is the shortest and 9 is the tallest.

-- A tree is visible if all of the other trees between it and an edge of
-- the grid are shorter than it.

-- Consider your map; how many trees are visible from outside the grid?

import Text.ParserCombinators.Parsec (Parser, parse, many1, sepEndBy1, digit, char, count)
import System.Exit (exitFailure)
import Data.List (foldl', transpose)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Height = Int
type Coord = (Int, Int)
type Heightmap = M.Map Coord Height

matrixParser :: Parser [[Height]]
matrixParser = sepEndBy1 (many1 (read <$> count 1 digit)) $ char '\n'

makeHeightmap :: [[Height]] -> Heightmap
makeHeightmap = M.fromList . concat . indexed
  where
    indexed matrix = [[((x, y), v) | (x, v) <- zip [0 ..] line] | (y, line) <- zip [0 ..] matrix]

filterVisible :: Height -> [Height] -> [Height]
filterVisible m [] = []
filterVisible m (x:_) | x >= m = [x]
filterVisible m (x:xs) = x : filterVisible m xs

traceTo :: Heightmap -> (Int, Int) -> Coord -> [Height]
traceTo m _ k | not $ M.member k m = []
traceTo m (dx, dy) (x, y) = fromJust (M.lookup (x, y) m) : traceTo m (dx, dy) (x+ dx, y + dy)

trace :: Heightmap -> Coord -> [[Height]]
trace m p = [traceLeft p, traceRight p, traceUp p, traceDown p]
  where
    traceLeft = traceTo m (-1, 0)
    traceRight = traceTo m (1, 0)
    traceUp = traceTo m (0, -1)
    traceDown = traceTo m (0, 1)

makeScores :: Heightmap -> [Int]
makeScores m = map (score . trace m) $ M.keys m
  where
    visible t = filterVisible (head t) (tail t)
    score ts = product $ map (length . visible) ts

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  heightMap <- parseInput $ makeHeightmap <$> matrixParser
  print $ maximum $ makeScores heightMap
