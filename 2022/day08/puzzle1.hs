#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2022/day/8
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
import qualified Data.Set as S

type Height = Int
type Coord = (Int, Int)

matrixParser :: Parser [[Height]]
matrixParser = sepEndBy1 (many1 (read <$> count 1 digit)) $ char '\n'

findVisibleInRow :: [(Coord, Height)] -> [Coord]
findVisibleInRow [] = []
findVisibleInRow (x:xs) = fst $ foldl' findVisibleInRow' ([fst x], snd x) xs
  where
    findVisibleInRow' (acc, m) (c, h) | h > m = (c : acc, h)
    findVisibleInRow' (acc, m) _ = (acc, m)

allRays :: [[Height]] -> [[(Coord, Height)]]
allRays matrix = indexed ++ map reverse indexed ++ indexed' ++ map reverse indexed'
  where
    indexed = [[((x, y), v) | (x, v) <- zip [0 ..] line] | (y, line) <- zip [0 ..] matrix]
    indexed' = transpose indexed

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  heightMap <- parseInput matrixParser
  print $ S.size $ S.fromList $ concatMap findVisibleInRow $ allRays heightMap
