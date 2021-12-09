#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2021/day/9
-- Your first goal is to find the low points - the locations
-- that are lower than any of its adjacent locations.

-- The risk level of a low point is 1 plus its height.

-- Find all of the low points on your heightmap. What is the sum of
-- the risk levels of all low points on your heightmap?

import Data.Char (digitToInt)
import Data.List (minimumBy)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, mapMaybe)
import System.Exit (exitFailure)
import Text.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec (Parser, char, digit, many1, parse, sepEndBy1)

type Coord = (Int, Int)

type Heightmap = M.Map Coord Int

matrixParser :: Parser [[Int]]
matrixParser = sepEndBy1 (many1 (digitToInt <$> digit)) $ char '\n'

heightmapParser :: Parser Heightmap
heightmapParser = makeHeightmap <$> matrixParser

makeHeightmap :: [[Int]] -> Heightmap
makeHeightmap = M.fromList . concat . indexed
  where
    indexed matrix = [[((x, y), v) | (x, v) <- zip [0 ..] line] | (y, line) <- zip [0 ..] matrix]

adjusted :: Coord -> [Coord]
adjusted (x, y) = [(x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], not (dy == 0 && dx == 0)]

neighbors :: Heightmap -> Coord -> [Int]
neighbors m c = mapMaybe (`M.lookup` m) $ adjusted c

at :: Heightmap -> Coord -> Int
at m c = fromJust $ M.lookup c m

isLocalMinimum :: Heightmap -> Coord -> Bool
isLocalMinimum m c = all (at m c <=) $ neighbors m c

allMinimums :: Heightmap -> [Coord]
allMinimums m = filter (isLocalMinimum m) $ M.keys m

risk :: Heightmap -> Coord -> Int
risk m c = at m c + 1

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  heightmap <- parseInput heightmapParser
  print $ sum $ map (risk heightmap) $ allMinimums heightmap
