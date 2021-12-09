#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2021/day/9#part2
-- Next, you need to find the largest basins so you know what areas are most
-- important to avoid.

-- A basin is all locations that eventually flow downward to a single low point.
-- Therefore, every low point has a basin, although some basins are very small.
-- Locations of height 9 do not count as being in any basin, and all other locations
-- will always be part of exactly one basin.

-- The size of a basin is the number of locations within the basin, including the low point.
-- The example above has four basins.

-- What do you get if you multiply together the sizes of the three largest basins?

import Data.Char (digitToInt)
import Data.List (sort)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
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
adjusted (x, y) = [(x + dx, y) | dx <- [-1 .. 1], dx /= 0] ++ [(x, y + dy) | dy <- [-1 .. 1], dy /= 0]

neighbors :: Heightmap -> Coord -> [Coord]
neighbors m c = filter (`M.member` m) $ adjusted c

at :: Heightmap -> Coord -> Int
at m c = fromJust $ M.lookup c m

isLocalMinimum :: Heightmap -> Coord -> Bool
isLocalMinimum m c = all ((at m c <=) . at m) $ neighbors m c

allMinimums :: Heightmap -> [Coord]
allMinimums m = filter (isLocalMinimum m) $ M.keys m

basin :: Heightmap -> Coord -> [Coord]
basin m c = basin' $ S.fromList [c]
  where
    basin' coords | coords == next coords = S.toList coords
    basin' coords = basin' (next coords)
    next coords = foldl (flip S.insert) coords $ concatMap neighborsNot9 $ S.toList coords
    neighborsNot9 = filter ((/= 9) . at m) . neighbors m

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
  print $ product . take 3 . reverse . sort . map (length . basin heightmap) $ allMinimums heightmap
