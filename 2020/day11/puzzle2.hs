#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/11#part2
-- As soon as people start to arrive, you realize your mistake. People don't
-- just care about adjacent seats - they care about the first seat they
-- can see in each of those eight directions!

-- Also, people seem to be more tolerant than you expected: it now takes five or
-- more visible occupied seats for an occupied seat to become empty (rather than
-- four or more from the previous rules). The other rules still apply: empty seats
-- that see no occupied seats become occupied, seats matching no rule don't
-- change, and floor never changes.

-- Given the new visibility method and the rule change for occupied seats
-- becoming empty, once equilibrium is reached, how many seats end up occupied?

import qualified Data.Map.Strict as M
import Text.ParserCombinators.Parsec (Parser, many, parse, sepBy, newline, char, (<|>))
import System.Exit (exitFailure)
import Data.Maybe (mapMaybe)

data Seat = Empty | Occupied | Floor deriving (Show, Eq, Ord, Enum, Bounded)
type Grid = M.Map Coord Seat
type Coord = (Int, Int)

seatParser :: Parser Seat
seatParser =
  char '.' *> pure Floor
  <|> char '#' *> pure Occupied
  <|> char 'L' *> pure Empty

gridParser :: Parser [[Seat]]
gridParser = sepBy (many seatParser) newline

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

makeGrid :: [[Seat]] -> Grid
makeGrid = M.fromList . concat . map extractCoord . enumerate
  where
    extractCoord (j, row) = map (extractRowCoord j) $ enumerate row
    extractRowCoord j (i, x) = ((j, i), x)

addCoord :: Coord -> Coord -> Coord
addCoord (dy, dx) (y, x) = (y + dy, x + dx)

relativeSeat :: Grid -> Coord -> Coord -> Maybe Seat
relativeSeat grid pos diff = getSeat $ rayTrace diff
  where
    getSeat d = M.lookup (addCoord pos d) grid
    rayTrace d | isNotFloor d = d
    rayTrace d = rayTrace $ addCoord d diff
    isNotFloor d = maybe True (/= Floor) $ getSeat d

neighbours :: Grid -> Coord -> [Seat]
neighbours grid pos = mapMaybe (relativeSeat grid pos) diffs
  where
    diffs = [(-1, -1), (-1, 0), (-1, 1),
             ( 0, -1),          ( 0, 1),
             ( 1, -1), ( 1, 0), ( 1, 1)]

hasStateAround :: Seat -> Grid -> Coord -> Int
hasStateAround seat grid pos = length $ filter (== seat) $ neighbours grid pos

occupiedAround :: Grid -> Coord -> Int
occupiedAround = hasStateAround Occupied

evolve :: Grid -> [Grid]
evolve grid = newGrid : evolve newGrid
  where
    newGrid = M.mapWithKey evolveSeat grid
    evolveSeat pos Empty | (occupiedAround grid pos) == 0 = Occupied
    evolveSeat pos Occupied | (occupiedAround grid pos) >= 5 = Empty
    evolveSeat _ value = value

stable :: Grid -> Grid
stable grid = head stableGrids
  where
    stableGrids = map snd $ dropWhile isDifferent $ zip infEvolve $ tail infEvolve
    isDifferent (a, b) = a /= b
    infEvolve = evolve grid

totalOccupied :: Grid -> Int
totalOccupied = length . filter (== Occupied) . map snd . M.toList

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do
      input <- getContents
      return $ parse p "(stdin)" input

main :: IO ()
main = do
  seats <- parseInput gridParser
  putStrLn $ show $ totalOccupied $ stable $ makeGrid seats
