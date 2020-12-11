#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/11
-- The seat layout fits neatly on a grid. Each position is either floor (.), an
-- empty seat (L), or an occupied seat (#). For example, the initial seat
-- layout might look like this:

--     If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
--     If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
--     Otherwise, the seat's state does not change.

-- Simulate your seating area by applying the seating rules repeatedly until
-- no seats change state. How many seats end up occupied?

import qualified Data.Map as M
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

relativeSeat :: Grid -> Coord -> Coord -> Maybe Seat
relativeSeat grid (dy, dx) (y, x) = M.lookup (y + dy, x + dx) grid

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
    evolveSeat pos Occupied | (occupiedAround grid pos) >= 4 = Empty
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
