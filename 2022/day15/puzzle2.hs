#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2022/day/15#part2
-- The sensors report back their positions and closest beacons (your puzzle input).

-- Your handheld device indicates that the distress signal is coming from a beacon nearby.
-- The distress beacon is not detected by any sensor, but the distress beacon must have x
-- and y coordinates each no lower than 0 and no larger than 4000000.

-- To isolate the distress beacon's signal, you need to determine its tuning frequency, which can
-- be found by multiplying its x coordinate by 4000000 and then adding its y coordinate.

-- Find the only possible position for the distress beacon. What is its tuning frequency?

import Text.ParserCombinators.Parsec (Parser, parse, (<|>), many1, sepEndBy1, char, string, digit)
import System.Exit (exitFailure)
import qualified Data.Set as S

type Coord = (Int, Int)

lineParser :: Parser (Coord, Coord)
lineParser = do 
  _ <- string "Sensor at x="
  x1 <- numParser
  _ <- string ", y="
  y1 <- numParser
  _ <- string ": closest beacon is at x="
  x2 <- numParser
  _ <- string ", y="
  y2 <- numParser
  return ((x1, y1), (x2, y2))

numParser :: Parser Int
numParser = read <$> many1 (digit <|> char '-')

distance :: Coord -> Coord -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

findNearestUncovered :: (Coord -> Bool) -> [(Coord, Coord)] -> [Coord]
findNearestUncovered f lines = filter (not . isCovered) $ filter f $ concatMap around detectors
  where
    detectors = map makeDetector lines
    makeDetector (p1, p2) = (distance p1 p2, p1)
    around (d, p) = allPointsAtDistance (d + 1) p
    isCovered p = any (isCovered' p) detectors
    isCovered' p (d, s) = distance p s <= d

allPointsAtDistance :: Int -> Coord -> [Coord]
allPointsAtDistance d (x, y) = [(x + mx * dx, y + my * (d - dx)) | dx <- [1..d], mx <- [-1, 1], my <- [-1, 1]]

isInside :: Coord -> Coord -> Coord -> Bool
isInside (x1, y1) (x2, y2) (x, y) = between x x1 x2 && between y y1 y2
  where
    between x x1 x2 = x <= x2 && x >= x1

frequency :: Coord -> Integer
frequency (x, y) = fromIntegral x * 4000000 + fromIntegral y

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  lines <- parseInput $ sepEndBy1 lineParser $ char '\n'
  print $ frequency $ head $ findNearestUncovered (isInside (0, 0) (4000000, 4000000)) lines
