#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2022/day/15
-- The sensors report back their positions and closest beacons (your puzzle input).

-- Consult the report from the sensors you just deployed.
-- In the row where y=2000000, how many positions cannot contain a beacon?

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

listPointsAt :: Int -> Int -> Coord -> [Coord]
listPointsAt y maxDistance (px, py) | abs (y - py) > maxDistance = []
listPointsAt y maxDistance (px, py) = [(x, y) | x <- [(px - dx) .. (px + dx)]]
  where
    dx = maxDistance - abs (y - py) 

allImposiblePointsAt :: Int -> [(Coord, Coord)] -> [Coord]
allImposiblePointsAt y lines = S.toList $ flip S.difference allBeacons $ S.fromList $ concatMap listPointsAt' detectors
  where
    detectors = map makeDetector lines
    makeDetector (p1, p2) = (distance p1 p2, p1)
    listPointsAt' (d, p) = listPointsAt y d p
    allBeacons = S.fromList $ map snd lines

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
  print $ length $ allImposiblePointsAt 2000000 lines
