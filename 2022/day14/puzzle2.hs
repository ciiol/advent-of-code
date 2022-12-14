#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2022/day/14#part2

-- Using your scan, simulate the falling sand until the source of
-- the sand becomes blocked. How many units of sand come to rest?

import Text.ParserCombinators.Parsec (Parser, parse, sepEndBy1, many1, try, string, char, digit)
import System.Exit (exitFailure)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S

type Coord = (Int, Int)
type Line = [Coord]
type Occupied = S.Set Coord

lineParser :: Parser Line
lineParser = sepEndBy1 ((,) <$> num <*> (char ',' *> num)) (try $ string " -> ")
  where
    num = read <$> many1 digit

allLinePoints :: Line -> [Coord]
allLinePoints [] = []
allLinePoints [_] = []
allLinePoints ((x1, y1):p2@(x2, y2):ps) = new ++ allLinePoints (p2:ps)
  where
    new = [(x, y) | x <- r x2 x1, y <- r y1 y2]
    r x1 x2 | x1 > x2 = [x2 .. x1]
    r x1 x2 = [x1 .. x2]

makeOccupied :: [Line] -> Occupied
makeOccupied = S.fromList . concatMap allLinePoints

lowerLimit :: Occupied -> Int
lowerLimit = maximum . map snd . S.toList

addCoord :: Coord -> Coord -> Coord
addCoord (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

tryAddCoord :: Occupied -> Coord -> Coord -> Maybe Coord
tryAddCoord s p1 p2 = tryAddCoord' s $ addCoord p1 p2
  where
    tryAddCoord' s p | S.member p s = Nothing
    tryAddCoord' _ p = Just p

simulateSand :: Occupied -> Coord -> Maybe Coord
simulateSand s c = simulateSand' $ mapMaybe (tryAddCoord s c) [(0, 1), (-1, 1), (1, 1)]
  where
    simulateSand' [] = Nothing
    simulateSand' (x:_) = Just x

isNotLower :: Int -> Coord -> Bool
isNotLower maxY (_x, y) = y <= maxY

traceSand :: Occupied -> Coord -> [Coord]
traceSand s c = c : maybe [] (traceSand s) (simulateSand s c)

simulate :: Int -> Occupied -> [Coord] -> [Occupied]
simulate _ _ [] = error "no more sand"
simulate _ s (c:_) | S.member c s = []
simulate maxY s (c:cs) = s : simulate maxY s' cs
  where
    end = last $ takeWhile (isNotLower (maxY + 1)) $ traceSand s c
    s' = S.insert end s

simulationLen :: [Line] -> Int
simulationLen l = length $ simulate maxY occupied (repeat (500, 0))
  where
    occupied = makeOccupied l
    maxY = lowerLimit occupied

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
  print $ simulationLen lines
