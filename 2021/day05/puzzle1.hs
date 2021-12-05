#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2021/day/5
-- You come across a field of hydrothermal vents on the ocean floor!
-- These vents constantly produce large, opaque clouds, so it would be
-- best to avoid them if possible.

-- They tend to form in lines. Each line of vents is given as a line segment
-- in the format x1,y1 -> x2,y2 where x1,y1 are the coordinates of one end
-- the line segment and x2,y2 are the coordinates of the other end.

-- Consider only horizontal and vertical lines. At how many points do at least two lines overlap?

import Text.Parsec.Language (emptyDef)
import System.Exit (exitFailure)
import Data.List (foldl', intersect, sort, group)
import qualified Data.Set as S
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec (Parser, many1, parse, char)

type Line = (Coord, Coord)
type Coord = (Integer, Integer)

lexer = P.makeTokenParser emptyDef

lineParser :: Parser Line
lineParser = do
  p1 <- coordParser
  _ <- P.symbol lexer "->"
  p2 <- coordParser
  return $ makeLine p1 p2

coordParser :: Parser Coord
coordParser = do
  x <- P.natural lexer
  _ <- char ','
  y <- P.natural lexer
  return (x, y)

makeLine :: Coord -> Coord -> Line
makeLine p1 p2 | p1 <= p2 = (p1, p2)
makeLine p1 p2 = makeLine p2 p1

points :: Line -> [Coord]
points ((x1, y1), (x2, y2)) | x1 == x2 = [(x1, y) | y <- [y1..y2]]
points ((x1, y1), (x2, y2)) | y1 == y2 = [(x, y1) | x <- [x1..x2]]
points _ = []

allPoints :: [Line] -> [Coord]
allPoints = concatMap points

counts :: [Line] -> [Int]
counts = map length . group . sort . allPoints

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
        putStrLn $ "Error: " ++ show err
        exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
    lines <- parseInput $ many1 lineParser
    print $ length $ filter (>= 2) $ counts lines
