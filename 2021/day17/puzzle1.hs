#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2021/day/17
-- The probe launcher on your submarine can fire the probe with any integer
-- velocity in the x (forward) and y (upward, or downward if negative) directions.

-- The probe's x,y position starts at 0,0. Then, it will follow some trajectory by moving
-- in steps. On each step, these changes occur in the following order:

--     The probe's x position increases by its x velocity.
--     The probe's y position increases by its y velocity.
--     Due to drag, the probe's x velocity changes by 1 toward the
--       value 0; that is, it decreases by 1 if it is greater than 0,
--       increases by 1 if it is less than 0, or does not change if it is already 0.
--     Due to gravity, the probe's y velocity decreases by 1.

-- For the probe to successfully make it into the trench, the probe must be on some
-- trajectory that causes it to be within a target area after any step. The submarine
-- computer has already calculated this target area (your puzzle input).

-- What is the highest y position it reaches on this trajectory?

import Data.List (sort)
import Data.Maybe (catMaybes, fromJust)
import System.Exit (exitFailure)
import Text.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec (Parser, alphaNum, char, many1, parse, string)
import qualified Text.ParserCombinators.Parsec.Token as P

type Coord = (Integer, Integer)

data Area = Area Coord Coord deriving (Show)

lexer = P.makeTokenParser emptyDef

inputParser :: Parser Area
inputParser = do
  _ <- string "target area: x="
  x1 <- P.integer lexer
  _ <- string ".."
  x2 <- P.integer lexer
  _ <- string ", y="
  y1 <- P.integer lexer
  _ <- string ".."
  y2 <- P.integer lexer
  return $ Area (min x1 x2, min y1 y2) (max x1 x2, max y1 y2)

fire :: Coord -> Integer -> Integer -> [Coord]
fire c@(x, y) dx dy = c : fire (x + dx, y + dy) (updateDx dx) (updateDy dy)
  where
    updateDx dx | dx > 0 = dx - 1
    updateDx dx | dx < 0 = dx + 1
    updateDx dx = dx
    updateDy dy = dy - 1

findHit :: Area -> [Coord] -> Maybe Coord
findHit a [] = Nothing
findHit (Area (x1, y1) (x2, y2)) (p@(x, y) : _ps) | x >= x1 && y >= y1 && x <= x2 && y <= y2 = Just p
findHit (Area (x1, _y1) p2@(x2, y2)) (p@(x, y) : ps) | y < y2 = Nothing
findHit a (_p : ps) = findHit a ps

findHigestPoint :: [Coord] -> Coord
findHigestPoint [p] = p
findHigestPoint points = findHigestPoint' (head points) (tail points)
  where
    findHigestPoint' (x, y) (p@(x', y') : px) | y' >= y = findHigestPoint' p px
    findHigestPoint' max _ = max

willHit :: Area -> Integer -> Integer -> Bool
willHit a dx dy = simulate' $ findHit a trace
  where
    trace = fire (0, 0) dx dy
    simulate' (Just _) = True
    simulate' Nothing = False

findHigest :: Area -> Coord
findHigest a = findHigest $ filter (uncurry (willHit a)) [(dx, dy) | dx <- [0 .. 250], dy <- [0 .. 250]]
  where
    flipCoord (a, b) = (b, a)
    findHigest = flipCoord . maximum . map (flipCoord . findHigestPoint . uncurry (fire (0, 0)))

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  area <- parseInput inputParser
  print $ snd $ findHigest area
