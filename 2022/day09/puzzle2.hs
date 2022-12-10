#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2022/day/9#part2
-- You decide to distract yourself by modeling rope physics; maybe you can even
-- figure out where not to step.

-- Consider a rope with a knot at each end; these knots mark the head and the tail of the rope.
-- If the head moves far enough away from the tail, the tail is pulled toward the head.

-- Due to the aforementioned Planck lengths, the rope must be quite short; in fact, the head (H) and
-- tail (T) must always be touching (diagonally adjacent and even overlapping both count as touching).

-- If the head is ever two steps directly up, down, left, or right from the tail, the tail must
-- also move one step in that direction so it remains close enough.

-- Otherwise, if the head and tail aren't touching and aren't in the same row or column,
-- the tail always moves one step diagonally to keep up.

-- Rather than two knots, you now must simulate a rope consisting of ten knots. One knot is still the
-- head of the rope and moves according to the series of motions. Each knot further down the rope
-- follows the knot in front of it using the same rules as before.

-- Simulate your complete series of motions on a larger rope with ten knots.
-- How many positions does the tail of the rope visit at least once?

import Text.ParserCombinators.Parsec (Parser, parse, digit, char, many1, try, (<|>), sepEndBy1)
import System.Exit (exitFailure)
import Data.Set as S (fromList, size)

data Command = U Int | D Int | L Int | R Int deriving (Eq, Show)
type Coord = (Int, Int)
type Rope = [Coord]

commandParser :: Parser Command
commandParser = command U 'U' <|> command D 'D' <|> command L 'L' <|> command R 'R'
  where
    command f c = f <$> (try (char c) *> char ' ' *> (read <$> many1 digit))

applyCommand :: Coord -> Command -> [Coord]
applyCommand (x, y) (U n) = [(x, y + d) | d <- [1 .. n]]
applyCommand (x, y) (D n) = [(x, y - d) | d <- [1 .. n]]
applyCommand (x, y) (L n) = [(x - d, y) | d <- [1 .. n]]
applyCommand (x, y) (R n) = [(x + d, y) | d <- [1 .. n]]

trace :: Coord -> [Command] -> [Coord]
trace _ [] = []
trace p (c:cs) = next' ++ trace last' cs
  where
    next' = applyCommand p c
    last' = last next'

distance :: Coord -> Coord -> Int
distance (x1, y1) (x2, y2) = max (abs (x1 - x2)) (abs (y1 - y2))

unit :: Coord -> Coord
unit (x, y) = (unit' x, unit' y)
  where
    unit' x | x > 1 = 1
    unit' x | x < -1 = -1
    unit' x = x

subCoord :: Coord -> Coord -> Coord
subCoord (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

addCoord :: Coord -> Coord -> Coord
addCoord (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

simulateTail :: Coord -> Coord -> Coord
simulateTail h t | distance h t <= 1 = t
simulateTail h t = addCoord t $ unit $ subCoord h t

traceTail :: Coord -> [Coord] -> [Coord]
traceTail t [] = []
traceTail t (h:hs) = next : traceTail next hs
  where
    next = simulateTail h t

traceRope :: Rope -> [Command] -> [[Coord]]
traceRope [] _ = error "oops"
traceRope [x] commands = [trace x commands]
traceRope (x:xs) commands = traceTail x (head tailTrace) : tailTrace
  where
    tailTrace = traceRope xs commands

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  commands <- parseInput $ sepEndBy1 commandParser $ char '\n'
  print $ S.size $ S.fromList $ head $ traceRope (replicate 10 (0, 0)) commands
