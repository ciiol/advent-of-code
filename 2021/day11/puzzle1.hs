#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2021/day/11
-- You enter a large cavern full of rare bioluminescent dumbo octopuses!
-- Each octopus slowly gains energy over time and flashes brightly for a
-- moment when its energy is full.

-- You can model the energy levels and flashes of light in steps. During a
-- single step, the following occurs:

--     First, the energy level of each octopus increases by 1.
--     Then, any octopus with an energy level greater than 9 flashes. This increases
--       the energy level of all adjacent octopuses by 1, including octopuses that
--       are diagonally adjacent. If this causes an octopus to have an energy level greater
--       than 9, it also flashes. This process continues as long as new octopuses keep
--       having their energy level increased beyond 9. (An octopus can only flash at most once per step.)
--     Finally, any octopus that flashed during this step has its energy level set to
--       0, as it used all of its energy to flash.

-- Given the starting energy levels of the dumbo octopuses in your cavern, simulate 100 steps.
-- How many total flashes are there after 100 steps?

import Data.Char (digitToInt)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Set as S
import System.Exit (exitFailure)
import Text.ParserCombinators.Parsec (Parser, char, digit, many1, parse, sepEndBy1)

type Coord = (Int, Int)

type Field = M.Map Coord Int

matrixParser :: Parser [[Int]]
matrixParser = sepEndBy1 (many1 (digitToInt <$> digit)) $ char '\n'

fieldParser :: Parser Field
fieldParser = makeField <$> matrixParser

makeField :: [[Int]] -> Field
makeField = M.fromList . concat . indexed
  where
    indexed matrix = [[((x, y), v) | (x, v) <- zip [0 ..] line] | (y, line) <- zip [0 ..] matrix]

adjusted :: Coord -> [Coord]
adjusted (x, y) = [(x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], not (dy == 0 && dx == 0)]

neighbors :: Field -> Coord -> [Coord]
neighbors field c = filter (`M.member` field) $ adjusted c

at :: Field -> Coord -> Int
at field c = fromJust $ M.lookup c field

simulate :: Field -> [[Coord]]
simulate = simulateStep S.empty . increased
  where
    increased = M.map (+ 1)
    simulateStep flashed field | null (snd (simulateTick flashed field)) = flashedL : simulate (zeroed field flashedL)
      where
        flashedL = S.toList flashed
    simulateStep flashed field = simulateStep nextFlashed nextField
      where
        simulated = simulateTick flashed field
        nextFlashed = S.union flashed $ S.fromList $ snd simulated
        nextField = fst simulated
    simulateTick flashed field = (foldl' flash field newFlashed, newFlashed)
      where
        newFlashed = toFlash flashed field
    toFlash flashed field = [k | (k, v) <- M.toList field, v > 9, not $ S.member k flashed]
    flash field c = foldl' (flip $ M.update $ Just . (+ 1)) field $ neighbors field c
    zeroed field elems = M.union (M.fromList $ zip elems [0, 0 ..]) field

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  field <- parseInput fieldParser
  print $ length $ concat $ take 100 $ simulate field
