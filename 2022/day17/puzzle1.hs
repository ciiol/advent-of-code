#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2022/day/17
-- How many units tall will the tower of rocks be after 2022 rocks have stopped falling?

import Text.ParserCombinators.Parsec (Parser, parse, (<|>), many1, char)
import System.Exit (exitFailure)
import Data.Maybe (maybe, fromMaybe)
import qualified Data.Set as S

type Coord = (Int, Int)
type Rock = S.Set Coord
data Instruction = L | R deriving (Eq, Show)
type Occupied = S.Set Coord

instructionParser :: Parser Instruction
instructionParser = (L <$ char '<') <|> (R <$ char '>')

rocks :: [Rock]
rocks = cycle $ map S.fromList [rock1, rock2, rock3, rock4, rock5]
  where
    rock1 = [(x, 0) | x <- [0..3]]
    rock2 = [(1, -2), (1, -1), (0, -1), (2, -1), (1, 0)]
    rock3 = [(x, 0) | x <- [0..2]] ++ [(2, -2), (2, -1)]
    rock4 = [(0, y) | y <- [-3..0]]
    rock5 = [(x, y) | x <- [0, 1], y <- [-1, 0]]

addCoord :: Coord -> Coord -> Coord
addCoord (dy, dx) (y, x) = (y + dy, x + dx)

moveRock :: Rock -> Coord -> Rock
moveRock r c = S.map (addCoord c) r

makeMove :: Instruction -> Coord
makeMove L = (-1, 0)
makeMove R = (1, 0)

moves :: [Instruction] -> [Coord]
moves = cycle . map makeMove

tryMove :: Occupied -> Rock -> Coord -> Maybe Rock
tryMove s r c = validate $ moveRock r c
  where
    validate r' = if isValid r' then Just r' else Nothing
    isValid r = all isValidCoord r && S.disjoint r s
    isValidCoord (x, y) = x <= 6 && x >= 0

simulateRock :: Occupied -> [Coord] -> Rock -> ([Coord], Rock)
simulateRock s [] r = error "no more moves"
simulateRock s (m:ms) r = maybe (ms, r') (simulateRock s ms) $ tryMove s r' (0, 1)
  where
    r' = fromMaybe r $ tryMove s r m

simulate :: Occupied -> [Coord] -> [Rock] -> [Occupied]
simulate s ms [] = error "no more rocks"
simulate s ms (r:rs) = s : simulate s' ms' rs
  where
    spawned = moveRock r (2, minimum (map snd $ S.toList s) - 4)
    simulated = simulateRock s ms spawned
    r' = snd simulated
    ms' = fst simulated
    s' = S.union s r'

findHeight :: Occupied -> Int
findHeight = (* (-1)) . minimum . map snd . S.toList

makeOccupied :: Occupied
makeOccupied = S.fromList [(x, 0) | x <- [0..6]]

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  instructions <- parseInput $ many1 instructionParser
  print $ findHeight $ (!! 2022) $ simulate makeOccupied (moves instructions) rocks
