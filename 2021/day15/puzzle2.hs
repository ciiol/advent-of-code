#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2021/day/15#part2
-- The cavern is large, but has a very low ceiling, restricting your motion to two
-- dimensions. The shape of the cavern resembles a square; a quick scan of chiton
-- density produces a map of risk level throughout the cave (your puzzle input).

-- The entire cave is actually five times larger in both dimensions than you thought;
-- the area you originally scanned is just one tile in a 5x5 tile area that forms the
-- full map. Your original map tile repeats to the right and downward; each time the
-- tile repeats to the right or downward, all of its risk levels are 1 higher than the
-- tile immediately up or left of it.

-- what is the lowest total risk of any path from the top left to the bottom right?

import Data.Char (digitToInt)
import Data.List (foldl', sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Debug.Trace (trace)
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
adjusted (x, y) = [(x + dx, y) | dx <- [-1 .. 1], dx /= 0] ++ [(x, y + dy) | dy <- [-1 .. 1], dy /= 0]

neighbors :: Field -> Coord -> [Coord]
neighbors field c = filter (`M.member` field) $ adjusted c

at :: Field -> Coord -> Int
at field c = fromJust $ M.lookup c field

enlarge :: Field -> Field
enlarge f = M.fromList new
  where
    size = maximum $ M.keys f
    sizeX = fst size + 1
    sizeY = snd size + 1
    new = [ ((x + sizeX * dx, y + sizeY * dy), addRisk cost (dx + dy))
          | ((x, y), cost) <- M.toList f
          , dx <- [0..4]
          , dy <- [0..4]
          ]
    addRisk risk delta | risk + delta > 9 = (mod (risk + delta - 1) 9) + 1
    addRisk risk delta = risk + delta

costs :: Field -> Coord -> Field
costs f start = M.fromList $ concat $ costs' S.empty [(0, start)]
  where
    costs' :: S.Set Coord -> [(Int, Coord)] -> [[(Coord, Int)]]
    costs' visited [] = []
    costs' known (x : others) = addNew known others $ expand known x
    addNew :: S.Set Coord -> [(Int, Coord)] -> [(Int, Coord)] -> [[(Coord, Int)]]
    addNew known others new = result : next
      where
        result = [(pos, cost) | (cost, pos) <- new]
        next = costs' (foldl' (flip S.insert) known $ map snd new) (sort $ new ++ others)
    expand :: S.Set Coord -> (Int, Coord) -> [(Int, Coord)]
    expand known (cost, pos) = map (\c -> (at f c + cost, c)) $ filter (unknown known) $ neighbors f pos
    unknown known x = not $ S.member x known

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
  print $ snd $ maximum $ M.toList $ costs (enlarge field) (0, 0)
