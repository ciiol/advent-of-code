#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/24
-- A member of the renovation crew gives you a list of the tiles
-- that need to be flipped over (your puzzle input). Each line in
-- the list identifies a single tile that needs to be flipped by
-- giving a series of steps starting from a reference tile in the
-- very center of the room. (Every line starts from the same
-- reference tile.)

-- Go through the renovation crew's list and determine which tiles
-- they need to flip. After all of the instructions have been followed,
-- how many tiles are left with the black side up?

import qualified Data.List as L
import qualified Data.Set as S
import Control.Arrow ((&&&))
import Text.ParserCombinators.Parsec (Parser, many1, endBy1, parse, try, string, newline, (<|>))
import System.Exit (exitFailure)

data Step = E | SE | SW | W | NW | NE deriving (Show, Eq, Ord)
type Coord = (Int, Int, Int)

tryString :: String -> Parser String
tryString = try . string

stopParser :: Parser Step
stopParser =
    tryString "e" *> pure E
    <|> tryString "se" *> pure SE
    <|> tryString "sw" *> pure SW
    <|> tryString "w" *> pure W
    <|> tryString "nw" *> pure NW
    <|> tryString "ne" *> pure NE

toCoord :: Step -> Coord
toCoord E = (1, -1, 0)
toCoord W = (-1, 1, 0)
toCoord SE = (0, -1, 1)
toCoord SW = (-1, 0, 1)
toCoord NE = (1, 0, -1)
toCoord NW = (0, 1, -1)

addCoord :: Coord -> Coord -> Coord
addCoord (x, y, z) (dx, dy, dz) = (x + dx, y + dy, z + dz)

walk :: [Step] -> Coord
walk = L.foldl' walk' (0, 0, 0)
  where
    walk' acc = (addCoord acc) . toCoord

flippedTiles :: [Coord] -> S.Set Coord
flippedTiles = L.foldl' flippedTiles' S.empty
  where
    flippedTiles' acc pos | S.member pos acc = S.delete pos acc
    flippedTiles' acc pos | S.notMember pos acc = S.insert pos acc
    flippedTiles' _ _ = error "oops"

around :: Coord -> [Coord]
around p = map (addCoord p) diffs
  where
    diffs = map toCoord [E, W, SE, SW, NE, NW]

count :: [Coord] -> [(Int, Coord)]
count = map (length &&& head) . L.group . L.sort

play :: S.Set Coord -> S.Set Coord
play lives = S.fromList $ map snd $ filter isAlive $ count $ concat $ map around $ S.toList lives
  where
    isAlive (n, x) | (n == 0 || n > 2) && x `S.member` lives = False
    isAlive (n, x) | (n == 2) && (not $ x `S.member` lives) = True
    isAlive (_, x) = x `S.member` lives

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
  paths <- parseInput $ endBy1 (many1 stopParser) newline
  putStrLn $ show $ map length $ take 101 $ iterate play $ flippedTiles $ map walk paths
