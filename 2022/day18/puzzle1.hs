#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2022/day/18
-- it approximates the shape of the lava droplet with 1x1x1 cubes on a 3D grid,
-- each given as its x,y,z position.

-- To approximate the surface area, count the number of sides of each cube that are
-- not immediately connected to another cube.

-- What is the surface area of your scanned lava droplet?

import Text.ParserCombinators.Parsec (Parser, parse, many1, sepEndBy1, char, digit)
import System.Exit (exitFailure)
import Data.List (foldl1')
import qualified Data.Set as S

type Coord = (Int, Int, Int)
type Figure = S.Set Coord

cubeParser :: Parser Figure
cubeParser = makeFugure <$> coord <*> (comma *> coord) <*> (comma *> coord)
  where
    coord = read <$> many1 digit
    comma = char ','

makeFugure :: Int -> Int -> Int -> Figure
makeFugure x y z = S.fromList $ xSides ++ ySides ++ zSides
  where
    x' = x * 2
    y' = y * 2
    z' = z * 2
    xSides = [(x' - 1, y', z'), (x' + 1, y', z')]
    ySides = [(x', y' - 1, z'), (x', y' + 1, z')]
    zSides = [(x', y', z' - 1), (x', y', z' + 1)]

merge :: Figure -> Figure -> Figure
merge f1 f2 = S.union f1' f2'
  where
    f1' = S.difference f1 f2
    f2' = S.difference f2 f1

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  cubes <- parseInput $ sepEndBy1 cubeParser $ char '\n'
  print $ S.size $ foldl1' merge cubes
