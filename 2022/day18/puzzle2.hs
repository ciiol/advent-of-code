#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2022/day/18#part2
-- it approximates the shape of the lava droplet with 1x1x1 cubes on a 3D grid,
-- each given as its x,y,z position.

-- To approximate the surface area, count the number of sides of each cube that are
-- not immediately connected to another cube.

-- What is the exterior surface area of your scanned lava droplet?

import Text.ParserCombinators.Parsec (Parser, parse, many1, sepEndBy1, char, digit)
import System.Exit (exitFailure)
import Data.List (foldl1', maximumBy)
import Data.Function (on)
import qualified Data.Set as S

type Coord = (Int, Int, Int)
type Side = Coord
type Cube = Coord
type Figure = S.Set Side

cubeParser :: Parser Cube
cubeParser = makeCube <$> coord <*> (comma *> coord) <*> (comma *> coord)
  where
    coord = read <$> many1 digit
    comma = char ','

makeCube :: Int -> Int -> Int -> Cube
makeCube x y z = (x * 2, y * 2, z * 2)

merge :: Figure -> Figure -> Figure
merge f1 f2 = S.union f1' f2'
  where
    f1' = S.difference f1 f2
    f2' = S.difference f2 f1

cubes :: Side -> [Cube]
cubes (x, y, z) | odd x = [(x - 1, y, z), (x + 1, y, z)]
cubes (x, y, z) | odd y = [(x, y - 1, z), (x, y + 1, z)]
cubes (x, y, z) | odd z = [(x, y, z - 1), (x, y, z + 1)]
cubes _ = error "oops"

sides :: Cube -> Figure
sides (x, y, z) = S.fromList $ xSides ++ ySides ++ zSides
  where
    xSides = [(x - 1, y, z), (x + 1, y, z)]
    ySides = [(x, y - 1, z), (x, y + 1, z)]
    zSides = [(x, y, z - 1), (x, y, z + 1)]

nearestCubes :: Cube -> [Cube]
nearestCubes (x, y, z) =
  [ (x - 2, y, z), (x + 2, y, z)
  , (x, y - 2, z), (x, y + 2, z)
  , (x, y, z - 2), (x, y, z + 2)
  ]

nearestConnectedCubes :: Figure -> Cube -> [Cube]
nearestConnectedCubes f c = filter isAlmostConnected $ nearestCubes c
  where
    isAlmostConnected c = any (not . S.disjoint f . sides) $ nearestCubes c

getExternalSurface :: S.Set Cube -> Figure -> Figure
getExternalSurface internal f = S.intersection f $ S.fromList $ concatMap (S.toList . sides) $ S.toList externalCubes
  where
    external = minimum $ cubes $ minimum $ S.toList f
    externalCubes = getAllExternalCubes (S.fromList [external]) (S.fromList [external])
    connectedCubes cubes = filter (not . (`S.member` internal)) $ concatMap (nearestConnectedCubes f) $ S.toList cubes
    getAllExternalCubes q v | q == S.empty = v
    getAllExternalCubes q v = getAllExternalCubes toVisit $ S.union toVisit v
      where
        toVisit = S.fromList $ filter (not . (`S.member` v)) $ connectedCubes q

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
  print $ S.size $ getExternalSurface (S.fromList cubes) (foldl1' merge $ map sides cubes)
