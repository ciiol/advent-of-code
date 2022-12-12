#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2022/day/12#part2
-- You ask the device for a heightmap of the surrounding area (your puzzle input).

-- You'd like to reach E, but to save energy, you should do it in as few steps as possible.
-- During each step, you can move exactly one square up, down, left, or right. To avoid needing
-- to get out your climbing gear, the elevation of the destination square can be at most one
-- higher than the elevation of your current square

-- What is the fewest steps required to move starting from any square with elevation a to the
-- location that should get the best signal?

import Text.ParserCombinators.Parsec (Parser, parse, sepEndBy1, many1, letter, char)
import System.Exit (exitFailure)
import Data.Maybe (fromJust, mapMaybe)
import Data.Char (ord)
import Data.List (foldl')
import qualified Data.Set as S
import qualified Data.Map.Strict as M

type Coord = (Int, Int)
data Elem = Start | End | Point Char deriving (Eq, Show)
type Heightmap = M.Map Coord Elem

matrixParser :: Parser [[Elem]]
matrixParser = sepEndBy1 (many1 (makeElem <$> letter)) $ char '\n'
  where
    makeElem 'S' = Point 'a'
    makeElem 'E' = End
    makeElem c = Point c

heightmapParser :: Parser Heightmap
heightmapParser = makeHeightmap <$> matrixParser

makeHeightmap :: [[Elem]] -> Heightmap
makeHeightmap = M.fromList . concat . indexed
  where
    indexed matrix = [[((x, y), v) | (x, v) <- zip [0 ..] line] | (y, line) <- zip [0 ..] matrix]

adjusted :: Coord -> [Coord]
adjusted (x, y) = [(x + dx, y) | dx <- [-1 .. 1], dx /= 0] ++ [(x, y + dy) | dy <- [-1 .. 1], dy /= 0]

filterGood :: Heightmap -> Coord -> [Coord] -> [Coord]
filterGood m from = filter (isGood m from)

isGood :: Heightmap -> Coord -> Coord -> Bool
isGood m from to = isGood' (at' from) (at' to)
  where
    at' = cost . at m
    cost Start = ord 'a'
    cost End = ord 'z'
    cost (Point c) = ord c
    isGood' from to = from + 1 >= to

neighbors :: Heightmap -> Coord -> [Coord]
neighbors m c = filter (`M.member` m) $ adjusted c

at :: Heightmap -> Coord -> Elem
at m c = fromJust $ M.lookup c m

findStarts :: Heightmap -> [Coord]
findStarts = map fst . filter ((== Point 'a') . snd) . M.toList

findPath :: (Elem -> Bool) -> Heightmap -> Coord -> Maybe [Coord]
findPath isEnd h start = bfs (M.fromList [(start, start)]) (M.fromList [((0, start), (0, start))])
  where
    unwind _ p | p == start = [start]
    unwind v p = p : unwind v (fromJust $ M.lookup p v)
    point = snd
    cost = fst
    addPotentialPath from m p = M.insert (cost from + 1, p) from m
    addVisited p v from = M.insert from p v
    bfs v m | M.size m == 0 = Nothing
    bfs v m | isEnd (at h candidate) = Just $ reverse $ unwind v candidate
      where
        candidate = point $ fst $ M.findMin m
    bfs v m = bfs v' $ foldl' (addPotentialPath candidate) (M.delete candidate m) new
      where
        v' = foldl' (addVisited candidatePoint) v new
        candidate = fst $ M.findMin m
        candidatePoint = point candidate
        new = notVisited $ filterGood h candidatePoint $ neighbors h candidatePoint
        notVisited = filter (not . (`M.member` v))

findPathLen :: Maybe [Coord] -> Maybe Int
findPathLen Nothing = Nothing
findPathLen (Just p) = Just $ length p - 1

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  heightmap <- parseInput heightmapParser
  print $ minimum $ mapMaybe (findPathLen . findPath (== End) heightmap) $ findStarts heightmap
