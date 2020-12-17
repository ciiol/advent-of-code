#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/17#part2
-- Starting with your given initial configuration, simulate six cycles in a
-- 4-dimensional space. How many cubes are left in the active state after
-- the sixth cycle?

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.ParserCombinators.Parsec (Parser, many, parse, sepBy, newline, char, (<|>))
import System.Exit (exitFailure)

data State = Active | Inactive deriving (Show, Eq, Ord, Enum, Bounded)
data Grid = Grid {gActive :: S.Set Coord, gAround :: AroundIndex} deriving (Show, Eq, Ord)
type AroundIndex = M.Map Coord Int
type Coord = (Int, Int, Int, Int)

stateParser :: Parser State
stateParser =
  char '.' *> pure Inactive
  <|> char '#' *> pure Active

gridParser :: Parser [[State]]
gridParser = sepBy (many stateParser) newline

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

emptyGrid :: Grid
emptyGrid = Grid S.empty M.empty

makeGrid :: [[State]] -> Grid
makeGrid = foldr addActive emptyGrid . map fst . filter ((== Active) . snd) . concat . map extractCoord . enumerate
  where
    extractCoord (j, row) = map (extractRowCoord j) $ enumerate row
    extractRowCoord j (i, x) = ((j, i, 0, 0), x)

addActive :: Coord -> Grid -> Grid
addActive coord grid | S.member coord (gActive grid) = error "oops"
addActive coord grid = grid {gActive = newActive, gAround = newAround}
  where
    newActive = S.insert coord $ gActive grid
    newAround = foldr (updateAroundIndex (+ 1)) (gAround grid) $ neighbours coord

removeActive :: Coord -> Grid -> Grid
removeActive coord grid | not $ S.member coord (gActive grid) = error "oops"
removeActive coord grid = grid {gActive = newActive, gAround = newAround}
  where
    newActive = S.delete coord $ gActive grid
    newAround = foldr (updateAroundIndex (flip (-) 1)) (gAround grid) $ neighbours coord

updateAroundIndex :: (Int -> Int) -> Coord -> AroundIndex -> AroundIndex
updateAroundIndex f = M.alter (update . f . (maybe 0 id))
  where
    update 0 = Nothing
    update x | x < 0 = error "oops"
    update x = Just x

relativePoint :: Coord -> Coord -> Coord
relativePoint (dx, dy, dz, dw) (x, y, z, w) = (x + dx, y + dy, z + dz, w + dw)

makeCordGrid :: [Coord]
makeCordGrid = concat $ map addX [-1..1]
  where
    addX x = concat $ map (addY x) [-1..1]
    addY x y = concat $ map (addZ x y) [-1..1]
    addZ x y z = map (addW x y z) [-1..1]
    addW x y z w = (x, y, z, w)

neighbours :: Coord -> [Coord]
neighbours pos = map (relativePoint pos) diffs
  where
    diffs = filter (/= (0, 0, 0, 0)) makeCordGrid

findInIndex :: (Int -> Bool) -> (Coord -> Bool) -> Grid -> [Coord]
findInIndex f filterF grid = filter filterF $ arounds ++ active
  where
    arounds = map fst $ filter (f . snd) $ M.toList $ gAround grid
    active = filter (not . flip M.member (gAround grid)) $ S.toList $ gActive grid

newActives :: Grid -> [Coord]
newActives grid = findInIndex isActive isNotInActiveSet grid
  where
    isActive 3 = True
    isActive _ = False
    isNotInActiveSet = not . flip S.member (gActive grid)

newInactives :: Grid -> [Coord]
newInactives grid = findInIndex isInactive isInActiveSet grid
  where
    isInactive 3 = False
    isInactive 2 = False
    isInactive _ = True
    isInActiveSet = flip S.member (gActive grid)

nextGeneration :: Grid -> Grid
nextGeneration grid = foldr addActive (foldr removeActive grid $ newInactives grid) (newActives grid)

evolve :: Grid -> [Grid]
evolve grid = newGrid : evolve newGrid
  where
    newGrid = nextGeneration grid

totalActive :: Grid -> Int
totalActive = S.size . gActive

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
  seats <- parseInput gridParser
  putStrLn $ show $ totalActive $ last $ take 6 $ evolve $ makeGrid seats
