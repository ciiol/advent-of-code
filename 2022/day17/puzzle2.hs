#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2022/day/17#part2
-- How tall will the tower be after 1000000000000 rocks have stopped?

import Text.ParserCombinators.Parsec (Parser, parse, (<|>), many1, char)
import System.Exit (exitFailure)
import Data.Maybe (maybe, fromMaybe, fromJust)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

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

moves :: [Instruction] -> [(Int, Coord)]
moves = cycle . zip [0..] . map makeMove

tryMove :: Occupied -> Rock -> Coord -> Maybe Rock
tryMove s r c = validate $ moveRock r c
  where
    validate r' = if isValid r' then Just r' else Nothing
    isValid r = all isValidCoord r && S.disjoint r s
    isValidCoord (x, y) = x <= 6 && x >= 0

simulateRock :: Occupied -> [(Int, Coord)] -> Rock -> ([(Int, Coord)], Rock)
simulateRock s [] r = error "no more moves"
simulateRock s (m:ms) r = maybe (ms, r') (simulateRock s ms) $ tryMove s r' (0, 1)
  where
    r' = fromMaybe r $ tryMove s r $ snd m

simulate :: Occupied -> [(Int, Coord)] -> [Rock] -> [(Int, Occupied)]
simulate s ms [] = error "no more rocks"
simulate s ms (r:rs) = (fst (head ms), s) : simulate s' ms' rs
  where
    spawned = moveRock r (2, findHeight s - 4)
    simulated = simulateRock s ms spawned
    r' = snd simulated
    ms' = fst simulated
    s' = S.union s r'

findHeight :: Occupied -> Int
findHeight = minimum . map snd . S.toList

makeOccupied :: Occupied
makeOccupied = S.fromList [(x, 0) | x <- [0..6]]

descript :: Occupied -> [Int]
descript s = map findDiff [0..6]
  where
    height = findHeight s
    findDiff x = (minimum [y | (x', y) <- S.toList s, x == x']) - height

findPattern :: [(Int, Occupied)] -> (Int, Int)
findPattern s = findPattern' M.empty $ zip s [0..]
  where
    key ((l, s), i) = (i `mod` 5, descript s, l)
    findPattern' p [] = error "oops"
    findPattern' p (k@(s, i):ks) | key k `M.member` p = (previous, i - previous)
      where
        previous = fromJust (M.lookup (key k) p)
    findPattern' p (k@(s, i):ks) = findPattern' (M.insert (key k) i p) ks

predict :: Integer -> [(Int, Occupied)] -> Integer
predict x s = fromIntegral startHeight + repeats * repeatBlockHeight + endHeight
  where
    height i = -1 * findHeight (snd (s !! i))
    pat = findPattern s
    start = fst pat
    every = snd pat
    end = fromInteger $ (x - fromIntegral start) `mod` fromIntegral every
    repeats = (x - fromIntegral start) `div` fromIntegral every
    startHeight = height start
    endHeight = fromIntegral $ height (start + end) - startHeight
    repeatBlockHeight = fromIntegral $ height (start + every) - startHeight

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
  print $ predict 1000000000000 $ simulate makeOccupied (moves instructions) rocks
