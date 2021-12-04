#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2021/day/4#part2
-- Bingo is played on a set of boards each consisting of a 5x5 grid of numbers.
-- Numbers are chosen at random, and the chosen number is marked on all boards on
-- which it appears. (Numbers may not appear on all boards.) If all numbers in
-- any row or any column of a board are marked, that board wins.

-- The score of the winning board can now be calculated. Start by finding the sum of all
-- unmarked numbers on that board; then, multiply that sum
-- by the number that was just called when the board won.

-- To guarantee victory against the giant squid, figure out which board will win first.

-- On the other hand, it might be wise to try a different strategy: let the giant squid win.

-- You aren't sure how many bingo boards a giant squid could play at once, so rather than waste
-- time counting its arms, the safe thing to do is to figure out which board will win last and
-- choose that one. That way, no matter which boards it picks, it will win for sure.

-- What will your final score be if you choose that board?

import Text.Parsec.Language (emptyDef)
import System.Exit (exitFailure)
import Data.Functor (($>))
import Data.Maybe (fromJust)
import Data.List (foldl', transpose, find)
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec (Parser, sepBy1, many1, endBy1, parse, try, char, newline)

type Change = Integer
data Cell = Unmarked Integer | Marked Integer deriving (Show, Eq, Ord)
type Board = [[Cell]]

lexer = P.makeTokenParser emptyDef

trySymbol :: String -> Parser String
trySymbol = P.symbol lexer

inputParser :: Parser ([Integer], [Board])
inputParser = do
  numbers <- sepBy1 (P.natural lexer) (char ',')
  boardsContent <- many1 $ P.natural lexer
  return (numbers, map makeBoard $ chunk 25 $ map Unmarked boardsContent)

makeBoard :: [Cell] -> Board
makeBoard = chunk 5

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

cols :: Board -> [[Cell]]
cols = transpose

rows :: Board -> [[Cell]]
rows = id

mapBoard :: (Cell -> Cell) -> Board -> Board
mapBoard f = map (map f)

foldBoard :: (a -> Cell -> a) -> a -> Board -> a
foldBoard f = foldl' (foldl' f)

isMarked :: Cell -> Bool 
isMarked (Marked _) = True 
isMarked (Unmarked _) = False

isWinner :: Board -> Bool 
isWinner board = hasWinner (rows board) || hasWinner (cols board)
  where
    hasWinner = any (all isMarked)

mark :: Integer -> Cell -> Cell
mark _ (Marked num) = Marked num
mark num (Unmarked cell) | cell == num = Marked num
mark _ (Unmarked cell) = Unmarked cell

markAll :: Integer -> [Board] -> [Board]
markAll num = map (mapBoard (mark num))

play :: [Integer] -> [Board] -> (Integer, Board)
play [] _ = error "no winners"
play (x:xs) boards = play' $ filter (not . isWinner) newBoards
  where
    newBoards = markAll x boards
    play' [] = (x, fromJust $ find isWinner (reverse newBoards))
    play' rest = play xs rest

result :: (Integer, Board) -> Integer
result (winner, board) = winner * foldBoard folder 0 board
  where
    folder acc (Unmarked num) = acc + num
    folder acc (Marked _) = acc

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
        putStrLn $ "Error: " ++ show err
        exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
    (numbers, boards) <- parseInput inputParser
    print $ result $ play numbers boards
