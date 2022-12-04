#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2022/day/4
-- Elves pair up and make a big list of the section assignments fo
-- each pair (your puzzle input).

-- In how many assignment pairs do the ranges overlap?

import Text.ParserCombinators.Parsec (Parser, sepEndBy1, many1, digit, char, parse)
import System.Exit (exitFailure)

type Range = (Integer, Integer)
type Pair = (Range, Range)

pairParser :: Parser Pair
pairParser = (,) <$> rangeParser <*> (char ',' *> rangeParser)

rangeParser :: Parser Range
rangeParser = (,) <$> num <*> (char '-' *> num)
  where
    num = read <$> many1 digit

isOverlap :: Pair -> Bool
isOverlap (range1, range2) = (range1 `overlapLeft` range2) || (range2 `overlapLeft` range1)
  where
    overlapLeft (l1, r1) (l2, r2) = l1 <= l2 && r1 >= l2

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  pairs <- parseInput $ sepEndBy1 pairParser $ char '\n'
  print $ length $ filter isOverlap pairs
