#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2021/day/3
-- The diagnostic report (your puzzle input) consists of a list of binary numbers which,
-- when decoded properly, can tell you many useful things about the conditions of the submarine.
-- The first parameter to check is the power consumption.

-- You need to use the binary numbers in the diagnostic report to generate two new binary numbers
-- (called the gamma rate and the epsilon rate). The power consumption can then be found by
-- multiplying the gamma rate by the epsilon rate.

-- Each bit in the gamma rate can be determined by finding the most common bit in the
-- corresponding position of all numbers in the diagnostic report. The epsilon rate is calculated
-- in a similar way; rather than use the most common bit, the least common bit from each position is used.

-- What is the power consumption of the submarine?

import System.Exit (exitFailure)
import Data.Functor (($>))
import Data.List (foldl')
import Text.ParserCombinators.Parsec (Parser, many1, sepEndBy1, parse, try, char, newline, (<|>))

type ReportItem = Int
type Report = [ReportItem]
data Summary = Summary Int [Int] deriving (Show)

reportParser :: Parser Report
reportParser = many1 reportItemParser

reportItemParser :: Parser ReportItem
reportItemParser = 
  try (char '0') $> 0
  <|> try (char '1') $> 1

summarize :: [Report] -> Summary
summarize [] = error "oops, no reports"
summarize (x:xs) = foldl' summarize' (init x) xs
  where
    init = Summary 1
    summarize' (Summary count sums) report = Summary (count + 1) (addReport sums report)
    addReport = zipWith (+)

toDecimal :: [Int] -> Int
toDecimal = foldl' ((+) . (* 2)) 0

makeBinary :: (Int -> Bool) -> [Int] -> [Int]
makeBinary f = map (\x -> if f x then 1 else 0)

gamma :: Summary -> Int
gamma (Summary count sums) = toDecimal $ makeBinary ((count `div` 2) <) sums

epsilon :: Summary -> Int
epsilon (Summary count sums) = toDecimal $ makeBinary ((count `div` 2) >) sums

result :: [Report] -> Int
result reports = (gamma summary) * (epsilon summary)
  where
    summary = summarize reports

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  reports <- parseInput $ sepEndBy1 reportParser newline
  print $ result reports
