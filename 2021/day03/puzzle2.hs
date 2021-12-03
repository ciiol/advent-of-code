#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2021/day/3#part2
-- Next, you should verify the life support rating, which can be determined by
-- multiplying the oxygen generator rating by the CO2 scrubber rating.

-- Both the oxygen generator rating and the CO2 scrubber rating are values that can be
-- found in your diagnostic report - finding them is the tricky part. Both values are
-- located using a similar process that involves filtering out values until only one remains.
-- Before searching for either rating value, start with the full list of binary numbers from
-- your diagnostic report and consider just the first bit of those numbers. Then:

--     Keep only numbers selected by the bit criteria for the type of rating value for which you
--       are searching. Discard numbers which do not match the bit criteria.
--     If you only have one number left, stop; this is the rating value for which you are searching.
--     Otherwise, repeat the process, considering the next bit to the right.

-- The bit criteria depends on which type of rating value you want to find:

--     To find oxygen generator rating, determine the most common value (0 or 1) in the
--       current bit position, and keep only numbers with that bit in that position. If 0 and 1 
--       are equally common, keep values with a 1 in the position being considered.
--     To find CO2 scrubber rating, determine the least common value (0 or 1) in the current bit
--       position, and keep only numbers with that bit in that position. If 0 and 1 are equally common,
--       keep values with a 0 in the position being considered.

-- What is the life support rating of the submarine?

import System.Exit (exitFailure)
import Data.Functor (($>))
import Data.Bifunctor (second)
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

gamma :: Summary -> [Int]
gamma (Summary count sums) = makeBinary ((count <=) . (* 2)) sums

epsilon :: Summary -> [Int]
epsilon (Summary count sums) = makeBinary ((count >) . (* 2)) sums

matchedHead :: (Eq a) => [a] -> [a] -> Bool
matchedHead (x:_) (y:_) = x == y 
matchedHead _ _ = error "oops, can not compare"

findReport :: (Summary -> [Int]) -> [Report] -> Report
findReport f reports = findReport' $ zip reports reports
  where
    findReport' :: [(Report, Report)] -> Report
    findReport' [] = error "no reports"
    findReport' [(original, _)] = original
    findReport' pairs = findReport' $ dropHead $ filterMatched pairs
    filterMatched pairs = filter (matchedHead (mask pairs) . snd) pairs
    dropHead = map (second tail)
    mask = f . summarize . map snd

oxygen :: [Report] -> Int
oxygen = toDecimal . findReport gamma

co2 :: [Report] -> Int
co2 = toDecimal . findReport epsilon

result :: [Report] -> Int
result reports = (oxygen reports) * (co2 reports)

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
