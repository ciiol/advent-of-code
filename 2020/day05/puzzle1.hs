#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/5
-- Instead of zones or groups, this airline uses binary space
-- partitioning to seat people. A seat might be specified like
-- FBFBBFFRLR, where F means "front", B means "back", L means
-- "left", and R means "right".

-- The first 7 characters will either be F or B; these specify
-- exactly one of the 128 rows on the plane (numbered 0 through 127).
-- Each letter tells you which half of a region the given seat is in.
-- Start with the whole list of rows; the first letter indicates
-- whether the seat is in the front (0 through 63) or the back
-- (64 through 127). The next letter indicates which half of that
-- region the seat is in, and so on until you're left with exactly
-- one row.

-- Every seat also has a unique seat ID: multiply the row by 8,
-- then add the column. In this example, the seat has
-- ID 44 * 8 + 5 = 357.

-- What is the highest seat ID on a boarding pass?

import Text.ParserCombinators.Parsec
import System.Exit (exitFailure)

data Range = Range Int Int deriving (Show, Eq, Ord, Bounded)
data Range2 = Range2 Range Range deriving (Show, Eq, Ord, Bounded)

data RuleElem = L | R | F | B deriving (Show, Eq, Ord, Enum, Bounded)
type Rule = [RuleElem]

rulesParser :: Parser [Rule]
rulesParser = endBy (many1 ruleElemParser) spaces

ruleElemParser :: Parser RuleElem
ruleElemParser =
    char 'L' *> pure L
    <|> char 'R' *> pure R
    <|> char 'F' *> pure F
    <|> char 'B' *> pure B

takeLeftHalf :: Range -> Range
takeLeftHalf (Range from to) = Range from (to - (to - from) `div` 2)

takeRightHalf :: Range -> Range
takeRightHalf (Range from to) = Range (from + (to - from) `div` 2) to

applyToHRange :: (Range -> Range) -> Range2 -> Range2
applyToHRange f (Range2 h v) = Range2 (f h) v

applyToVRange :: (Range -> Range) -> Range2 -> Range2
applyToVRange f (Range2 h v) = Range2 h (f v)

applyRuleElem :: Range2 -> RuleElem -> Range2
applyRuleElem range F = applyToHRange takeLeftHalf range
applyRuleElem range B = applyToHRange takeRightHalf range
applyRuleElem range L = applyToVRange takeLeftHalf range
applyRuleElem range R = applyToVRange takeRightHalf range

applyRule :: Range2 -> Rule -> Range2
applyRule = foldl (applyRuleElem)

getId :: Range2 -> Int
getId (Range2 (Range x1 x2) (Range y1 y2))
    | x1 == x2 - 1 && y1 == y2 - 1 = x1 * 8 + y1
getId _ = 0

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
        putStrLn $ "Error: " ++ show err
        exitFailure
    parseInput' p = do
        input <- getContents
        return $ parse p "(stdin)" input

defaultRange2 :: Range2
defaultRange2 = Range2 (Range 0 128) (Range 0 8)

main :: IO ()
main = do
    rules <- parseInput rulesParser
    putStrLn $ show $ foldl max 0 $ map (getId . applyRule defaultRange2) rules
