#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/5#part2
-- Your seat wasn't at the very front or back, though;
-- the seats with IDs +1 and -1 from yours will be in your list.

-- What is the ID of your seat?

import Text.ParserCombinators.Parsec
import System.Exit (exitFailure)
import Data.Set (Set, (\\), fromList, intersection)

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

findNeighbours :: (Int -> Int) -> [Int] -> Set Int
findNeighbours f ids = fromList $ map f ids

findSurroundedByNeighbours :: [Int] -> Set Int
findSurroundedByNeighbours ids = findNeighbours (subtract 1) ids `intersection` findNeighbours (+ 1) ids

findFree :: [Int] -> Set Int
findFree ids = findSurroundedByNeighbours ids \\ fromList ids

main :: IO ()
main = do
    rules <- parseInput rulesParser
    putStrLn $ show $ findFree $ map (getId . applyRule defaultRange2) rules
