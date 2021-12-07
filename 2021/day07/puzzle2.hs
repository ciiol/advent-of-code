#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2021/day/7#part2
-- As it turns out, crab submarine engines don't burn fuel at a constant rate.
-- Instead, each change of 1 step in horizontal position costs 1 more unit of fuel
-- than the last: the first step costs 1, the second step costs 2, the third step
-- costs 3, and so on.

-- Determine the horizontal position that the crabs can align to using the least fuel possible.
-- How much fuel must they spend to align to that position?

import Data.List (minimumBy)
import System.Exit (exitFailure)
import Text.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec (Parser, char, parse, sepBy1)
import qualified Text.ParserCombinators.Parsec.Token as P

type Position = Integer

lexer = P.makeTokenParser emptyDef

positionsParser :: Parser [Position]
positionsParser = sepBy1 (P.natural lexer) (char ',')

cost :: Integer -> [Position] -> Integer
cost target = sum . map (abs . cost' target)
  where
    cost' target position = (diff target position) * (diff target position + 1) `div` 2
    diff target position = abs $ target - position

cheapest :: [Position] -> Position
cheapest positions = snd $ minimum $ zip (costs targets) targets
  where
    targets = [0 .. (maximum positions)]
    costs = map (`cost` positions)

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  positions <- parseInput positionsParser
  print $ cost (cheapest positions) positions
