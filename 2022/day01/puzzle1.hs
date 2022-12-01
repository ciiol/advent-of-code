#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2022/day/1
-- The Elves take turns writing down the number of Calories contained by the various
-- meals, snacks, rations, etc. that they've brought with them, one item per line.
-- Each Elf separates their own inventory from the previous Elf's inventory (if any)
-- by a blank line.

-- Find the Elf carrying the most Calories. How many total Calories is that Elf carrying?

import Text.ParserCombinators.Parsec (Parser, digit, char, many1, parse, sepEndBy1)
import System.Exit (exitFailure)
import Data.List (maximumBy)

inventoryParser :: Parser [Integer]
inventoryParser = sepEndBy1 (read <$> many1 digit) (char '\n')

inventoriesParser :: Parser [[Integer]]
inventoriesParser = sepEndBy1 inventoryParser (char '\n')

findLargest :: [[Integer]] -> [Integer]
findLargest = maximumBy compareSum
  where
    compareSum a b = compare (sum a) (sum b)

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  inventories <- parseInput inventoriesParser
  print $ sum $ findLargest inventories
