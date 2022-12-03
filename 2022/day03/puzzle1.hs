#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2022/day/3
-- The list of items for each rucksack is given as characters all on a single line.
-- A given rucksack always has the same number of items in each of its two compartments,
-- so the first half of the characters represent items in the first compartment, while the
-- second half of the characters represent items in the second compartment.

-- To help prioritize item rearrangement, every item type can be converted to a priority:

--     Lowercase item types a through z have priorities 1 through 26.
--     Uppercase item types A through Z have priorities 27 through 52.

-- Find the item type that appears in both compartments of each rucksack.
-- What is the sum of the priorities of those item types?

import Text.ParserCombinators.Parsec (Parser, sepEndBy1, many1, alphaNum, char, parse)
import System.Exit (exitFailure)
import Data.Char (ord, isAsciiLower, isAsciiUpper)
import Data.Set as S (fromList, toList, intersection)

type Item = Char
type Rucksack = ([Item], [Item])

rucksackParser :: Parser Rucksack
rucksackParser = makeRucksack <$> many1 alphaNum

makeRucksack :: [Item] -> Rucksack
makeRucksack items = splitAt (length items `div` 2) items

findErrors :: Rucksack -> [Item]
findErrors (first, second) = S.toList $ S.intersection first' second'
  where
    first' = S.fromList first
    second' = S.fromList second

priority :: Item -> Int
priority c | isAsciiLower c = ord c - ord 'a' + 1
priority c | isAsciiUpper c = ord c - ord 'A' + 27
priority _ = error "unexpected char"

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  rucksacks <- parseInput $ sepEndBy1 rucksackParser $ char '\n'
  print $ sum $ concatMap (map priority . findErrors) rucksacks
