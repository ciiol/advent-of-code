#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2022/day/3#part2
-- For safety, the Elves are divided into groups of three. Every Elf carries a badge
-- that identifies their group. For efficiency, within each group of three Elves, the
-- badge is the only item type carried by all three Elves. That is, if a group's badge
-- is item type B, then all three Elves will have item type B somewhere in their rucksack,
-- and at most two of the Elves will be carrying any other item type.

-- Every set of three lines in your list corresponds to a single group, but each group
-- can have a different badge item type.

-- Find the item type that corresponds to the badges of each three-Elf group.
-- What is the sum of the priorities of those item types?

import Text.ParserCombinators.Parsec (Parser, sepEndBy1, many1, alphaNum, char, parse)
import System.Exit (exitFailure)
import Data.Char (ord, isAsciiLower, isAsciiUpper)
import Data.Set as S (fromList, toList, intersection)

type Item = Char
type Rucksack = [Item]

rucksackParser :: Parser Rucksack
rucksackParser = many1 alphaNum

findBadge :: [Rucksack] -> Item
findBadge rucksacks = head $ S.toList commonItems
  where
    rucksacks' = map S.fromList rucksacks
    commonItems = foldl1 S.intersection rucksacks'

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n list = fst splitted : chunks n (snd splitted)
  where
    splitted = splitAt n list

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
  print $ sum $ map (priority . findBadge) $ chunks 3 rucksacks
