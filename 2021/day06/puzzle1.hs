#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2021/day/6
-- You know nothing about this specific species of lanternfish,
-- you make some guesses about their attributes. Surely, each
-- lanternfish creates a new lanternfish once every 7 days.

-- However, this process isn't necessarily synchronized between
-- every lanternfish - one lanternfish might have 2 days left until
-- it creates another lanternfish, while another might have 4. So,
-- you can model each fish as a single number that represents the
-- number of days until it creates a new lanternfish.

-- Furthermore, you reason, a new lanternfish would surely need
-- slightly longer before it's capable of producing more lanternfish:
-- two more days for its first cycle.

-- How many lanternfish would there be after 80 days?

import Data.List (concatMap)
import System.Exit (exitFailure)
import Text.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec (Parser, char, parse, sepBy1)
import qualified Text.ParserCombinators.Parsec.Token as P

type Fish = Integer

lexer = P.makeTokenParser emptyDef

fishesParser :: Parser [Fish]
fishesParser = sepBy1 (P.natural lexer) (char ',')

simulate :: [Fish] -> [[Fish]]
simulate fishes = fishes : simulate next
  where
    next = concatMap simulateFish fishes
    simulateFish 0 = [6, 8]
    simulateFish x = [x - 1]

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  fishes <- parseInput fishesParser
  print $ length $ simulate fishes !! 80
