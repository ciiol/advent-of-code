#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2022/day/13#part2
-- Packet data consists of lists and integers. Each list starts with [, ends with ], and
-- contains zero or more comma-separated values (either integers or other lists). Each
-- packet is always a list and appears on its own line.

-- When comparing two values, the first value is called left and the second value is called right. Then:

--     If both values are integers, the lower integer should come first. If the
--       left integer is lower than the right integer, the inputs are in the right order.
--       If the left integer is higher than the right integer, the inputs are not in the
--       right order. Otherwise, the inputs are the same integer; continue checking the
--       next part of the input.
--     If both values are lists, compare the first value of each list, then the second value,
--       and so on. If the left list runs out of items first, the inputs are in the right order.
--       If the right list runs out of items first, the inputs are not in the right order. If
--       the lists are the same length and no comparison makes a decision about the order,
--       continue checking the next part of the input.
--     If exactly one value is an integer, convert the integer to a list which contains that
--       integer as its only value, then retry the comparison. For example, if comparing [0,0,0]
--       and 2, convert the right value to [2] (a list containing 2); the result is then foun
--       by instead comparing [0,0,0] and [2].

-- Afterward, locate the divider packets. To find the decoder key for this distress signal,
-- you need to determine the indices of the two divider packets and multiply them together.
-- (The first packet is at index 1, the second packet is at index 2, and so on.) In this example,
-- the divider packets are 10th and 14th, and so the decoder key is 140.

-- Organize all of the packets into the correct order.
-- What is the decoder key for the distress signal?

import Text.ParserCombinators.Parsec (Parser, parse, try, (<|>), char, digit, many, many1, sepEndBy1, sepEndBy)
import System.Exit (exitFailure)
import Data.List (sort, elemIndex)
import Data.Maybe (fromJust)

data Signal = L [Signal] | N Int deriving (Show, Eq)

signalParser :: Parser Signal
signalParser = try lParser <|> try nParser
  where
    nParser = N . read <$> many1 digit
    lParser = L <$> (char '[' *> sepEndBy signalParser (char ',') <* char ']')

signalsParser :: Parser [Signal]
signalsParser = sepEndBy1 signalParser (many $ char '\n')

instance Ord Signal where
  compare (N x1) (N x2) = compare x1 x2
  compare s1@(L _) s2@(N _) = compare s1 (L [s2])
  compare s1@(N _) s2@(L _) = compare (L [s1]) s2
  compare (L l1) (L l2) = compare l1 l2

decoder :: [Signal] -> Int
decoder s = index divider1 * index divider2
  where
    index x = 1 + fromJust (elemIndex x s)

divider1 :: Signal
divider1 = L [L [N 2]]

divider2 :: Signal
divider2 = L [L [N 6]]

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  signals <- parseInput signalsParser
  print $ decoder $ sort $ [divider1, divider2] ++ signals
