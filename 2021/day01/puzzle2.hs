#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2021/day/1#part2
-- This report indicates that, scanning outward from the submarine,
-- the sonar sweep found depths of 199, 200, 208, 210, and so on.

-- Considering every single measurement isn't as useful as you expected: there's just too much noise in the data.
-- Instead, consider sums of a three-measurement sliding window.

-- How many sums are larger than the previous sum?

import Text.ParserCombinators.Parsec
import Text.Parsec.Language (emptyDef)
import System.Exit (exitFailure)
import qualified Text.ParserCombinators.Parsec.Token as P

windowed :: Int -> [Integer] -> [[Integer]]
windowed size list = windowed' $ splitAt size list
    where
        windowed' (head, []) = [head]
        windowed' (head, _rest) = head : windowed size (tail list)


increases :: [Integer] -> Int
increases depths = length $ filter increase pairs
    where
        increase (a, b) = a < b 
        pairs = zip depths $ tail depths

lexer = P.makeTokenParser emptyDef

depthsParser :: Parser [Integer]
depthsParser = many (P.natural lexer)

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
    where
        report err = do
            putStrLn $ "Error: " ++ show err
            exitFailure
        parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
    depths <- parseInput depthsParser
    print $ increases $ map sum $ windowed 3 depths
