#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2021/day/1
-- This report indicates that, scanning outward from the submarine,
-- the sonar sweep found depths of 199, 200, 208, 210, and so on.

-- The first order of business is to figure out how quickly the depth increases,
-- just so you know what you're dealing with - you never know if the keys will
-- get carried into deeper water by an ocean current or a fish or something.

-- Count the number of times a depth measurement increases from the previous measurement.

-- How many measurements are larger than the previous measurement?

import Text.ParserCombinators.Parsec
import Text.Parsec.Language (emptyDef)
import System.Exit (exitFailure)
import qualified Text.ParserCombinators.Parsec.Token as P

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
    print $ increases depths
