#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/13
-- Your notes (your puzzle input) consist of two lines. The first line is your estimate
-- of the earliest timestamp you could depart on a bus. The second line lists the bus
-- IDs that are in service according to the shuttle company; entries that show x must
-- be out of service, so you decide to ignore them.

-- What is the ID of the earliest bus you can take to the airport multiplied by the number
-- of minutes you'll need to wait for that bus?

import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec (Parser, parse, sepBy, char, (<|>))
import Text.Parsec.Language (emptyDef)
import System.Exit (exitFailure)
import Data.List (sortOn)

data Bus = OutOfService | Bus {bTime :: Time} deriving (Show, Eq, Ord)
type Time = Integer

busesParser :: Parser [Bus]
busesParser = sepBy busParser (P.comma lexer)

busParser :: Parser Bus
busParser = outOfServiceParser <|> goodBusParser

outOfServiceParser :: Parser Bus
outOfServiceParser = do
  _ <- char 'x'
  return OutOfService

goodBusParser :: Parser Bus
goodBusParser = do
  arg <- P.integer lexer
  return $ Bus arg

lexer = P.makeTokenParser emptyDef

onlyGood :: [Bus] -> [Bus]
onlyGood = filter (/= OutOfService)

timeToWait :: Time -> Bus -> Time
timeToWait start (Bus num) = num - (start `mod` num)
timeToWait _start OutOfService = error "oops"

findClosest :: Time -> [Bus] -> Bus
findClosest start = head . sortOn (timeToWait start)

result :: Time -> [Bus] -> Integer
result start buses = (timeToWait start closest) * (bTime closest)
  where
    closest = findClosest start buses

parseLine :: Parser a -> IO a
parseLine parser = parseLine' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseLine' p = do
      input <- getLine
      return $ parse p "(stdin)" input

main :: IO ()
main = do
  start <- parseLine (P.integer lexer)
  buses <- parseLine busesParser
  putStrLn $ show $ result start $ onlyGood buses
