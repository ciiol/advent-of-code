#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/13
-- The shuttle company is running a contest: one gold coin for anyone that can find the earliest
-- timestamp such that the first bus ID departs at that time and each subsequent listed bus
-- ID departs at that subsequent minute. (The first line in your input is no longer relevant.)

-- What is the earliest timestamp such that all of the listed bus IDs depart at offsets matching
-- their positions in the list?

import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec (Parser, parse, sepBy, char, (<|>))
import Text.Parsec.Language (emptyDef)
import System.Exit (exitFailure)

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

enumeratedGood :: [Bus] -> [(Time, Bus)]
enumeratedGood = filter ((/= OutOfService) . snd) . zip [0..]

findSequenceStart :: [Bus] -> Time
findSequenceStart buses = findSequenceStart' (enumeratedGood buses) 0 1
  where
    findSequenceStart' [] start _step = start
    findSequenceStart' ((i, (Bus n)):xs) start step | (start + i) `mod` n == 0 =
      findSequenceStart' xs start (step * n)
    findSequenceStart' b start step =
      findSequenceStart' b (start + step) step

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
  _start <- parseLine (P.integer lexer)
  buses <- parseLine busesParser
  putStrLn $ show $ findSequenceStart buses
