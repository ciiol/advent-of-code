#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2021/day/8
-- Each digit of a seven-segment display is rendered by turning on or off any of seven
-- segments named a through g.

-- The signals which control the segments have been mixed up on each display.
-- The submarine is still trying to display numbers by producing output on signal wires a through g,
-- but those wires are connected to segments randomly.

-- In the output values, how many times do digits 1, 4, 7, or 8 appear?

import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Language.Haskell.TH (Dec)
import System.Exit (exitFailure)
import Text.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec (Parser, char, letter, many1, parse, sepEndBy1, string)

type Code = [Char]

data Rule = Rule [Code] [Code] deriving (Show)

type Decoder = M.Map Code Int

codeParser :: Parser Code
codeParser = do
  code <- many1 letter
  return $ sort code

ruleParser :: Parser Rule
ruleParser = do
  samples <- sepEndBy1 codeParser $ char ' '
  _ <- string "| "
  output <- sepEndBy1 codeParser $ char ' '
  return $ Rule samples output

makeDecoder :: [Code] -> Decoder
makeDecoder = M.fromList . mapMaybe deduce
  where
    deduce code | length code == 2 = Just (code, 1)
    deduce code | length code == 3 = Just (code, 7)
    deduce code | length code == 4 = Just (code, 4)
    deduce code | length code == 7 = Just (code, 8)
    deduce _ = Nothing

decode :: Rule -> [Int]
decode (Rule samples output) = mapMaybe (decode' $ makeDecoder samples) output
  where
    decode' = flip M.lookup

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  rules <- parseInput $ sepEndBy1 ruleParser $ char '\n'
  print $ length $ concatMap decode rules
