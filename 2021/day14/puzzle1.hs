#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2021/day/14
-- The submarine manual contains instructions for finding the optimal polymer formula;
-- specifically, it offers a polymer template and a list of pair insertion rules
-- (your puzzle input). You just need to work out what polymer would result after
-- repeating the pair insertion process a few times.

-- Apply 10 steps of pair insertion to the polymer template and find the most
-- and least common elements in the result. What do you get if you take the
-- quantity of the most common element and subtract the quantity of
-- the least common element?

import Data.List (concatMap, group, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import System.Exit (exitFailure)
import Text.ParserCombinators.Parsec (Parser, alphaNum, char, many1, parse, skipMany, string)

type Template = [Char]

type Pair = (Char, Char)

type Rules = M.Map Pair Char

inputParser :: Parser (Template, Rules)
inputParser = (,) <$> many1 alphaNum <*> (M.fromList <$> many1 ruleParser)

ruleParser :: Parser (Pair, Char)
ruleParser = do
  _ <- skipMany $ char '\n'
  from <- (,) <$> alphaNum <*> alphaNum
  _ <- string " -> "
  to <- alphaNum
  return (from, to)

applyRules :: Template -> Rules -> Template
applyRules template rules = concatMap applyRules' (zip template $ tail (template ++ ['*']))
  where
    applyRules' x@(a, b) | x `M.member` rules = [a, fromJust $ M.lookup x rules]
    applyRules' (a, b) = [a]

result :: Template -> Rules -> Int -> Int
result template rules num = most - least
  where
    final = foldl applyRules template $ replicate num rules
    counts = sort $ map (\x -> (length x, x)) $ group $ sort final
    least = fst $ head counts
    most = fst $ last counts

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  (template, rules) <- parseInput inputParser
  print $ result template rules 10
