#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/15
-- In this game, the players take turns saying numbers.
-- They begin by taking turns reading from a list of starting
-- numbers (your puzzle input). Then, each turn consists of
--   considering the most recently spoken number:

--     If that was the first time the number has been spoken, the
--     current player says 0.
--     Otherwise, the number had been spoken before; the current player announces
--     how many turns apart the number is from when it was previously spoken.

-- Given your starting numbers, what will be the 2020th number spoken?

import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec (Parser, parse, sepBy)
import Text.Parsec.Language (emptyDef)
import System.Exit (exitFailure)

startParser :: Parser [Integer]
startParser = sepBy (P.integer lexer) (P.comma lexer)

lexer = P.makeTokenParser emptyDef

nextNumber :: [Integer] -> Integer
nextNumber [] = error "empty list"
nextNumber (x:xs) = applyRules prev
  where
    prev = take 1 $ map fst $ filter ((== x) . snd) $ zip [1..] xs
    applyRules [n1] | n1 == 1 = n1
    applyRules [n1] = n1
    applyRules [] = 0
    applyRules l = error $ "unexpected " ++ (show l) ++ " in " ++ (show (x:xs))

traceNumbers :: [Integer] -> [Integer]
traceNumbers start = start' ++ traceNumbers' start'
  where
    start' = reverse start
    traceNumbers' s = next : traceNumbers' (next:s)
      where
        next = nextNumber s

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do
      input <- getContents
      return $ parse p "(stdin)" input

main :: IO ()
main = do
  start <- parseInput startParser
  putStrLn $ show $ last $ take 2020 $ traceNumbers start
