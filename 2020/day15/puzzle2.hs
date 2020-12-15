#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p
{-# LANGUAGE BangPatterns #-}

-- https://adventofcode.com/2020/day/15#part2
-- Given your starting numbers, what will be the 30000000th number spoken?

import qualified Text.ParserCombinators.Parsec.Token as P
import qualified Data.Map.Strict as M
import Text.ParserCombinators.Parsec (Parser, parse, sepBy)
import Text.Parsec.Language (emptyDef)
import System.Exit (exitFailure)

startParser :: Parser [Integer]
startParser = sepBy (P.integer lexer) (P.comma lexer)

lexer = P.makeTokenParser emptyDef

nextNumber :: Integer -> Integer -> M.Map Integer Integer -> Integer
nextNumber x n index = applyRules prev
  where
    prev = M.lookup x index
    applyRules (Just n') | n' == n - 1 = 1
    applyRules (Just n') = n - n'
    applyRules Nothing = 0

traceNumbers :: [Integer] -> [Integer]
traceNumbers start = start ++ traceNumbers' (fromIntegral $ length start) (last start) index
  where
    index = M.fromList $ zip (init start) [1..]
    traceNumbers' n x i = next : traceNumbers' next_n next (M.insert x n i)
      where
        !next_n = n + 1
        !next = nextNumber x n i

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
  putStrLn $ show $ last $ take 30000000 $ traceNumbers start
