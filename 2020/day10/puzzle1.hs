#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/10
-- Find a chain that uses all of your adapters to connect the charging
-- outlet to your device's built-in adapter and count the joltage
-- differences between the charging outlet, the adapters, and your device.
-- What is the number of 1-jolt differences multiplied by the
-- number of 3-jolt differences?

import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec (Parser, many, parse)
import Text.Parsec.Language (emptyDef)
import System.Exit (exitFailure)
import Data.List (sort, group)

intsParser :: Parser [Int]
intsParser = many intParser

intParser :: Parser Int
intParser = do
  num <- P.integer lexer
  return $ fromIntegral num

lexer = P.makeTokenParser emptyDef

diffs :: [Int] -> [Int]
diffs list = map diff $ zip (tail list) list
  where
    diff (a, b) = a - b

addBorders :: [Int] -> [Int]
addBorders nums = 0 : (3 + (foldr max 0 nums)) : nums

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
  nums <- parseInput intsParser
  putStrLn $ show $ foldl (*) 1 $ map length $ group $ sort $ diffs $ sort $ addBorders nums
