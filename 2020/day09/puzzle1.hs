#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/9
-- XMAS starts by transmitting a preamble of 25 numbers. After that,
-- each number you receive should be the sum of any two of the 25
-- immediately previous numbers. The two numbers will have different
-- values, and there might be more than one such pair.

-- What is the first number that does not have this property?

import qualified Data.Set as S
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec (Parser, many, parse)
import Text.Parsec.Language (emptyDef)
import System.Exit (exitFailure)

numbersParser :: Parser [Integer]
numbersParser = many (P.integer lexer)

lexer = P.makeTokenParser emptyDef

isSum :: S.Set Integer -> Integer -> Bool
isSum s n = any resInSet $ S.elems s
  where
    resInSet x = (n - x) `S.member` s


firstInvalid :: S.Set Integer -> [Integer] -> Integer
firstInvalid _ [] = error "no invalid"
firstInvalid s (x:xs) =
  case isSum s x of
    True -> firstInvalid (x `S.insert` s) xs
    False -> x

processCode :: Int -> [Integer] ->  Integer
processCode preambleSize nums = firstInvalid (S.fromList preamble) rest
  where
    preamble = take preambleSize nums
    rest = drop preambleSize nums


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
  code <- parseInput numbersParser
  putStrLn $ show $ processCode 25 code
