#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/9#part2
-- The final step in breaking the XMAS encryption relies on the
-- invalid number you just found: you must find a contiguous set
-- of at least two numbers in your list which sum to the invalid
-- number from step 1.

-- To find the encryption weakness, add together the smallest and
-- largest number in this contiguous range; in this example,
-- these are 15 and 47, producing 62.

-- What is the encryption weakness in your XMAS-encrypted list
-- of numbers?

import qualified Data.Set as S
import qualified Text.ParserCombinators.Parsec.Token as P
import Data.List (find)
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

suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes (_x:xs) = xs : (suffixes xs)

takeWhileF :: (a -> b -> (Bool, b)) -> b -> [a] -> [a]
takeWhileF f state (x:xs) =
  case f x state of
    (True, state') -> x : takeWhileF f state' xs
    (False, _) -> []
takeWhileF _f _state [] = []

takeWhilePossibleSum :: Integer -> [Integer] -> [Integer]
takeWhilePossibleSum s l = takeWhileF isPossibleSum 0 l
  where
    isPossibleSum x acc = ((acc + x) <= s, acc + x)

findSumSeq :: Integer -> [Integer] -> [Integer]
findSumSeq target l = findSum' $ map (takeWhilePossibleSum target) $ suffixes l
  where
    findSum' prefixes = maybe (error "no sum") id $ find isGoodSum prefixes
    isGoodSum list = (sum list) == target

hackCode :: Int -> [Integer] -> Integer
hackCode preambleSize code = firstCode + lastCode
  where
    sumSeq = findSumSeq (processCode preambleSize code) code
    firstCode = head sumSeq
    lastCode = last sumSeq

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
  putStrLn $ show $ hackCode 25 code
