#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/10
-- What is the total number of distinct ways you can arrange
-- the adapters to connect the charging outlet to your device?

import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec (Parser, many, parse)
import Text.Parsec.Language (emptyDef)
import System.Exit (exitFailure)
import Data.List (sort, subsequences)

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
addBorders nums = [((minimum nums) - 1)] ++ nums ++ [(3 + (maximum nums))]

isValid :: [Int] -> Bool
isValid nums = all (< 4) $ diffs nums

validSubsequencesNaive :: [Int] -> Int
validSubsequencesNaive nums = length $ filter isValid $ subsequences' nums
  where
    subsequences' s = map (addRest s) $ subsequences $ tail $ init s
    addRest s x = [(head s)] ++ x ++ [(last s)]

validSubsequencesBrute :: [Int] -> Int
validSubsequencesBrute nums = foldr (*) 1 $ map validSubsequencesNaive $ intervals nums

intervals :: [Int] -> [[Int]]
intervals nums = intervals' $ zip (tail nums) (diffs nums)
  where
    intervals' [] = []
    intervals' s = (addBorders $ map fst $ takeWhile ((== 1) . snd) s) : (intervals' $ rest s)
    rest s = dropWhile ((== 3) . snd) $ dropWhile ((== 1) . snd) s

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
  putStrLn $ show $ validSubsequencesBrute $ sort $ addBorders nums
