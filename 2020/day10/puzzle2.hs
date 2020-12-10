#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/10
-- What is the total number of distinct ways you can arrange
-- the adapters to connect the charging outlet to your device?

import qualified Text.ParserCombinators.Parsec.Token as P
import qualified Data.Sequence as S
import Text.ParserCombinators.Parsec (Parser, many, parse)
import Text.Parsec.Language (emptyDef)
import System.Exit (exitFailure)
import Data.List (sort)
import Data.Sequence (Seq(Empty), (|>), dropWhileL, takeWhileR)
import Data.Foldable (toList)

intsParser :: Parser [Int]
intsParser = many intParser

intParser :: Parser Int
intParser = do
  num <- P.integer lexer
  return $ fromIntegral num

lexer = P.makeTokenParser emptyDef

addBorders :: [Int] -> [Int]
addBorders nums = 0 : (3 + (maximum nums)) : nums

pathCounts :: [Int] -> Int
pathCounts = snd . head . toList . S.reverse . foldl pathCounts' Empty
  where
    pathCounts' Empty x = Empty |> (x, 1)
    pathCounts' acc x = (shrink x acc) |> (x, (sum $ possiblePathCounts x acc))
    possiblePathCounts x = (map snd) . toList . takeWhileR ((>= x - 3) . fst)
    shrink current = dropWhileL ((< current - 3) . fst)

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
  putStrLn $ show $ pathCounts $ sort $ addBorders nums
