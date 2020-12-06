#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/6
-- The form asks a series of 26 yes-or-no questions marked a through z.
-- All you need to do is identify the questions for which anyone in your
-- group answers "yes".

-- For each group, count the number of questions to which anyone
-- answered "yes". What is the sum of those counts?

import Text.ParserCombinators.Parsec
import System.Exit (exitFailure)
import Data.Set (Set, fromList, size)

type FormItem = Char
type Form = [FormItem]

groupsParser :: Parser [[Form]]
groupsParser = sepBy1 groupParser (char '\n')

groupParser :: Parser [Form]
groupParser = endBy1 (many1 alphaNum) (char '\n')

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do
      input <- getContents
      return $ parse p "(stdin)" input

groupForm :: [Form] -> Set FormItem
groupForm = fromList . concat

main :: IO ()
main = do
  groups <- parseInput groupsParser
  putStrLn $ show $ sum $ map (size . groupForm) groups
