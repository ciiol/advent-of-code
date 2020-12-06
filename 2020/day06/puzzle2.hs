#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/6#part2
-- As you finish the last group's customs declaration,
-- you notice that you misread one word in the instructions:

-- You don't need to identify the questions to which anyone answered
-- "yes"; you need to identify the questions to which
-- everyone answered "yes"!

import Text.ParserCombinators.Parsec
import System.Exit (exitFailure)
import Data.Set (Set, empty, fromList, size, intersection)

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
groupForm (h:t) = foldl intersection (fromList h) $ map fromList t
groupForm [] = empty

main :: IO ()
main = do
  groups <- parseInput groupsParser
  putStrLn $ show $ sum $ map (size . groupForm) groups
