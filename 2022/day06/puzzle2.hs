#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2022/day/6
-- The device will send your subroutine a datastream buffer (your puzzle input);
-- your subroutine needs to identify the first position where the four most
-- recently received characters were all different.

-- Your device's communication system is correctly detecting packets, but still isn't working.
-- It looks like it also needs to look for messages.

-- A start-of-message marker is just like a start-of-packet marker, except it consists of 
-- 14 distinct characters rather than 4.

-- How many characters need to be processed before the first start-of-message marker is detected?

import Text.ParserCombinators.Parsec (Parser, parse, letter, many1)
import System.Exit (exitFailure)
import Data.Set as S (fromList, size)

takePrefix :: String -> String
takePrefix [] = []
takePrefix s | S.size (S.fromList $ take 14 s) == 14 = take 14 s
takePrefix (s:xs) = s : takePrefix xs

makeMarker :: String -> String
makeMarker str | S.size (S.fromList str) == 4 = str
makeMarker _ = fail "invalid marker"

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  message <- parseInput $ many1 letter
  print $ length $ takePrefix message
