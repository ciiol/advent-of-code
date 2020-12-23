#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/23
-- The cups will be arranged in a circle and labeled clockwise (your puzzle input).

-- Before the crab starts, it will designate the first cup in your list as the current cup.
-- The crab is then going to do 100 moves.

-- Each move, the crab does the following actions:

--     - The crab picks up the three cups that are immediately clockwise of
--     the current cup. They are removed from the circle; cup spacing is
--     adjusted as necessary to maintain the circle.
--     - The crab selects a destination cup: the cup with a label equal to
--     the current cup's label minus one. If this would select one of the
--     cups that was just picked up, the crab will keep subtracting one until
--     it finds a cup that wasn't just picked up. If at any point in this
--     process the value goes below the lowest value on any cup's label,
--     it wraps around to the highest value on any cup's label instead.
--     - The crab places the cups it just picked up so that they are
--     immediately clockwise of the destination cup. They keep the
--     same order as when they were picked up.
--     - The crab selects a new current cup: the cup which is immediately
--     clockwise of the current cup.

import qualified Data.List as L
import Text.ParserCombinators.Parsec (Parser, many1, parse, digit)
import System.Exit (exitFailure)

type Cup = Integer

singleton :: a -> [a]
singleton x = [x]

cupParser :: Parser Integer
cupParser = (read . singleton) <$> digit

shift :: [Cup] -> [Cup]
shift cups = map fst $ zip (drop 1 $ cycle cups) cups

insertAfter :: Cup -> [Cup] -> [Cup] -> [Cup]
insertAfter x cups allCups | x < (minimum allCups) = insertAfter (maximum allCups) cups allCups
insertAfter x cups allCups | L.notElem x allCups = insertAfter (x - 1) cups allCups
insertAfter x cups allCups = (takeWhile (/= x) allCups) ++ (x:cups) ++ (tail $ dropWhile (/= x) allCups)

playMove :: [Cup] -> [Cup]
playMove cups = shift $ insertAfter (current - 1) picked rest
  where
    picked = take 3 $ tail cups
    rest = current : (drop 4 cups)
    current = head cups

label :: [Cup] -> String
label cups = concat $ map (show . snd) $ tail $ zip cups (dropWhile (/= 1) $ cycle cups)

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
  cups <- parseInput $ many1 cupParser
  putStrLn $ show $ label $ last $ take 101 $ iterate playMove cups
