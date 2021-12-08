#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2021/day/8
-- Each digit of a seven-segment display is rendered by turning on or off any of seven
-- segments named a through g.

-- The signals which control the segments have been mixed up on each display.
-- The submarine is still trying to display numbers by producing output on signal wires a through g,
-- but those wires are connected to segments randomly.

-- For each entry, determine all of the wire/segment connections and decode the four-digit
-- output values. What do you get if you add up all of the output values?

import Data.List (intersect, sort, union, (\\))
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, mapMaybe)
import Language.Haskell.TH (Dec)
import System.Exit (exitFailure)
import Text.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec (Parser, char, letter, many1, parse, sepEndBy1, string)

type Code = [Char]

data Rule = Rule [Code] [Code] deriving (Show)

type Decoder = M.Map Code Int

type RDecoder = M.Map Int Code

codeParser :: Parser Code
codeParser = do
  code <- many1 letter
  return $ sort code

ruleParser :: Parser Rule
ruleParser = do
  samples <- sepEndBy1 codeParser $ char ' '
  _ <- string "| "
  output <- sepEndBy1 codeParser $ char ' '
  return $ Rule samples output

makeBaseDecoder :: [Code] -> Decoder
makeBaseDecoder codes =
  M.fromList
    [ (byLen 2, 1),
      (byLen 3, 7),
      (byLen 4, 4),
      (byLen 7, 8),
      (byLen 7, 8),
      (byLen 5, 235),
      (byLen 6, 690)
    ]
  where
    byLen l = foldl intersect ['a' .. 'g'] $ filter ((== l) . length) codes

reversedDecoder :: Decoder -> RDecoder
reversedDecoder decoder = M.fromList [(v, k) | (k, v) <- M.toList decoder]

codeOf :: RDecoder -> Int -> Code
codeOf r num = fromJust $ M.lookup num r

mapping :: RDecoder -> String -> [Char]
mapping r "a" = codeOf r 7 \\ codeOf r 1
mapping r "bd" = codeOf r 4 \\ codeOf r 7
mapping r "eg" = codeOf r 8 \\ (codeOf r 7 `union` codeOf r 4)
mapping r "e" = mapping r "eg" \\ codeOf r 235
mapping r "b" = mapping r "bd" \\ codeOf r 235
mapping r "g" = mapping r "eg" \\ mapping r "e"
mapping r "d" = mapping r "bd" \\ mapping r "b"
mapping r "c" = codeOf r 1 \\ codeOf r 690
mapping r "f" = codeOf r 1 \\ mapping r "c"
mapping _ t = error "oops" ++ t

singleton :: a -> [a]
singleton a = [a]

--   0:      1:      2:      3:      4:
--  aaaa    ....    aaaa    aaaa    ....
-- b    c  .    c  .    c  .    c  b    c
-- b    c  .    c  .    c  .    c  b    c
--  ....    ....    dddd    dddd    dddd
-- e    f  .    f  e    .  .    f  .    f
-- e    f  .    f  e    .  .    f  .    f
--  gggg    ....    gggg    gggg    ....

--   5:      6:      7:      8:      9:
--  aaaa    aaaa    aaaa    aaaa    aaaa
-- b    .  b    .  .    c  b    c  b    c
-- b    .  b    .  .    c  b    c  b    c
--  dddd    dddd    ....    dddd    dddd
-- .    f  e    f  .    f  e    f  .    f
-- .    f  e    f  .    f  e    f  .    f
--  gggg    gggg    ....    gggg    gggg
makeDecoder :: [Code] -> Decoder
makeDecoder codes = M.fromList $ improved ++ M.toList base
  where
    base = makeBaseDecoder codes
    r = reversedDecoder base
    convert = sort . concatMap (mapping r . singleton)
    improved =
      [ (convert "abcefg", 0),
        (convert "acdeg", 2),
        (convert "acdfg", 3),
        (convert "abdfg", 5),
        (convert "abdefg", 6),
        (convert "abcdfg", 9)
      ]

decode :: Rule -> Int
decode (Rule samples output) = build $ map (fromJust . decode' (makeDecoder samples)) output
  where
    decode' = flip M.lookup
    build = foldl (\acc x -> acc * 10 + x) 0

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  rules <- parseInput $ sepEndBy1 ruleParser $ char '\n'
  print $ sum $ map decode rules
