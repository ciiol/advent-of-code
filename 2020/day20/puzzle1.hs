#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/20
-- After decoding the satellite messages, you discover that the data actually
-- contains many small images created by the satellite's camera array. The
-- camera array consists of many cameras; rather than produce a single square
-- image, they produce many smaller square image tiles that need to be reassembled
-- back into a single image.

-- Assemble the tiles into an image. What do you get if you multiply together the
-- IDs of the four corner tiles?

import qualified Data.Set as S
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec (Parser, many1, parse, endBy1, newline, char, (<|>))
import Text.Parsec.Language (emptyDef)
import System.Exit (exitFailure)
import Data.List (sort)

data State = Active | Inactive deriving (Show, Eq, Ord, Enum, Bounded)
data Tile = Tile {tID :: Integer, tImage :: [[State]], tDesc :: S.Set Descriptor} deriving (Show, Eq, Ord)
type Descriptor = [[State]]

stateParser :: Parser State
stateParser =
  char '.' *> pure Inactive
  <|> char '#' *> pure Active

imageParser :: Parser [[State]]
imageParser = endBy1 (many1 stateParser) newline

tileParser :: Parser Tile
tileParser = do
  _ <- P.symbol lexer "Tile"
  tileID <- P.natural lexer
  _ <- P.symbol lexer ":"
  image <- P.lexeme lexer imageParser
  return $ makeTile tileID image

lexer = P.makeTokenParser emptyDef

makeTile :: Integer -> [[State]] -> Tile
makeTile tileID image = Tile tileID image $ S.fromList $ descriptors image

borders :: [[a]] -> [[a]]
borders array = [left, right, top, bottom]
  where
    top = head array
    bottom = last array
    left = map head array
    right = map last array

descriptors :: [[State]] -> [Descriptor]
descriptors = map normalize . borders
  where
    normalize a = sort [a, reverse a]

commonDescriptors :: Tile -> Tile -> S.Set Descriptor
commonDescriptors t1 t2 = S.intersection (tDesc t1) (tDesc t2)

commonDescriptorsAll :: Tile -> [Tile] -> S.Set Descriptor
commonDescriptorsAll t ts = foldr S.union S.empty $ common ts
  where
    common = map (commonDescriptors t)

findCorners :: [Tile] -> [Tile]
findCorners tiles = filter ((== 2) . commonNum tiles) tiles

commonNum :: [Tile] -> Tile -> Int
commonNum tiles t = S.size $ commonDescriptorsAll t $ filter (/= t) tiles

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
  tiles <- parseInput (many1 tileParser)
  putStrLn $ show $ product $ map tID $ findCorners tiles
