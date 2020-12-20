#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/20#part2
-- The borders of each tile are not part of the actual image; start by removing them.

-- Now, you're ready to search for sea monsters! Because your image is
-- monochrome, a sea monster will look like this:

--                   #
-- #    ##    ##    ###
--  #  #  #  #  #  #

-- How many # are not part of a sea monster?

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec (Parser, many1, parse, endBy1, newline, char, (<|>))
import Text.Parsec.Language (emptyDef)
import System.Exit (exitFailure)
import Data.List (find)
import Data.Maybe (mapMaybe, listToMaybe)

data State = Active | Inactive deriving (Show, Eq, Ord, Enum, Bounded)
data Tile = Tile {tID :: Integer, tImage :: Image} deriving (Show, Eq, Ord)
type Descriptor = (Image -> [State], Image -> [State])
type Image = [[State]]
type Index = M.Map Coord State
type Pattern = S.Set Coord
type Coord = (Int, Int)

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
  return $ Tile tileID image

lexer = P.makeTokenParser emptyDef

top :: Descriptor
top = (head, last)

bottom :: Descriptor
bottom = (last, head)

left :: Descriptor
left = (map head, map last)

right :: Descriptor
right = (map last, map head)

rotate :: [[a]] -> [[a]]
rotate = foldr addHead []
  where
    addHead x [] = map (flip (:) []) x
    addHead x xs = map addHead' $ zip x xs
      where
        addHead' (l, ls) = l : ls

variants :: [[a]] -> [[[a]]]
variants = concat . map rotations . flips
  where
    rotations = take 4 . iterate rotate
    flips i = [i, reverse i, map reverse i, reverse $ map reverse i]

updateImage :: Tile -> Image -> Tile
updateImage t i = t {tImage = i}

fitTiles :: Descriptor -> Tile -> Tile -> Maybe Tile
fitTiles _f t1 t2 | (tID t1) == (tID t2) = Nothing
fitTiles f t1 t2 = maybe Nothing (Just . updateImage t2) $ find ((== (source $ tImage t1)) . dest) $ variants $ tImage t2
  where
    source = fst f
    dest = snd f

match :: Descriptor -> [Tile] -> Tile -> Maybe Tile
match f tiles t = listToMaybe $ mapMaybe (fitTiles f t) tiles

matchLine :: Descriptor -> [Tile] -> Tile -> [Tile]
matchLine f tiles t = matchLine' $ match f tiles t
  where
    matchLine' Nothing = [t]
    matchLine' (Just t') = t : (matchLine f tiles t')

makeLeftCorner :: [Tile] -> Tile
makeLeftCorner tiles = last $ matchLine left tiles $ last $ matchLine top tiles start
  where
    start = head tiles

matchGrid :: [Tile] -> [[Tile]]
matchGrid tiles = map (matchLine right tiles) $ matchLine bottom tiles start
  where
    start = makeLeftCorner tiles

unborder :: Tile -> Image
unborder = unborder' . tImage
  where
    unborder' = middle . map middle
    middle = init . tail

concatImagesHorizontal :: [Image] -> Image
concatImagesHorizontal ([]:_) = []
concatImagesHorizontal images = (concat $ map head images) : (concatImagesHorizontal $ map tail images)

concatImagesVertical :: [Image] -> Image
concatImagesVertical = concat

concatImages :: [[Image]] -> Image
concatImages = concatImagesVertical . map concatImagesHorizontal

fullImage :: [Tile] -> Image
fullImage = concatImages . map (map unborder) . matchGrid

index :: Image -> Index
index image = M.fromList [((x, y), p) |
  (y, row) <- zip [0..] image,
  (x, p) <- zip [0..] row,
  p == Active]

makePatterns :: [Pattern]
makePatterns = map (S.fromList . points) $ variants raw
  where
    points image = [(x, y) | (y, row) <- zip [0..] image, (x, p) <- zip [0..] row, p == '#']
    raw = ["                  # ",
           "#    ##    ##    ###",
           " #  #  #  #  #  #   "]

addCoord :: Coord -> Coord -> Coord
addCoord (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

hasPattern :: Index -> Coord -> Pattern -> Bool
hasPattern i c p= all (flip M.member i) $ map (addCoord c) $ S.toList p

findPatterns :: [Pattern] -> Image -> [Coord]
findPatterns patterns image = filter hasAnyPattern starts
  where
    starts = [(x, y) | y <- [0..(length image)], x <- [0..(length $ head image)]]
    indexed = index image
    hasAnyPattern c = any (hasPattern indexed c) patterns

countHabitat :: Image -> Int
countHabitat image = total - monsters
  where
    indexed = index image
    total = M.size indexed
    monsters = (S.size $ head makePatterns) * (length $ findPatterns makePatterns image)

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
  putStrLn $ show $ countHabitat $ fullImage tiles
