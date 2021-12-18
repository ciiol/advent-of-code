#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2021/day/17
-- This snailfish homework is about addition. To add two snailfish numbers, form a pair
-- from the left and right parameters of the addition operator.
-- For example, [1,2] + [[3,4],5] becomes [[1,2],[[3,4],5]].

-- There's only one problem: snailfish numbers must always be reduced, and
-- the process of adding two snailfish numbers can result in snailfish numbers
-- that need to be reduced.

-- To reduce a snailfish number, you must repeatedly do the first action in this
-- list that applies to the snailfish number:

--     If any pair is nested inside four pairs, the leftmost such pair explodes.
--     If any regular number is 10 or greater, the leftmost such regular number splits.

-- Once no action in the above list applies, the snailfish number is reduced.

-- What is the largest magnitude of any sum of two different snailfish numbers from the homework assignment?

import Data.List (find, foldl')
import Data.Maybe (fromMaybe, isJust)
import Data.Text.Array (new)
import System.Exit (exitFailure)
import Text.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec (Parser, many1, parse, (<|>))
import qualified Text.ParserCombinators.Parsec.Token as P

data FNum = Leaf Integer | Tree FNum FNum deriving (Show, Eq)

data PathEntry = L | R deriving (Show, Eq, Ord)

type Path = [PathEntry]

lexer = P.makeTokenParser emptyDef

fnumParser :: Parser FNum
fnumParser = Leaf <$> P.integer lexer <|> makeTree <$> P.brackets lexer (P.commaSep lexer fnumParser)
  where
    makeTree [a, b] = Tree a b
    makeTree _ = error "oops"

takeNum :: Path -> FNum -> FNum
takeNum [] n = n
takeNum _ n@(Leaf _) = n
takeNum (L : xs) (Tree a _) = takeNum xs a
takeNum (R : xs) (Tree _ b) = takeNum xs b

fromLeaf :: FNum -> Integer
fromLeaf (Leaf n) = n
fromLeaf _ = error "not a Leaf"

fromPair :: FNum -> (Integer, Integer)
fromPair (Tree (Leaf a) (Leaf b)) = (a, b)
fromPair n = error $ "not a pair of Leafs: " ++ show n

addFNum :: FNum -> FNum -> FNum
addFNum a b = normalize $ Tree a b

sumFNum :: [FNum] -> FNum
sumFNum [] = error "empty list"
sumFNum (x : xs) = foldl' addFNum x xs

pathes :: FNum -> [(FNum, Path)]
pathes = pathes' []
  where
    r = reverse
    pathes' p n@(Leaf _) = [(n, r p)]
    pathes' p n@(Tree a b) = (n, r p) : (pathes' (L : p) a ++ pathes' (R : p) b)

findInFNum :: (FNum -> Path -> Maybe a) -> FNum -> Maybe a
findInFNum f n = fromMaybe Nothing $ find isJust $ map (uncurry f) $ pathes n

findTreeOnDeep :: Int -> FNum -> Maybe Path
findTreeOnDeep n = findInFNum (findOnDeep' n)
  where
    findOnDeep' n (Tree _ _) path | length path == n = Just path
    findOnDeep' _ _ _ = Nothing

findGreaterThen :: Integer -> FNum -> Maybe Path
findGreaterThen n = findInFNum (findGreaterThen' n)
  where
    findGreaterThen' t (Leaf n) path | t < n = Just path
    findGreaterThen' _ _ _ = Nothing

parent :: Path -> Path
parent = init

updateAt :: Path -> FNum -> (FNum -> FNum) -> FNum
updateAt _ n@(Leaf _) f = f n
updateAt [] n f = f n
updateAt (L : xs) (Tree a b) f = Tree (updateAt xs a f) b
updateAt (R : xs) (Tree a b) f = Tree a (updateAt xs b f)

addTo :: Path -> FNum -> Integer -> FNum
addTo p n dx = updateAt p n (add dx)
  where
    add dx (Leaf n) = Leaf $ n + dx
    add _ _ = error "unexpected value to add"

leftPath :: Path -> Path
leftPath p = leftPath' $ reverse p
  where
    leftPath' [] = repeat L
    leftPath' (L : xs) = leftPath' xs
    leftPath' (R : xs) = reverse (L : xs) ++ repeat R

rightPath :: Path -> Path
rightPath p = rightPath' $ reverse p
  where
    rightPath' [] = repeat R
    rightPath' (R : xs) = rightPath' xs
    rightPath' (L : xs) = reverse (R : xs) ++ repeat L

addToLeftAt :: Path -> FNum -> Integer -> FNum
addToLeftAt p = addTo $ leftPath p

addToRightAt :: Path -> FNum -> Integer -> FNum
addToRightAt p = addTo $ rightPath p

replaceTo :: Path -> FNum -> FNum -> FNum
replaceTo p n to = updateAt p n (const to)

explode :: FNum -> FNum
explode n = explode' (findTreeOnDeep 4 n) n
  where
    explode' Nothing n = n
    explode' (Just p) n = replaceTo' (addToRightAt' (addToLeftAt' n a) b) (Leaf 0)
      where
        nums = fromPair $ takeNum p n
        a = fst nums
        b = snd nums
        addToLeftAt' = addToLeftAt p
        addToRightAt' = addToRightAt p
        replaceTo' = replaceTo p

split :: FNum -> FNum
split n = split' (findGreaterThen 9 n) n
  where
    split' Nothing n = n
    split' (Just p) n = replaceTo' n (Tree (Leaf a) (Leaf b))
      where
        num = fromLeaf $ takeNum p n
        a = floor $ fromIntegral num / 2
        b = ceiling $ fromIntegral num / 2
        replaceTo' = replaceTo p

normalize :: FNum -> FNum
normalize n = normalize' (findTreeOnDeep 4 n) (findGreaterThen 9 n) n
  where
    normalize' Nothing Nothing = id
    normalize' (Just _) _ = normalize . explode
    normalize' Nothing (Just _) = normalize . split

magnitude :: FNum -> Integer
magnitude (Tree a b) = magnitude a * 3 + magnitude b * 2
magnitude (Leaf a) = a

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

main :: IO ()
main = do
  nums <- parseInput $ many1 fnumParser
  print $ maximum $ [magnitude (sumFNum [n1, n2]) | n1 <- nums, n2 <- nums, n1 /= n2]
