#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/12#part2
-- Almost all of the actions indicate how to move a waypoint which is relative to the ship's position:

--     Action N means to move the waypoint north by the given value.
--     Action S means to move the waypoint south by the given value.
--     Action E means to move the waypoint east by the given value.
--     Action W means to move the waypoint west by the given value.
--     Action L means to rotate the waypoint around the ship left
--     (counter-clockwise) the given number of degrees.
--     Action R means to rotate the waypoint around the ship right
--     (clockwise) the given number of degrees.
--     Action F means to move forward to the waypoint a number of
--     times equal to the given value.

-- Figure out where the navigation instructions actually lead. What is the
-- Manhattan distance between that location and the ship's starting position?

import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec (Parser, (<|>), try, many, parse, char)
import Text.Parsec.Language (emptyDef)
import System.Exit (exitFailure)

data Direction = North | South | East | West deriving (Show, Eq, Ord)
data Instruction =
  Move Direction Distance |
  MoveForward Distance |
  TurnLeft Degrees |
  TurnRight Degrees
  deriving (Show, Eq, Ord)
type Distance = Int
type Degrees = Int
data Position = Position Int Int deriving (Show, Eq, Ord)
data Delta = Delta Int Int deriving (Show, Eq, Ord)
type Instructions = [Instruction]
data Ferry = Ferry
  {
    fWaypoint :: Delta,
    fPos :: Position
  }
  deriving (Show, Eq, Ord)

instructionsParser :: Parser Instructions
instructionsParser = many instructionParser

instructionParser :: Parser Instruction
instructionParser = moveParser <|> moveForwardParser <|> turnParser

turnParser :: Parser Instruction
turnParser = turnLeftParser <|> turnRightParser

directionParser :: Parser Direction
directionParser =
  char 'N' *> pure North
  <|> char 'S' *> pure South
  <|> char 'E' *> pure East
  <|> char 'W' *> pure West

moveParser :: Parser Instruction
moveParser = do
  d <- try $ directionParser
  arg <- P.integer lexer
  return $ Move d $ fromIntegral arg

moveForwardParser :: Parser Instruction
moveForwardParser = do
  _ <- try $ char 'F'
  arg <- P.integer lexer
  return $ MoveForward $ fromIntegral arg

turnLeftParser :: Parser Instruction
turnLeftParser = do
  _ <- try $ char 'L'
  arg <- P.integer lexer
  return $ TurnLeft $ fromIntegral arg

turnRightParser :: Parser Instruction
turnRightParser = do
  _ <- try $ char 'R'
  arg <- P.integer lexer
  return $ TurnRight $ fromIntegral arg

lexer = P.makeTokenParser emptyDef

makeFerry :: Ferry
makeFerry = Ferry (Delta 10 (-1)) (Position 0 0)

updateFPos :: (Position -> Position) -> Ferry -> Ferry
updateFPos f (Ferry d p) = Ferry d (f p)

updateFWaypoint :: (Delta -> Delta) -> Ferry -> Ferry
updateFWaypoint f (Ferry w p) = Ferry (f w) p

rotate :: Degrees -> Delta -> Delta
rotate 0 vector = vector
rotate angle vector | angle > 90 = rotate (angle - 90) $ rotate 90 vector
rotate angle vector | angle < 0 = rotate (360 + angle) vector
rotate 90 vector = rotate90 vector
  where
    rotate90 (Delta dx dy) = (Delta (-dy) dx)
rotate angle _vector = error $ "unsupported angle " ++ (show angle)

addToPosition :: Delta -> Position -> Position
addToPosition (Delta dx dy) (Position x y) = Position (x + dx) (y + dy)

delta :: Int -> Int -> Int
delta a b | a >= b = a - b
delta a b = b - a

manhattanDistance :: Position -> Position -> Int
manhattanDistance (Position x1 y1) (Position x2 y2) = (delta x1 x2) + (delta y1 y2)

directionToDelta :: Direction -> Delta
directionToDelta North = Delta 0 (-1)
directionToDelta South = Delta 0 1
directionToDelta East = Delta 1 0
directionToDelta West = Delta (-1) 0

multiplyDelta :: Int -> Delta -> Delta
multiplyDelta n (Delta x y) = Delta (x * n) (y * n)

addDelta :: Delta -> Delta -> Delta
addDelta (Delta x1 y1) (Delta x2 y2) = Delta (x1 + x2) (y1 + y2)

move :: Direction -> Distance -> Delta -> Delta
move direction dist = addDelta $ multiplyDelta dist $ directionToDelta direction

moveTowards :: Delta -> Int -> Position -> Position
moveTowards waypoint dist = addToPosition (multiplyDelta dist waypoint)

applyInstruction :: Instruction -> Ferry -> Ferry
applyInstruction (Move direction distance) ferry = updateFWaypoint (move direction distance) ferry
applyInstruction (MoveForward distance) ferry = updateFPos (moveTowards (fWaypoint ferry) distance) ferry
applyInstruction (TurnLeft angle) ferry = updateFWaypoint (rotate (-angle)) ferry
applyInstruction (TurnRight angle) ferry = updateFWaypoint (rotate angle) ferry

trace :: Instructions -> Ferry -> [Ferry]
trace [] ferry = [ferry]
trace (x:xs) ferry = ferry : trace xs nextFerry
  where
    nextFerry = applyInstruction x ferry

finish :: Instructions -> Ferry -> Position
finish path = fPos . last . trace path

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
  path <- parseInput instructionsParser
  putStrLn $ show $ manhattanDistance (Position 0 0) $ finish path makeFerry
