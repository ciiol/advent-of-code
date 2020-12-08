#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/8
-- The boot code is represented as a text file with one instruction
-- per line of text. Each instruction consists of an operation
-- (acc, jmp, or nop) and an argument (a signed number like +4 or -20).

-- Run your copy of the boot code. Immediately before any instruction
-- is executed a second time, what value is in the accumulator?

import qualified Data.Set as S
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec (Parser, (<|>), try, many, parse)
import Text.Parsec.Language (emptyDef)
import System.Exit (exitFailure)

data Instruction = Acc (Int) | Jmp (Int) | Nop deriving (Show, Eq, Ord)
type Code = [Instruction]
type Memory = Int
data Computer = Computer
  {
    computerInstruction :: Int,
    computerMemory :: Memory,
    computerCode :: Code
  }
  deriving (Show, Eq, Ord)

codeParser :: Parser Code
codeParser = many instructionParser

instructionParser :: Parser Instruction
instructionParser = jmpParser <|> accParser <|> nopParser

jmpParser :: Parser Instruction
jmpParser = do
  _ <- try $ P.symbol lexer "jmp"
  arg <- P.integer lexer
  return $ Jmp $ fromIntegral arg

accParser :: Parser Instruction
accParser = do
  _ <- try $ P.symbol lexer "acc"
  arg <- P.integer lexer
  return $ Acc $ fromIntegral arg

nopParser :: Parser Instruction
nopParser = do
  _ <- try $ P.symbol lexer "nop"
  _arg <- P.integer lexer
  return $ Nop

lexer = P.makeTokenParser emptyDef

makeComputer :: Code -> Computer
makeComputer = Computer 0 0

updateMemory :: (Memory -> Memory) -> Computer -> Computer
updateMemory f (Computer i m c) = Computer i (f m) c

updateInstruction :: (Int -> Int) -> Computer -> Computer
updateInstruction f (Computer i m c) = Computer (f i) m c

applyInstruction :: Instruction -> Computer -> Computer
applyInstruction (Acc num) = updateInstruction (+ 1) . updateMemory (+ num)
applyInstruction (Jmp num) = updateInstruction (+ num)
applyInstruction Nop = updateInstruction (+ 1)

nextInstruction :: Computer -> Instruction
nextInstruction computer = computerCode computer !! computerInstruction computer

trace :: Computer -> [Computer]
trace computer = computer : trace computer'
  where
    i = nextInstruction computer
    computer' = applyInstruction i computer

takeWhileF :: (a -> b -> (Bool, b)) -> b -> [a] -> [a]
takeWhileF f state (x:xs) =
  case f x state of
    (True, state') -> x : takeWhileF f state' xs
    (False, _) -> []
takeWhileF _f _state [] = []

takeCycle :: [Computer] -> [Computer]
takeCycle computers = takeWhileF isVisited S.empty computers
  where
    isVisited c visited = (not $ i `S.member` visited, i `S.insert` visited)
      where
        i = computerInstruction c

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
  code <- parseInput codeParser
  putStrLn $ show $ computerMemory $ last $ takeCycle $ trace $ makeComputer code
