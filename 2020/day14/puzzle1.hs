#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2020/day/14
-- The initialization program (your puzzle input) can either update the
-- bitmask or write a value to memory. Values and memory addresses are
-- both 36-bit unsigned integers. For example, ignoring bitmasks for a
-- moment, a line like mem[8] = 11 would write the value 11 to memory
-- address 8.

-- The bitmask is always given as a string of 36 bits, written with the
-- most significant bit (representing 2^35) on the left and the least
-- significant bit (2^0, that is, the 1s bit) on the right. The current
-- bitmask is applied to values immediately before they are written to
-- memory: a 0 or 1 overwrites the corresponding bit in the value, while
-- an X leaves the bit in the value unchanged.

-- Execute the initialization program. What is the sum of all values left
-- in memory after it completes?

import qualified Text.ParserCombinators.Parsec.Token as P
import qualified Data.Bits as B
import qualified Data.Map as M
import Data.Bits ((.&.), (.|.))
import Data.Word (Word64)
import Text.ParserCombinators.Parsec (Parser, parse, many, try, (<|>), digit, char)
import Text.Parsec.Language (emptyDef)
import System.Exit (exitFailure)

data Operation = SetMask Mask | Write Address MemoryValue deriving (Show, Eq, Ord)
data Mask = Mask {mOnes :: MaskField, mZeros :: MaskField} deriving (Show, Eq, Ord)
type MaskField = Word64
type MemoryValue = Word64
type Address = Integer
type Memory = M.Map Address MemoryValue
data Computer = Computer {cMemory :: Memory, cMask :: Mask} deriving (Show, Eq, Ord)

operationsParser :: Parser [Operation]
operationsParser = many operationParser

operationParser :: Parser Operation
operationParser = maskParser <|> writeParser

maskParser :: Parser Operation
maskParser = do
  _ <- try $ P.symbol lexer "mask"
  _ <- P.symbol lexer "="
  arg <- P.lexeme lexer $ many $ digit <|> char 'X'
  return $ SetMask $ makeMask arg

writeParser :: Parser Operation
writeParser = do
  _ <- try $ P.symbol lexer "mem"
  address <- P.brackets lexer (P.integer lexer)
  _ <- P.symbol lexer "="
  value <- P.integer lexer
  return $ Write address $ fromIntegral value

lexer = P.makeTokenParser emptyDef

makeMask :: [Char] -> Mask
makeMask = makeMask' 0 0
  where
    makeMask' acc0 acc1 [] = Mask {mZeros = (B.complement acc0), mOnes = acc1}
    makeMask' acc0 acc1 ('0':xs) = makeMask' (acc0 * 2 + 1) (acc1 * 2) xs
    makeMask' acc0 acc1 ('1':xs) = makeMask' (acc0 * 2) (acc1 * 2 + 1) xs
    makeMask' acc0 acc1 ('X':xs) = makeMask' (acc0 * 2) (acc1 * 2) xs
    makeMask' _acc0 _acc1 (x:_xs) = error $ "unexpected token " ++ show x

applyMask :: Mask -> MemoryValue -> MemoryValue
applyMask mask = (.|.) (mOnes mask) . (.&.) (mZeros mask)

makeComputer :: Computer
makeComputer = Computer M.empty $ makeMask []

applyOperation :: Computer -> Operation -> Computer
applyOperation c (SetMask mask) = c {cMask = mask}
applyOperation c (Write address value) = c {cMemory = newMemory}
  where
    newMemory = M.insert address (applyMask (cMask c) value) $ cMemory c

allMemoryValues :: Computer -> [MemoryValue]
allMemoryValues = map snd . M.toList . cMemory

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
  operations <- parseInput operationsParser
  putStrLn $ show $ sum $ allMemoryValues $ foldl applyOperation makeComputer operations
