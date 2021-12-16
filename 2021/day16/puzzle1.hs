#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p

-- https://adventofcode.com/2021/day/16
-- The transmission was sent using the Buoyancy Interchange Transmission System (BITS),
-- a method of packing numeric expressions into a binary sequence. Your submarine's computer
-- has saved the transmission in hexadecimal (your puzzle input).

-- Every packet begins with a standard header: the first three bits encode the packet version,
-- and the next three bits encode the packet type ID.

-- Packets with type ID 4 represent a literal value. Literal value packets encode a single binary number.
-- To do this, the binary number is padded with leading zeroes until its length is a multiple of four
-- bits, and then it is broken into groups of four bits. Each group is prefixed by a 1 bit except
-- the last group, which is prefixed by a 0 bit.

-- Every other type of packet (any packet with a type ID other than 4) represent an operator that performs some
-- calculation on one or more sub-packets contained within.

-- An operator packet contains one or more packets. To indicate which subsequent binary data
-- represents its sub-packets, an operator packet can use one of two modes indicated by the
-- bit immediately after the packet header; this is called the length type ID:

--     If the length type ID is 0, then the next 15 bits are a number that represents the total
--       length in bits of the sub-packets contained by this packet.
--     If the length type ID is 1, then the next 11 bits are a number that represents the number
--       of sub-packets immediately contained by this packet.

-- Finally, after the length type ID bit and the 15-bit or 11-bit field, the sub-packets appear.

-- Decode the structure of your hexadecimal-encoded BITS transmission; what do you get if you
-- add up the version numbers in all packets?

import Data.Char (digitToInt)
import Data.Either (either)
import Data.List (concatMap, foldl')
import System.Exit (exitFailure)
import Text.ParserCombinators.Parsec (Parser, alphaNum, char, count, digit, many, many1, parse, (<|>))

data Header = Header Integer Integer deriving (Eq, Ord, Show)

data Packet = Packet Header PacketBody deriving (Eq, Ord, Show)

data PacketBody = Single Integer | Operator [Packet] deriving (Eq, Ord, Show)

type Bits = [Char]

inputParser :: Parser [Char]
inputParser = many1 alphaNum

makeBits :: [Char] -> Bits
makeBits = concatMap convert
  where
    convert '0' = "0000"
    convert '1' = "0001"
    convert '2' = "0010"
    convert '3' = "0011"
    convert '4' = "0100"
    convert '5' = "0101"
    convert '6' = "0110"
    convert '7' = "0111"
    convert '8' = "1000"
    convert '9' = "1001"
    convert 'A' = "1010"
    convert 'B' = "1011"
    convert 'C' = "1100"
    convert 'D' = "1101"
    convert 'E' = "1110"
    convert 'F' = "1111"
    convert _ = error "oops"

bits :: Int -> Parser Integer
bits n = decodeInteger <$> rawBits n

rawBits :: Int -> Parser [Char]
rawBits n = count n digit

decodeInteger :: [Char] -> Integer
decodeInteger = toInteger . foldl' decode 0
  where
    decode acc d = acc * 2 + digitToInt d

parseHeader :: Parser Header
parseHeader = Header <$> bits 3 <*> bits 3

parsePacket :: Parser Packet
parsePacket = do
  version <- bits 3
  typeId <- bits 3
  body <- parsePacketBody typeId
  return $ Packet (Header version typeId) body

parsePacketBody :: Integer -> Parser PacketBody
parsePacketBody 4 = Single <$> parseSingleValue
parsePacketBody _ = Operator <$> parseMultiplePackets

parseMultiplePackets :: Parser [Packet]
parseMultiplePackets = char '0' *> parseMultiplePacketsByLen <|> char '1' *> parseMultiplePacketsByNum

parseMultiplePacketsByLen :: Parser [Packet]
parseMultiplePacketsByLen = do
  num <- bits 15
  body <- rawBits (fromIntegral num)
  return $ parseString body $ many parsePacket

parseMultiplePacketsByNum :: Parser [Packet]
parseMultiplePacketsByNum = do
  num <- bits 11
  count (fromIntegral num) parsePacket

parseSingleValue :: Parser Integer
parseSingleValue = decodeInteger . concat <$> parseBlocks
  where
    parseBlocks = char '0' *> (singleton <$> rawBits 4) <|> char '1' *> ((:) <$> rawBits 4 <*> parseBlocks)

singleton :: a -> [a]
singleton x = [x]

result :: Packet -> Integer
result (Packet (Header version _) body) = version + result' body
  where
    result' (Single num) = 0
    result' (Operator packets) = sum $ map result packets

parseInput :: Parser a -> IO a
parseInput parser = parseInput' parser >>= either report return
  where
    report err = do
      putStrLn $ "Error: " ++ show err
      exitFailure
    parseInput' p = do parse p "(stdin)" <$> getContents

parseString :: String -> Parser a -> a
parseString s parser = either (error . ("parse error: " ++) . show) id $ parse parser "" s

main :: IO ()
main = do
  input <- parseInput inputParser
  print $ result $ parseString (makeBits input) parsePacket
