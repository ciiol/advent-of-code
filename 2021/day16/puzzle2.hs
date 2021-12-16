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

-- Literal values (type ID 4) represent a single number as described above. The remaining type IDs are more interesting:
--     Packets with type ID 0 are sum packets - their value is the sum of the values of their sub-packets.
--       If they only have a single sub-packet, their value is the value of the sub-packet.
--     Packets with type ID 1 are product packets - their value is the result of multiplying
--       together the values of their sub-packets. If they only have a single sub-packet,
--       their value is the value of the sub-packet.
--     Packets with type ID 2 are minimum packets - their value is the minimum of the
--       values of their sub-packets.
--     Packets with type ID 3 are maximum packets - their value is the maximum of the
--       values of their sub-packets.
--     Packets with type ID 5 are greater than packets - their value is 1 if the value
--       of the first sub-packet is greater than the value of the second sub-packet;
--       otherwise, their value is 0. These packets always have exactly two sub-packets.
--     Packets with type ID 6 are less than packets - their value is 1 if the value of
--       the first sub-packet is less than the value of the second sub-packet; otherwise,
--       their value is 0. These packets always have exactly two sub-packets.
--     Packets with type ID 7 are equal to packets - their value is 1 if the value of
--       the first sub-packet is equal to the value of the second sub-packet; otherwise,
--       their value is 0. These packets always have exactly two sub-packets.

-- What do you get if you evaluate the expression represented by your hexadecimal-encoded BITS transmission?

import Data.Char (digitToInt)
import Data.Either (either)
import Data.List (concatMap, foldl')
import System.Exit (exitFailure)
import Text.ParserCombinators.Parsec (Parser, alphaNum, char, count, digit, many, many1, parse, (<|>), unexpected)

data Header = Header Integer Integer deriving (Eq, Ord, Show)

data Packet = Packet Header PacketBody deriving (Eq, Ord, Show)

data PacketBody = Single Integer
                | Sum [Packet]
                | Product [Packet]
                | Minimum [Packet]
                | Maximum [Packet]
                | GreaterThen Packet Packet
                | LessThen Packet Packet
                | Equal Packet Packet
                deriving (Eq, Ord, Show)

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
parsePacketBody 0 = Sum <$> parseMultiplePackets
parsePacketBody 1 = Product <$> parseMultiplePackets
parsePacketBody 2 = Minimum <$> parseMultiplePackets
parsePacketBody 3 = Maximum <$> parseMultiplePackets
parsePacketBody 4 = Single <$> parseSingleValue
parsePacketBody 5 = applyTwo GreaterThen <$> parseMultiplePackets
parsePacketBody 6 = applyTwo LessThen <$> parseMultiplePackets
parsePacketBody 7 = applyTwo Equal <$> parseMultiplePackets
parsePacketBody n = unexpected $ "typeID == " ++ show n

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

applyTwo :: (Packet -> Packet -> a) -> [Packet] -> a
applyTwo f [p1, p2] = f p1 p2
applyTwo _ packets = error $ "unexpected number of packets: " ++ show (length packets)

parseSingleValue :: Parser Integer
parseSingleValue = decodeInteger . concat <$> parseBlocks
  where
    parseBlocks = char '0' *> (singleton <$> rawBits 4) <|> char '1' *> ((:) <$> rawBits 4 <*> parseBlocks)

singleton :: a -> [a]
singleton x = [x]

calc :: Packet -> Integer
calc (Packet _ body) = calcBody body
  where
    calcBody (Single num) = num
    calcBody (Sum packets) = sum $ map calc packets
    calcBody (Product packets) = product $ map calc packets
    calcBody (Maximum packets) = maximum $ map calc packets
    calcBody (Minimum packets) = minimum $ map calc packets
    calcBody (Equal p1 p2) = if calc p1 == calc p2 then 1 else 0
    calcBody (GreaterThen p1 p2) = if calc p1 > calc p2 then 1 else 0
    calcBody (LessThen p1 p2) = if calc p1 < calc p2 then 1 else 0

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
  print $ calc $ parseString (makeBits input) parsePacket
