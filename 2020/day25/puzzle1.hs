#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p
{-# LANGUAGE BangPatterns #-}

-- https://adventofcode.com/2020/day/24
-- The handshake used by the card and the door involves an operation that
-- transforms a subject number. To transform a subject number, start with the
-- value 1. Then, a number of times called the loop size, perform the
-- following steps:

--     - Set the value to itself multiplied by the subject number.
--     - Set the value to the remainder after dividing the value by 20201227.

-- The cryptographic handshake works like this:

--     - The card transforms the subject number of 7 according to the
--     card's secret loop size. The result is called the card's public key.
--     - The door transforms the subject number of 7 according to the
--     door's secret loop size. The result is called the door's public key.
--     - The card and door use the wireless RFID signal to transmit the
--     two public keys (your puzzle input) to the other device. Now, the
--     card has the door's public key, and the door has the card's public key.
--     Because you can eavesdrop on the signal, you have both public keys,
--     but neither device's loop size.
--     - The card transforms the subject number of the door's public key
--     according to the card's loop size. The result is the encryption key.
--     - The door transforms the subject number of the card's public key
--     according to the door's loop size. The result is the same
--     encryption key as the card calculated.

-- What encryption key is the handshake trying to establish?


import qualified Data.List as L
import Text.ParserCombinators.Parsec (Parser, many1, endBy1, digit, parse, newline)
import System.Exit (exitFailure)

type Key = Integer
type RoundNumber = Integer

keyParser :: Parser Key
keyParser = read <$> many1 digit

getRounds :: [RoundNumber] -> [(Key, RoundNumber)]
getRounds = getRounds' 1 0 . L.sort
  where
    getRounds' _acc _n [] = []
    getRounds' acc n (x:xs) | x == acc = (x, n) : (getRounds' acc n xs)
    getRounds' acc n keys = getRounds' next_acc next_n keys
      where
        !next_acc = ((acc * 7) `mod` 20201227)
        !next_n = n + 1

transformNumber :: Integer -> RoundNumber -> Key
transformNumber = transformNumber' 1
  where
    transformNumber' acc _ 0 = acc
    transformNumber' acc n r = transformNumber' next_acc n next_r
      where
        !next_acc = ((acc * n) `mod` 20201227)
        !next_r = r - 1

encryptionKey :: [(Key, RoundNumber)] -> Key
encryptionKey keys = transformNumber key roundN
  where
    lastKey = last keys
    firstKey = head keys
    key = fst lastKey
    roundN = snd firstKey

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
  keys <- parseInput $ endBy1 keyParser newline
  putStrLn $ show $ encryptionKey $ getRounds keys
