{-# LANGUAGE TypeApplications #-}
import Day10 (knotHash)

import Text.Printf
import Debug.Trace
import Data.Bits


test = "flqrgnkx"

input = "jxqlasbh"

tests1 = [(test, 8108)]
tests2 = [(test, 1242)]


keyToHashes :: String -> [String]
keyToHashes key = map (printf "%s-%d" key) ([0..127] :: [Int])


toDumbBit :: Char -> [Bool]
toDumbBit c = reverse bitVals
    where intVal = read @Int (printf "0x%c" c)
          bitVals = map (testBit intVal) [0..3]

toDumbBits :: String -> [Bool]
toDumbBits = concat . map toDumbBit

part1 :: String -> Int
part1 = sum . (map (length . filter id)) .  map (toDumbBits . knotHash . traceShowId) . keyToHashes

part2 :: String -> Int
part2 = map (toDumbBits . knotHash . traceShowId) . keyToHashes


main :: IO ()
main = part1 <$> return test >>= print
