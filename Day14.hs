{-# LANGUAGE TypeApplications #-}
import Day10 (knotHash)
import Day12

import Text.Printf
import Debug.Trace
import Data.Bits

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Arrow

import Data.Set (Set)
import qualified Data.Set as S
import Control.Arrow ((&&&))

import Data.Maybe

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

toFree :: String -> [[Bool]]
toFree = map (toDumbBits . knotHash . traceShowId) . keyToHashes




enumerate :: [[Bool]] -> [[Maybe Int]]
enumerate = btdi 0 []
    where -- Not tail recursive for partial evaluation
        btdi :: Int ->  [Maybe Int] -> [[Bool]] -> [[Maybe Int]]
        btdi _ [] [] = []
        btdi n row ((x:xs):ys) | x = btdi (n+1) ((Just n):row) (xs:ys)
        btdi n row ((x:xs):ys) = btdi n (Nothing:row) (xs:ys)
        btdi n row ([]:ys) = (reverse row):(btdi n [] ys)

adjacent :: [[Maybe Int]] -> Map Int (Set Int)
adjacent freeMap = buildAdjFrom (0,0) M.empty
    where freeMapX = (length . head) freeMap
          freeMapY = length freeMap
          lookup (x,y) |  x < 0 || x >= freeMapX
                       || y < 0 || y >= freeMapY = Nothing
          lookup (x,y) =  (freeMap !! y) !! x
          neighbors (x,y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
          buildAdjFrom (x,y) adjM | y >= freeMapY = adjM
          buildAdjFrom (x,y) adjM | x >= freeMapX = buildAdjFrom (0, y+1) adjM
          buildAdjFrom loc@(x,y) adjM = case lookup (x,y) of
                                      Nothing -> buildAdjFrom (x+1, y) adjM
                                      Just n -> buildAdjFrom (x+1, y) $ M.insert n (S.fromList $ mapMaybe lookup (neighbors loc )) adjM

part1 :: String -> Int
part1 = sum . (map (length . filter id)) . toFree
part2 :: String -> Int
part2 = countComponents . adjacent . enumerate . toFree


main :: IO ()
main = (part1 &&& part2) <$> return input >>= print
