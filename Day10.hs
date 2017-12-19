
module Day10 where
{-# LANGUAGE TypeApplications #-}
import Data.Array

import Data.Ix
import Debug.Trace

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Data.Char
import Data.Bits

import Data.List (foldl')

import AoCUtils (iter')

liToArr :: [Int] -> Array Int Int
liToArr li = listArray (0, (length li) -1) li

arrLen :: Int
arrLen = 256

numRounds = 64

-- Part 1
exec1 :: [Int] -> Int
exec1 = go 0 0 (liToArr [0..(arrLen-1)])
    where go :: Int -> Int -> Array Int Int -> [Int] -> Int
          go _ _ arr [] = product $ take 2 (elems arr)
          go cp ss arr (l:ls) = go ncp nss  (ixmap (bounds arr) indf arr) ls
            where inds = map (`mod` arrLen) [cp..(cp+l-1)]
                  mapping = M.fromList $ zip inds (reverse inds)
                  indf =  \x -> M.findWithDefault x x mapping
                  ncp = (cp + l + ss) `mod` arrLen
                  nss = ss + 1


-- Part 2
toDenseHash :: [Int] -> String
toDenseHash [] = ""
toDenseHash xs = hexHash ++ (toDenseHash tl)
    where (hd, tl) = splitAt 16 xs
          xored = foldl1 xor hd
          (c1,c2) = xored `divMod` 16
          hexHash = map intToDigit [c1,c2]


exec2 :: [Int] -> String
exec2 lengths = (toDenseHash . elems . snd) $ iter' (round lengths) numRounds initVal
    where
      initVal :: ((Int, Int), Array Int Int)
      initVal = ((0,0) :: (Int,Int), (liToArr [0..(arrLen-1)]))
      round :: [Int] -> ((Int, Int), Array Int Int) -> ((Int, Int), Array Int Int)
      round = flip (foldl' step)
      step ((cp, ss), arr) l = ((ncp, nss), narr)
        where ncp = (cp + l + ss) `mod` arrLen
              nss = ss + 1
              inds = map (`mod` arrLen) [cp..(cp+l-1)]
              mapping = M.fromList $ zip inds (reverse inds)
              indf =  \x -> M.findWithDefault x x mapping
              narr = (ixmap (bounds arr) indf arr)

-- Part 2
initVec = [17, 31, 73, 47, 23]

knotHash :: String -> String
knotHash = exec2 . (++ initVec) . map ord

tests :: [(String, String)]
tests = [ (""         , "a2582a3a0e66e6e86e3812dcb672a272")
        , ("AoC 2017" , "33efeb34ea91902bb2f59c9920caa6cd")
        , ("1,2,3"    , "3efbe78a8d82f29979031a4aa0b16a9d")
        , ("1,2,4"    , "63960835bcdc130f0b66d7ff4f6a5a8e")
        ]


