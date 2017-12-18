
{-# LANGUAGE TypeApplications #-}
import Data.Array

import Data.Ix
import Debug.Trace
import Text.Printf

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Data.Char
import Data.Bits

-- While loops
import Data.Monoid
import Control.Monad
import Control.Arrow

while :: Monad m => (a -> Bool) -> m a -> m [a]
while cond act = act >>= rec
    where rec a | cond a = flip ((<$>) . (<>) . pure) (while cond act) a
          rec _  = pure mempty

readInput :: IO [String]
readInput = while (/= "") getLine


liToArr :: [Int] -> Array Int Int
liToArr li =listArray (0, (length li) -1) li


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
toDenseHash xs = (toHexPair $ foldl1 xor $ take 16 xs) ++ (toDenseHash (drop 16 xs))
    where toHexPair n = printf "%c%c" (intToDigit $ n `div` 16)  (intToDigit $ n `mod` 16)


exec2 :: [Int] -> String
exec2 lengths = toDenseHash $ execRounds numRounds (0,0, (liToArr [0..(arrLen-1)]))
    where
      execRounds 0 (_, _, arr) = elems arr
      execRounds n v = execRounds (n-1) $ execRound v lengths
      execRound :: (Int, Int, Array Int Int) -> [Int] -> (Int, Int, Array Int Int)
      execRound (cp, ss, arr) = go cp ss arr
        where
          go :: Int -> Int -> Array Int Int -> [Int] -> (Int, Int,  Array Int Int)
          go cp ss arr [] = (cp, ss, arr)
          go cp ss arr (l:ls) = go ncp nss  (ixmap (bounds arr) indf arr) ls
            where inds = map (`mod` arrLen) [cp..(cp+l-1)]
                  mapping = M.fromList $ zip inds (reverse inds)
                  indf =  \x -> M.findWithDefault x x mapping
                  ncp = (cp + l + ss) `mod` arrLen
                  nss = ss + 1

-- Part 2
initVec = [17, 31, 73, 47, 23]

main :: IO ()
main = (arr (exec1 . read . printf "[%s]") &&& arr (exec2 . (++ initVec) . map ord ) ). head <$> readInput >>= print
