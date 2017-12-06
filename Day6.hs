
import Data.Array.MArray
import Data.Array.ST
import Control.Monad.ST

-- import Data.Array
import Data.Ix
import Debug.Trace
import Text.Printf

import Data.Map (Map)
import qualified Data.Map as M 
import Data.Array (Array, accum)
import Data.Maybe (fromJust)


testLi = [0,2,7,0]
realLi = [14, 0, 15, 12, 11, 11, 3, 5, 1, 6, 8, 4, 9, 1, 8, 4]

testMemBanks :: ST s (STUArray s Int Int)
testMemBanks = newListArray (0,3) testLi


realMemBanks :: ST s (STUArray s Int Int)
realMemBanks = newListArray (0,15) realLi


maxAndInd :: Ord a => [(Int, a)] -> (Int, a)
maxAndInd xs = maxAndInd' (head xs) xs
  where maxAndInd' p (x:xs) = maxAndInd' (if (snd x > snd p) then x else p) xs
        maxAndInd' p [] = p



updLi :: (STUArray s Int Int) -> ST s (STUArray s Int Int)
updLi arr = do (mi, mb) <- maxAndInd <$> getAssocs arr
               writeArray arr mi 0
               bounds@(minb, maxb) <- getBounds arr
               if (inRange bounds (mi+1)) then
                  updB (mi+1) mb arr
               else
                  updB minb mb arr
    where updB _ 0 arr = return arr
          updB ci b arr = do cb <- readArray arr ci
                             writeArray arr ci (cb+1)
                             bounds@(minb, maxb) <- getBounds arr
                             if (inRange bounds (ci+1)) then
                                updB (ci+1) (b-1) arr
                             else
                                updB minb (b-1) arr


liToArr :: [Int] -> ST s (STUArray s Int Int)
liToArr li = newListArray (0, (length li) -1) li

solve :: [Int] -> ST s (Int, Int)
solve inp = do arr <- liToArr inp
               solve' arr 0 M.empty
    where 
        solve' :: (STUArray s Int Int) -> Int -> Map (Array Int Int) Int -> ST s (Int, Int)
        solve' arr c seen = do fra <- freeze arr
                               if (fra `M.member` seen) then
                                  return (c, (-) c $ fromJust $ fra `M.lookup` seen)
                               else do
                                  narr <- updLi arr
                                  solve' narr (c+1) (M.insert fra c seen)

main :: IO ()
main = (stToIO $ solve realLi) >>= print

readInput :: IO [Int]
readInput = readInput' []
    where
        readInput' sf = do inp <- getLine
                           if (inp == "") then do {
                             return $ map read $ reverse sf
                           } else do {
                             readInput' (inp:sf)
                           }
