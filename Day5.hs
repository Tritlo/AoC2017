import Data.Array.MArray
import Data.Array.ST
import Control.Monad.ST

-- import Data.Array
import Data.Ix
import Debug.Trace
import Text.Printf



testArr :: [Int]
testArr = [0, 3, 0, 1, -3]


liToArr :: [Int] -> ST s (STUArray s Int Int)
liToArr li = newListArray (0, (length li) -1) li


solve :: [Int] -> ST s Int
solve x = liToArr x >>= go 0 0
  where go :: Int -> Int -> STUArray s Int Int -> ST s Int
        go i c a = do jumps <- readArray a i
                      let nj = if jumps >= 3 then jumps - 1 else jumps + 1
                      writeArray a i nj
                      let ni = i + jumps
                      bounds <- getBounds a
                      if (inRange bounds ni) then
                        go ni (c+1) a
                      else
                        return (c+1)


readInput :: IO [Int]
readInput = readInput' []
    where
        readInput' sf = do inp <- getLine
                           if (inp == "") then do {
                             return $  map read $ reverse sf
                           } else do {
                             readInput' (inp:sf)
                           }


main :: IO ()
main = readInput >>= stToIO . solve >>= print
