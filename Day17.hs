{-# LANGUAGE BangPatterns #-}

import Data.Sequence (Seq, (><))

import qualified Data.Sequence as Sq

import AoCUtils


-- Part 1
rounds1 :: Int
rounds1 = 2017

getNextVal :: (Int, Int, Seq Int) -> Int
getNextVal (cp, _, sq) = Sq.index sq (cp+1)


upd :: Int -> (Int, Int, Seq Int) -> (Int, Int, Seq Int)
upd steps (cp, nv, !cb) = (np + 1, nv + 1, nb)
    where np = (cp + steps) `mod` nv
          nb = Sq.insertAt (np+1) nv cb

initS :: (Int, Int, Seq Int)
initS = (0, 1, Sq.singleton 0)

run :: Int -> Int -> (Int, Int, Seq Int)
run step rounds= iter (upd step) rounds initS

input = 376


-- Part 2

rounds2 :: Int
rounds2 = 50*1000*1000

-- We use the fact that we don't care about the rest of the list,
-- only what is after 0. Since 0 is always at the front, we're good to go!
-- The bang here is very important! Else we end up doing almost nothing but
-- GC.
go :: Int -> Int -> Int -> Int -> Int
go rs cp nv !cb = if rs == rounds2 then cb else go (rs+1) (np+1) (nv+1) nb
  where np = (cp + input) `mod` nv
        nb = if np == 0 then nv else cb

main :: IO ()
main = main1 >> main4
main1 = print (getNextVal (run input 2017))
main4 = print $ go 0 0 1 0
-- main2 = print $ getValAfter 0 (run2 input rounds2)
