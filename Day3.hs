{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ApplicativeDo #-}

import Debug.Trace
import Text.Printf
import Data.Sequence (iterateN)
import Text.PrettyPrint (renderStyle, style)
import Data.Foldable (toList)
import Data.List (findIndex)
import Data.Maybe (isJust, fromJust, catMaybes)
import Control.Applicative (Alternative, empty)
import Data.Foldable (asum)


-- THIS IS SOME IMPERATIVE BULLSHIT

input1 :: Int
input1 = 368078

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

growMatrix :: [[Int]] -> [[Int]]
growMatrix m = im
  where (MAndI im _) = growMAndI (MAndI m undefined)

data MAndI = MAndI [[Int]] [[Int]] deriving (Show)

imandi = MAndI [[1]] [[1]]

growMAndI :: MAndI -> MAndI
growMAndI (MAndI m d) = MAndI new newd
  where addToEnds :: [a] -> a -> a -> [a]
        addToEnds xs e i  = (e:xs) ++ [i]

        prev :: Int
        prev = fromIntegral $ (last . last) m

        prevBase :: Int
        prevBase = (round . sqrt) $ fromIntegral prev

        next :: Int
        next =  prevBase + 2

        lr = prev + prevBase
        lt = lr + next
        ll = lt + prevBase
        lb = ll + next

        newRight = [(prev+1) .. lr]
        newTop = [lr+1 .. lt]
        newLeft = [lt+1 .. ll]
        newBot = [ll+1 .. lb]
        new = addToEnds newMids (reverse newTop) newBot

        newMids :: [[Int]]
        newMids = map (uncurry3 addToEnds) $ zip3 m newLeft (reverse newRight)

        toZ :: [Int] -> [Int]
        toZ = flip take (repeat 0) . length

        newd = addToEnds newMidsd (toZ newTop) $ toZ newBot
        newMidsd :: [[Int]]
        newMidsd = map (uncurry3 addToEnds) $ zip3 d (toZ newLeft) (toZ newRight)


socketMap :: [[Int]] -> Int -> [[Int]]
socketMap i v = if (last . last) curr <= v && (last . last) next >= v
                  then next
                  else socketMap next v
    where next = growMatrix i
          curr = i

indexInMap i m = if (isJust iiniinm)
                   then (Just (fromJust $ iiniinm, fromJust $ iinm !! (fromJust iiniinm)))
                   else Nothing
    where iinm = map (findIndex (== i)) m
          iiniinm =  findIndex isJust iinm



inp1Map = socketMap [[1]] input1

indOfStart = fromJust $ indexInMap 1 inp1Map
indOfTarget = fromJust $ indexInMap input1 inp1Map

dist = abs (xs - xt) + abs (ys - yt)
    where (xs,ys) = indOfStart
          (xt,yt) = indOfTarget


-- PART 2: REVENGE OF THE IMPERATIVISM


natToIndex :: Int -> MAndI -> ((Int,Int), MAndI)
natToIndex n mandi@(MAndI m d) = case (indexInMap n m) of
                            Just p -> (p, mandi)
                            Nothing -> natToIndex n $ growMAndI mandi

-- Monadic list lookup
mII :: (Monad f, Alternative f) => f [a] -> Int -> f a
mII lif i = do a <- lif
               if i >= 0 && i < length a
                 then pure (a !! i)
                 else empty


sumAround :: (Int,Int) -> [[Int]] -> Int
sumAround (x,y) dx =
   sum $ catMaybes [ (d `mII` x) `mII` (y-1)
                   , (d `mII` (x-1)) `mII` (y-1)
                   , (d `mII` (x+1)) `mII` (y-1)
                   , (d `mII` x) `mII` (y+1)
                   , (d `mII` (x-1)) `mII` (y+1)
                   , (d `mII` (x+1)) `mII` (y+1)
                   , (d `mII` (x+1)) `mII` y
                   , (d `mII` (x-1)) `mII` y]
    where d = pure dx

updMAndI :: Int -> MAndI -> (Int, MAndI)
updMAndI n mandi = (val, MAndI nm (setmdr loc val nd))
    where (loc, MAndI nm nd) = natToIndex n mandi
          val = sumAround loc nd
          setmdr (x,y) nv d = lxb++(ny:lxe)
            where ny = lyb ++ (nv:lye)
                  (lyb, _:lye) = splitAt y $ d !! x
                  (lxb, _:lxe) = splitAt x d


go :: Int
go = go' 2 imandi
  where go' n mandi = trace (printf "nval: %d n: %d" nval n) $ if nval >= input1 then nval else go' (n+1)  nmandi
         where (nval, nmandi) = updMAndI n mandi

main :: IO ()
main = print go >> print dist
