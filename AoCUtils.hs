{-# LANGUAGE TypeApplications #-}
module AoCUtils where

import Text.ParserCombinators.ReadP
import Data.Char

import Data.Monoid
import Control.Monad
import System.IO

import Debug.Trace
import Text.Printf

parseInt :: ReadP Int
parseInt = read @Int <$> many1 (satisfy (\x -> isDigit x || (x == '-')))

while :: Monad m => (a -> Bool) -> m a -> m [a]
while cond act = act >>= rec
    where rec a | cond a = flip ((<$>) . (<>) . pure) (while cond act) a
          rec _  = pure mempty

readInput :: IO [String]
readInput = while (/= "" ) (isEOF >>= \x -> if x then return "" else getLine)

iter :: (a -> a) -> Int -> a -> a
iter _ 0 a = a
iter f n a = iter f (n-1) (f a)

iter' :: (a -> a) -> Int -> a -> a
iter' _ 0 a = a
iter' f n a = iter' f (n-1) $! (f a)

iterProgress :: (a -> a) -> Int -> a -> a
iterProgress = iterProgress' 0
  where iterProgress' cp _ done a | cp == done = a
        iterProgress' cp f done a = trace (printf "%f %% done" $ (100 * ((fromInteger $ fromIntegral cp)/(fromInteger $ fromIntegral done)) :: Float)) $ iterProgress' (cp+1) f done $! (f a)

validate :: Eq b => (a -> b) -> [(a, b)] -> Bool
validate _ [] = True
validate f ((t,r):ts) | (f t) == r = validate f ts
validate _ _ = False
