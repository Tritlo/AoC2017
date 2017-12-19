module AoCUtils where

import Data.Monoid
import Control.Monad

while :: Monad m => (a -> Bool) -> m a -> m [a]
while cond act = act >>= rec
    where rec a | cond a = flip ((<$>) . (<>) . pure) (while cond act) a
          rec _  = pure mempty

readInput :: IO [String]
readInput = while (/= "") getLine

iter :: (a -> a) -> Int -> a -> a
iter _ 0 a = a
iter f n a = iter f (n-1) (f a)

iter' :: (a -> a) -> Int -> a -> a
iter' _ 0 a = a
iter' f n a = iter' f (n-1) $! (f a)

validate :: Eq b => (a -> b) -> [(a, b)] -> Bool
validate _ [] = True
validate f ((t,r):ts) | (f t) == r = validate f ts
validate _ _ = False
