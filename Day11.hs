{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE DeriveDataTypeable #-}

import Data.Data
import Data.Typeable

import Text.ParserCombinators.ReadP
import Text.Read hiding (get, choice, )
import Text.Printf
import Data.Char
import Control.Arrow

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

--- Looking forward to deriving via :)

data Dir = N | NE | SE | S | SW | NW
  deriving (Eq, Ord, Typeable, Data, Enum, Bounded)

instance Read Dir where
  readPrec = lift dirParser

instance Show Dir where
    show = map toLower . showConstr . toConstr

dirParser :: ReadP Dir
dirParser = choice $ map (\x -> x <$ (string . show) x) [minBound..]



pairAdd :: (Num a, Num b) => (a, b) -> (a, b) -> (a,b)
pairAdd (a,b) (c,d) = (a+c, b+d)



tests = ["ne,ne,ne", "ne,ne,sw,sw", "ne,ne,s,s", "se,sw,se,sw,sw"]

 -- Part 1
isSouth = (== 's') . head .show
isNorth = not . isSouth
isEast d = (length (show d) == 2) && (last (show d) == 'e')
isWest d = (length (show d) == 2) && (last (show d) == 'w')

totalD :: (Dir -> Bool) -> [Dir] -> (Int, Int)
totalD isDir xs =  (totalEasts - totalWests, pureDir)
    where onlyDir = filter isDir xs
          totalEasts = length $ filter isEast onlyDir
          totalWests = length $ filter isWest onlyDir
          pureDir = length onlyDir - (min totalEasts totalWests)


revPair :: (a, b) -> (b, a)
revPair (a,b) = (b,a)

coords :: [Dir] -> (Int, Int)
coords dirs = revPair $ pairAdd (((-) 0) <$> (totalD isSouth dirs)) (totalD isNorth dirs)

hexDist :: (Int, Int) -> Int
hexDist (lat, lon) = max alon alat
    where alon = abs lon
          alat = abs lat


solve1 = hexDist . coords
parse = read @[Dir] . printf "[%s]"

-- Part 2


main :: IO ()
-- main = print $ map (id &&& (hexDist . coords . parse) &&& (coords . parse)) tests
main = solve . parse . head <$> readInput >>= print





