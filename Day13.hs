{-# LANGUAGE TypeApplications #-}

import Text.ParserCombinators.ReadP
import Data.Char
import Text.Read hiding (get, choice, )

import Text.Printf
import Data.Maybe (fromJust)
import Control.Arrow

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.Set as S
import Data.Set (Set)

import Data.Monoid
import Control.Monad

import Debug.Trace



while :: Monad m => (a -> Bool) -> m a -> m [a]
while cond act = act >>= rec
    where rec a | cond a = flip ((<$>) . (<>) . pure) (while cond act) a
          rec _  = pure mempty

readInput :: IO [String]
readInput = while (/= "") getLine


type GoingDown = Bool

type ScanState = (Int, GoingDown)
type Depth = Int
type Layer = Int

data Scanner = S Layer Depth ScanState deriving (Show)

initState :: ScanState
initState = (0,True)

instance Read Scanner where
    readPrec = lift parseScanner

parseInt :: ReadP Int
parseInt = read @Int <$> many1 (satisfy isDigit)

parseScanner :: ReadP Scanner
parseScanner = do layer <- parseInt
                  char ':'
                  skipSpaces
                  depth <- parseInt
                  return $ S layer depth initState


updScan :: Int -> ScanState -> ScanState
updScan max (cp, True) | cp + 1 == max = (cp -1, False)
updScan max (cp, True)                 = (cp +1, True)
updScan _ (cp, False)  | cp <= 0       = (cp +1, True)
updScan _ (cp, False)                  = (cp -1, False)

updScanner :: Scanner -> Scanner
updScanner (S layer depth st) = S layer depth (updScan depth st)

test = [ "0: 3"
       , "1: 2"
       , "4: 4"
       , "6: 4"
       ]

scannerMap :: [Scanner] -> Map Int Scanner
scannerMap = M.fromList  . (map (\s@(S l _ _) -> (l, s)))

-- Part 1
totalSeverity :: Map Int Scanner -> Int
totalSeverity scanMap = go 0 0 scanMap
  where maxScanner = S.findMax $ M.keysSet scanMap
        go :: Int -> Int -> Map Int Scanner -> Int
        go cp csev _ | cp > maxScanner = csev
        go cp csev cmap = go ncp nsev nmap
          where ncp = cp + 1
                nmap = M.map updScanner cmap
                nsev = case (cp `M.lookup` cmap) of
                         Just (S l d (0,_)) -> csev + (l*d)
                         _ -> csev

getsCaught :: Map Int Scanner -> Bool
getsCaught scanMap = go 0 scanMap
  where maxScanner = S.findMax $ M.keysSet scanMap
        go :: Int -> Map Int Scanner -> Bool
        go cp _ | cp > maxScanner = False
        go cp cmap = if caught then True else  go ncp nmap
          where ncp = cp + 1
                nmap = M.map updScanner cmap
                caught = case (cp `M.lookup` cmap) of
                         Just (S l d (0,_)) -> True
                         _ -> False


minDelay :: Map Int Scanner -> Integer
minDelay scanMap = minDelay' 0 scanMap
  where minDelay' n mp | getsCaught mp = minDelay' (n+1) (M.map updScanner mp)
        minDelay' n _ = n

main :: IO ()
main = (totalSeverity &&& minDelay) . scannerMap . map (read @Scanner) <$> readInput >>= print
