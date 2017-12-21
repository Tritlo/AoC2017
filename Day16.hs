{-# LANGUAGE TypeApplications #-}

import GHC.Generics
import Control.DeepSeq
import Text.ParserCombinators.ReadP

import Data.List
import Data.Maybe
import Data.Array.Unboxed
import Data.Array.MArray
import Data.Array.ST
import Control.Monad.ST

import AoCUtils
import Text.Read hiding (get, choice, )
import Debug.Trace
import Text.Printf
import Data.Int

import Control.Arrow

type Container = UArray Int Int
type Dancer = Int

toContainer :: String -> Container
toContainer str = listArray (0, length str -1) $ map charToId str

finish :: Container -> String
finish arr = map (chars !!) $ elems arr

spin :: Int -> Container -> Container
spin x arr = listArray b (tl ++ hd)
  where b@(_, e) = bounds arr
        (hd, tl) = (splitAt (e +1 - x ) . elems) arr

partner :: Dancer -> Dancer -> Container -> Container
partner a b arr = exchange ia ib arr
  where Just ia = elemIndex a els
        Just ib = elemIndex b els
        els = elems arr

exchange :: Int -> Int -> Container -> Container
exchange ia ib frozen = runSTUArray $ do arr <- thaw frozen
                                         writeArray arr ia (frozen ! ib)
                                         writeArray arr ib (frozen ! ia)
                                         return arr
parseSpin :: ReadP DanceMove
parseSpin = uncurry D <$> gather (spin <$> (char 's' >> parseInt))

parseExchange :: ReadP DanceMove
parseExchange =  uncurry D <$> gather (exchange <$> (char 'x' >> parseInt) <*> (char '/' >> parseInt))

parseDancer :: ReadP Dancer
parseDancer = charToId <$> get

parsePartner :: ReadP DanceMove
parsePartner = uncurry D <$> gather (partner <$> (char 'p' >> parseDancer) <*> (char '/' >> parseDancer))

parseDanceMove :: ReadP DanceMove
parseDanceMove = choice [parseSpin, parseExchange, parsePartner]

data DanceMove = D String (Container -> Container)

instance Read DanceMove where
    readPrec = lift parseDanceMove

instance Show DanceMove where
    show (D s _) = s

chars :: String
chars = "abcdefghijklmnop"

charToId :: Char -> Int
charToId = fromIntegral <$> fromJust . flip elemIndex chars

initDancers = toContainer chars
initTestState = toContainer "abcde"

test =  ["s1","x3/4", "pe/b"]

exec :: Container -> [DanceMove] -> Container
exec dancer (D s m:mvs) =
  -- trace (printf "%s: %s -> %s" s (finish dancer) (finish res))$
  exec res mvs
  where res = id $! m dancer
exec dancer [] = dancer

execShow :: [DanceMove] -> String
execShow moves = finish $ iter' m (showTime `mod` loopsAt) initDancers
    where m = flip exec moves
          loopsAt = findLoop' 1 $ m initDancers
          findLoop' n s | elems s == elems initDancers = n
          findLoop' n s = findLoop' (n+1) (m s)


showTime :: Int
showTime = 1000000000

main :: IO ()
main =  execShow . read @[DanceMove] . printf "[%s]" . head <$> readInput >>= print
