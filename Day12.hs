{-# LANGUAGE TypeApplications #-}
module Day12 where

import Text.ParserCombinators.ReadP
import Text.Read hiding (get, choice, )

import Text.Printf


import Control.Monad
import Control.Arrow

import Data.Char
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Debug.Trace

import Data.Maybe (fromJust)
import Control.Arrow

data Program = P Int (Set Int)

instance Read Program where
    readPrec = lift parseProgram

instance Show Program where
    show (P pid pipes) = printf "%d <-> %s" pid $ (init . tail . show . S.toList) pipes


parseInt :: ReadP Int
parseInt = read @Int <$> many1 (satisfy isDigit)

parseProgram :: ReadP Program
parseProgram =
    do pid <- parseInt
       skipSpaces
       string "<->"
       skipSpaces
       pipes <- sepBy parseInt (char ',' >> skipSpaces)
       return $ P pid $ S.fromList pipes


pipeMap :: [Program] -> Map Int (Set Int)
pipeMap = foldl f M.empty
  where f :: Map Int (Set Int) -> Program -> Map Int (Set Int)
        f m (P pid s) = M.insert pid s m

updSet :: Set Int -> Map Int (Set Int) -> Set Int
updSet curr m = if (S.size ns) == (S.size curr) then curr else updSet ns m
  where ns = foldl S.union curr $ S.map (fromJust . (`M.lookup` m)) curr

component :: Int -> Map Int (Set Int) -> Set Int
component pid pipeM = updSet (S.singleton pid) pipeM

tests = [ "0 <-> 2"
        , "1 <-> 1"
        , "2 <-> 0, 3, 4"
        , "3 <-> 2, 4"
        , "4 <-> 2, 3, 6"
        , "5 <-> 6"
        , "6 <-> 4, 5"
        ]

--- Part 2

countComponents :: Map Int (Set Int) -> Int
countComponents pipeM = go allPos 0
  where allPos = S.fromList $ M.keys pipeM
        go uns n | S.null uns = n
        go unchecked n = go (unchecked S.\\ group) (n+1)
          where anyEl = S.elemAt 0 unchecked
                group = component anyEl pipeM

