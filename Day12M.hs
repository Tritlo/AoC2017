module Main where
import Day12
import qualified Data.Set as S
import AoCUtils (readInput)

import Control.Arrow

main :: IO ()
main = (S.size . component 0 &&& countComponents) . pipeMap . map read <$> readInput >>= print
-- main = (S.size . component 0 &&& countComponents) . pipeMap . map read <$> return tests >>= print
