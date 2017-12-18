{-# LANGUAGE TypeApplications #-}

import Text.ParserCombinators.ReadP
import Debug.Trace
import Text.Printf

import Data.List

-- While loops
import Data.Monoid
import Control.Monad
while :: Monad m => (a -> Bool) -> m a -> m [a]
while cond act = act >>= rec
    where rec a | cond a = flip ((<$>) . (<>) . pure) (while cond act) a
          rec _  = pure mempty

readInput :: IO [String]
readInput = while (/= "") getLine


tests = [
    ("{}", 1),
    ("{{{}}}", 3),
    ("{{},{}}",3),
    ("{{{},{},{{}}}}", 6),
    ("{<{},{},{{}}>}", 1),
    ("{<a>,<a>,<a>,<a>}", 1),
    ("{{<a>},{<a>},{<a>},{<a>}}", 5),
    ("{{<!>},{<!>},{<!>},{<a>}}", 2) ]

tests2 = [ ("{}",1)
         , ("{{{}}}", 6)
         , ("{{},{}}", 5)
         , ("{{{},{},{{}}}}", 16)
         , ("{<a>,<a>,<a>,<a>}", 1)
         , ("{{<ab>},{<ab>},{<ab>},{<ab>}}", 9)
         , ("{{<!!>},{<!!>},{<!!>},{<!!>}}",9)
         , ("{{<a!>},{<a!>},{<a!>},{<ab>}}",3)
         ]

clean :: String -> String
clean str = removeIgnored $ concat $ map cleanMultiIgnores $ group str
    where cleanMultiIgnores ('!':r) = if (length r `mod` 2 == 0)  then "!" else ""
          cleanMultiIgnores xs = xs
          removeIgnored ('!':_:xs) = removeIgnored xs
          removeIgnored (x:xs) = (x:removeIgnored xs)
          removeIgnored [] = []



pG :: ReadP Int
pG = pG' 0
  where pG' par = do char '{'
                     nested <- sum <$> sepBy (choice [pG' (par+1), (string "<>" >> return 0)]) (char ',')
                     char '}'
                     return $ par + 1 + nested

stripGarbage :: String -> String
stripGarbage [] = []
stripGarbage str = gs ++ rest
    where (gs,sog) = span (/= '<') str
          (_, r)  = span (/= '>') sog
          dropRg ('>':r) = r
          dropRg r = r
          addIfNotEmpty [] = []
          addIfNotEmpty xs = "<>"++xs
          rest = addIfNotEmpty $ stripGarbage $ dropRg r


stripGBGCount :: String -> Int
stripGBGCount [] = 0
stripGBGCount str = length (dropLg gbg) + (stripGBGCount $ dropRg r)
    where (_,sog) = span (/= '<') str
          (gbg, r)  = span (/= '>') sog
          dropLg ('<':r) = r
          dropLg r = r
          dropRg ('>':r) = r
          dropRg r = r



main :: IO ()
main = do
    [inp] <- readInput
    traceM "running!"
    print (stripGarbage $ clean inp)
    [(res,"")] <- return $ readP_to_S pG $ stripGarbage $ clean inp
    print res
    print (stripGBGCount $ clean inp)
