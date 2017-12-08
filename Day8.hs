{-# LANGUAGE TypeApplications #-}
import Text.ParserCombinators.ReadP
import Data.Char (isDigit, isSeparator)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Text.Read hiding (get, choice, )
import Text.Printf
import Debug.Trace

number :: ReadP Int
number = do
    sign <- sign
    num <- many1 $ satisfy isDigit
    return $ sign $ (read num)

sign :: ReadP (Int -> Int)
sign = option (0+) (char '-'>> return (0-))

register :: ReadP String
register = manyTill get (satisfy isSeparator)

getRegOp :: ReadP (Int -> Int -> Int)
getRegOp = choice $ map (\(s,op) -> (string s >> return op)) [("inc", (+)), ("dec", (-))]

getCheckOp :: ReadP (Int -> Int -> Bool)
getCheckOp = choice $ map (\(s,op) -> (string s >> return op))
    [("<",(<)), ("<=",(<=)), ("==", (==)), (">=", (>=)),  (">", (>)), ("!=", (/=))]

data Instruction = Ins { modReg :: String
                       , modOp :: Int -> Int -> Int
                       , modOpName :: String
                       , modAm :: Int
                       , checkReg :: String
                       , checkOp :: Int -> Int -> Bool
                       , checkOpName :: String
                       , checkAm :: Int
                       }

instance Read Instruction where
    readPrec = lift parser

instance Show Instruction where
    show (Ins mr _ mon ma cr _ con ca) = printf "%s %s %d if %s %s %d" mr mon ma cr con ca

parser :: ReadP Instruction
parser= do modReg <- register
           skipSpaces
           (modOpName, modOp) <- gather getRegOp
           skipSpaces
           modAm <- number
           skipSpaces
           string "if"
           skipSpaces
           checkReg <- register
           skipSpaces
           (checkOpName, checkOp) <-gather getCheckOp
           skipSpaces
           checkAm <- number
           return $ Ins modReg modOp modOpName modAm checkReg checkOp checkOpName checkAm


exec :: (Int, Map String Int) -> Instruction -> (Int, Map String Int)
exec (maxSoFar, env) (Ins modReg modOp _ modAm checkReg checkOp _ checkAm) = (max newVal maxSoFar, M.insert modReg newVal env)
    where modVal = M.findWithDefault 0 modReg env
          checkVal = M.findWithDefault 0 checkReg env
          newVal = if checkVal `checkOp` checkAm then modVal `modOp` modAm else modVal

execProg :: [Instruction] -> (Int, Map String Int)
execProg = foldl exec (0, M.empty)

largestRemaining :: Map String Int -> Int
largestRemaining = M.foldr max 0

main :: IO ()
main = do (maxEver, resultingEnv) <- readInput >>= return . map (read @Instruction . traceShowId) >>= return . execProg
          print (maxEver, largestRemaining resultingEnv)

readInput :: IO [String]
readInput = readInput' []
    where
        readInput' sf = do inp <- getLine
                           if (inp == "") then
                            (return $ reverse sf)
                           else
                            (readInput' (inp:sf))

