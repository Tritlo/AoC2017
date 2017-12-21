import Debug.Trace
import Data.Bits
import AoCUtils
import Control.Arrow ((&&&))

genAFactor :: Integer
genAFactor = 16807

genBFactor :: Integer
genBFactor = 48271

genDivisor :: Integer
genDivisor = 2147483647

genNext :: Integer -> Integer -> Integer
genNext factor prev = (factor*prev) `rem` genDivisor

genA = genNext genAFactor
genB = genNext genBFactor

literalJudge :: Integer -> Integer -> Bool
literalJudge a b = getBits a == getBits b
    where getBits x = map (testBit x) [0..15]

fastJudge :: Integer -> Integer -> Bool
fastJudge a b = (a .&. 0xFFFF)  == (b .&. 0xFFFF)

-- Part 1
matchCount :: Int -> Integer -> Integer -> Int
matchCount limit initA initB =  length . filter id . take limit . map (uncurry fastJudge) $ zip aVals bVals
    where aVals = iterate genA initA
          bVals = iterate genB initB

-- Part 2
matches2 :: Int -> Integer -> Integer -> [(Integer, Integer)]
matches2 limit initA initB = filter (uncurry fastJudge) . take limit $ zip aVals bVals
    where -- Here we have to drop 1, since we don't compare the initial values.
          aVals = filter ((== 0) . (`mod` 4)) . drop 1 $ iterate genA initA
          bVals = filter ((== 0) . (`mod` 8)) . drop 1 $ iterate genB initB


test :: (Integer, Integer)
test = (65, 8921)
input = (516, 190)

testLimit = 5
limit1 = 40000000
limit2 = 5000000

main :: IO ()
main = (uncurry (matchCount limit1) &&& length . uncurry (matches2 limit2)) <$> return input >>= print
