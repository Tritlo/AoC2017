import Text.Printf
import System.IO (isEOF)
import Data.Maybe


type Input = [[Int]]
type Output = Int

test :: (Input, Output)
test = ([[5,1,9,5], [7,5,3], [2,4,6,8]], 18)

tests :: [(Input, Output)]
tests = [test]


passesTests :: (Input -> Output) -> [(Input, Output)] -> [Maybe String]
passesTests f tests = map (passesTest f) tests

passesTest :: (Input -> Output) -> (Input, Output) -> Maybe String
passesTest f (inp, expected) = if res == expected then Nothing else Just $ printf "Expected %d, got %d" expected res
    where res = f inp


solution :: Input -> Output
solution rows = sum (map sol rows)
    where sol :: [Int] -> Int
          sol row = maximum row - minimum row

solution2 :: Input -> Output
solution2 rows = sum (map sol rows)

sol row = if (null divisors) then 1 else (head $ head divisors)
  where dividesAny x = mapMaybe (\y -> if  not (x == y) &&  x `mod` y == 0 then Just (x `div` y) else Nothing) row
        divisors = filter (not . null) $ map dividesAny row


readInput :: IO Input
readInput = readInput' []
    where readInput' sf = do inp <- getLine
                             if inp == ""
                               then return $ reverse sf
                               else readInput' $ (map read $ words inp):sf

main :: IO ()
main = readInput >>= return . solution2 >>= print
