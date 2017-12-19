import Day10


import Text.Printf

import AoCUtils (readInput, validate)


part1 :: String -> Int
part1 = (exec1 . read . printf "[%s]")
part2 :: String -> String
part2 = knotHash

main :: IO ()
-- main = part2 . head <$> readInput >>= print
main = print $ validate knotHash tests

