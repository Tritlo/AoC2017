-- {-# LANGUAGE TypeApplications #-}
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sort)


isValidPassphrase :: String -> Bool
isValidPassphrase p = length wp == length swp && noAngrams
    where wp = words p
          swp = Set.fromList wp
          isangr a b = sort a == sort b
          noAngrams = not $ hasAngrs $ Set.toList swp
          hasAngrs (x:xs) = if (any (isangr x) xs) then True else hasAngrs xs
          hasAngrs _ = False





readInput :: IO [String]
readInput = readInput' []
    where
        readInput' :: [String] -> IO [String]
        readInput' sf = do inp <- getLine
                           if inp == ""
                            then return $ reverse sf
                            else readInput' (inp:sf)


main :: IO ()
main = readInput >>= print . length . filter isValidPassphrase
