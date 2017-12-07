import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as S
import Text.Printf
import Data.List as L

data ReadNode = RN { name :: String
                 , weight :: Int
                 , children :: [String]} deriving (Show, Eq)


data Node = N { nname::String, nweight::Int, nparent :: Maybe Node}

nodes :: Map String String
nodes = M.empty


wTI :: String -> Int
wTI = read . tail . init

toNode :: String -> ReadNode
toNode s = toNode' (words s)
  where toNode' (n:w:[]) = RN n (wTI w) []
        toNode'  (n:w:arrow:ns) = RN n (wTI w) (map (dropWhile (== ' ')) $ splitOn ','  $ unwords ns)
        toNode' _ = undefined


splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c xs = splitOn' c xs [] []
  where splitOn' :: Eq a => a -> [a] -> [[a]] -> [a] -> [[a]]
        splitOn' _ [] sf csf = reverse ((reverse csf):sf)
        splitOn' c (x:xs) sf csf | c == x = splitOn' c xs ((reverse csf):sf) []
        splitOn' c (x:xs) sf csf = splitOn' c xs sf (x:csf)

insertNames :: Map String String -> Map String Int -> [ReadNode] -> (Map String String, Map String Int)
insertNames nm wm [] = (nm, wm)
insertNames nm wm ((RN n w _):ns) = insertNames (M.insert n "" nm) (M.insert n w wm) ns

-- insertChildren :: Map String [String] -> [ReadNode] -> Map String [String]
insertChildren m [] = m
insertChildren m ((RN _ _ []):ns) = insertChildren m ns
insertChildren m ((RN n w (c:cs)):ns) = insertChildren (M.insert c n m) ((RN n w cs):ns)


findValue :: Eq a => a -> Map k a -> [k]
findValue a =  map fst . filter (((==) a ) . snd) . M.toList


data Tree = Tree { tn :: String, tw :: Int, tc :: [Tree] } deriving (Show)

kidMap :: [ReadNode] -> Map String [String]
kidMap = M.fromList . map (\(RN n _ cs) -> (n,cs))

mkFromRoot :: Map String [String] -> Map String Int -> String -> Tree
mkFromRoot kmap wmap n = Tree n (fromJust $ n `M.lookup` wmap)  $ map (mkFromRoot kmap wmap) $ fromJust $ n `M.lookup` kmap


weightOf :: Tree -> Int
weightOf (Tree _ w []) = w
weightOf (Tree _ w cs) = w + sum (map weightOf cs)

isImbalanced :: Tree -> Bool
isImbalanced (Tree _ _ []) = False
isImbalanced (Tree _ _ cs) = not $ (S.size (S.fromList $ map weightOf cs)) == 1

-- We do a lot of extra summing here, but this performs so well anyway that it
-- doesn't matter
findImbalance :: Tree -> String
findImbalance t@(Tree n w cs) | isImbalanced t && (not $ any isImbalanced cs)  = explanation
    where cws = map weightOf cs
          oddone = head $ fromJust $ L.find ((== 1) . length) $ group cws
          ind = fromJust $ L.findIndex ((==) oddone) cws
          oind = fromJust $ L.findIndex ( not . (== oddone)) cws
          (Tree on ow _) = cs !! ind
          othersw = cws !! oind
          oddonesw = cws !! ind
          explanation = printf "%s's stack weight is %d, others are %d. So weight should be %d" on oddonesw othersw $ ow + (othersw-oddonesw)


findImbalance (Tree _ _ cs) = findImbalance $ head (filter isImbalanced cs)

readInput :: IO [ReadNode]
readInput = readInput' []
    where
        readInput' sf = do inp <- getLine
                           if (inp == "") then do {
                             return $ map toNode $ reverse sf
                           } else do {
                             readInput' (inp:sf)
                           }

main :: IO ()
main = do ns <- readInput
          print ns
          let (nm, wm) = insertNames M.empty M.empty ns
          let m = insertChildren nm ns
          let kmap = kidMap ns
          let root = head $ findValue "" m
          let tree@(Tree n w tcs) = mkFromRoot kmap wm root
          putStrLn $ printf "Root is %s, and the imbalacing expalanation is that %s" (show root) (findImbalance tree)
