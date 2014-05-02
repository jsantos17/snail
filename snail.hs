import Data.List.Split
import System.Environment

genMatrix :: Int -> [[Int]]
genMatrix s = chunksOf s [1..s*s] 

tail' :: [a] -> [a]
tail' [] = []
tail' l = tail l

removeLast :: [a] -> [a]
removeLast m = reverse $ tail' $ reverse m

removeLastAndFirst :: [a] -> [a]
removeLastAndFirst m = tail' $ removeLast m 

inner :: [[a]] -> [[a]]
inner m = removeLastAndFirst $ map removeLastAndFirst m

right :: [[a]] -> [a]
right m = head m

down :: [[a]] -> [a]
down m = tail $ map last m

left :: [[a]] -> [a]
left m = tail $ reverse $ last m

up :: [[a]] -> [a]
up m = take (length m - 2) $ tail $ reverse $ map head m

outer :: [[a]] -> [a]
outer m = right m ++ down m ++ left m ++ up m

snail :: [[a]] -> [a]
snail [] = []
snail m = outer m ++ (snail $ inner m)

main = do
    putStrLn "Snail: "
    args <- getArgs
    print $ snail $ genMatrix $ read $ head args
