module Main where

import Data.List (partition)

xs = [1,2,3,4] :: [Int]

part :: Eq a => a -> [a] -> ([a], [a])
part n = partition (== n)

runPart [] _ = []
runPart xs n = uncurry (++) (part (xs!!n) xs) 

firstPerm :: [Int] -> [[Int]]
firstPerm xs = map (\x -> (runPart xs (x-1))) xs

