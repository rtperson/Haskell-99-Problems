{-# LANGUAGE DoRec #-}

import Data.List
import System.Random
import Control.Monad
import Control.Monad.Trans

{-
    problem 21, Insert an element at a given position into a list.
    Example:
    P21> insertAt 'X' "abcd" 2
        "aXbcd"
-}

list = "abcdefghijklmnop"

insertAt :: a -> [a] -> Int -> [a]
insertAt c xs n = (take (n-1) xs) ++ ( c : []) ++ (drop (n-1) xs)

{-
    problem 22, Create a list containing all integers within a given range. 
    Example:
    Prelude> range 4 9
        [4,5,6,7,8,9]
-}
-- this one is so trivial in Haskell I'm not sure why I'm bothering.
range n k | n <= k = [n..k]
          | k < n = reverse [k..n]
          
{-
    problem 23, Extract a given number of randomly selected elements from a list. 
    Example:
    Prelude System.Random>rnd_select "abcdefgh" 3 >>= putStrLn
        "eda"
        
    Two problems: 1) How to return a list, and 2) how to sample without duplication
-}
-- ah, randomization. Always fun in Haskell.
-- this selects only one letter. Not what we want...
rnd_select_one :: [a] -> IO [a]
rnd_select_one xs = do
    let len = (length xs) - 1
    n <- randomRIO (0, len)
    return [(xs!!n)]

-- this version allows resampling. 
rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
    g <- sequence (replicate n (rnd_select_one xs))
    return (concat g)

-- Ideally, to prevent re-sampling, we would use partition from Data.List
-- like so: 
-- partition (`elem` "g") "abcdefghijklmn"
--  --> ("g", "abcdefhijklmn")

-- we have a recursive relation now in the following code, 
-- in that the following works:
-- (acc, list')    <- rnd_select_one ([], list)
-- (acc', list'')  <- rnd_select_one (acc, list')
-- (acc'', list''') <- rnd_select_one (acc', list'')
--
-- This will select without resampling, so obviously the 
-- only problem remaining is how to recursively call 
-- rnd_select_one within a monad.

rnd_select_one' :: Eq a => ([a], [a]) -> Int -> IO ([a], [a])
rnd_select_one' (acc, xs) 0 = return (acc, xs)
rnd_select_one' (acc, []) _ = return (acc, [])
rnd_select_one' (acc, xs) n = do
    let len = (length xs) - 1
    d <- randomRIO (0, len)
    let el = (xs!!d)
    let (g, ys) = partition (`elem` [el]) xs
    let ret = (acc ++ g, ys)
    rnd_select_one' ret (n-1) 

rnd_select' :: Eq a => [a] -> Int -> IO [a]
rnd_select' xs n = do
    j <- rnd_select_one' ([], xs) n
    return $ fst j

    
main = do
    putStrLn list
    putStrLn $ insertAt 'M' list 4
    rnd_select' "abcdefgh" 3 >>= putStrLn
