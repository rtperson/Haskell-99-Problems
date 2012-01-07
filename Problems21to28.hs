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
    n' <- randomRIO (0, len)
    return [(xs!!n')]

-- this version allows resampling. 
rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do 
  xs <- replicateM n (rnd_select_one xs)
  return (foldl (++) [] xs)      

-- Ideally, to prevent re-sampling, we would use partition from Data.List
-- like so: 
-- partition (`elem` "g") "abcdefghijklmn"
--  --> ("g", "abcdefhijklmn")

rnd_select_one' :: Eq a => [a] -> IO (a, [a])
rnd_select_one' xs = do
    let len = (length xs) - 1
    n' <- randomRIO (0, len)
    let el = (xs!!n')
    let fil = filter (/= el) xs
    return (el,fil)

rnd_select' n xs 
    | n > (length xs) = xs
        
    


    
main = do
    putStrLn list
    putStrLn $ insertAt 'M' list 4
    --rnd_select  3 "abcdefgh" >>= putStrLn
