{-# LANGUAGE Arrows #-}
import Control.Arrow
import Data.List
 
thing :: String
thing = "aaaabccaadeeee"
 
-- Problem 10: Run-length encoding
-- My attempt got me close enough to consider it solved.
encode :: (Eq a) => [a] -> [(Int, [a])]
encode xs = 
    let groupList = groupBy (\x y -> x == y) xs
        lengths = map length groupList
        letterList = map nub groupList
    in zip lengths letterList
    
-- from the haskell.org solutions:
encode2 :: (Eq a) => [a] -> [(Int, a)]
encode2 = map (\x -> (length x, head x)) . group

-- Problem 11, modified run-length encoding
-- if an element has no duplicates, mark it as such
-- Example: 
-- encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--    Multiple 2 'a',Single 'd',Multiple 4 'e']

data EncodedElement a = Multiple Int a | Single a
    deriving Show

encodeModified :: (Eq a) => [a] -> [EncodedElement a]    
encodeModified = map (\x -> if (length x > 1) 
                              then Multiple (length x) (head x) 
                              else Single (head x)) . group

{-
problem 12, Decode a run-length encoded list.

Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.

Example:

decodeModified 
   [Multiple 4 'a',Single 'b',Multiple 2 'c',
    Multiple 2 'a',Single 'd',Multiple 4 'e']

"aaaabccaadeeee"
-}
decodeModified :: [EncodedElement a] -> [a]
decodeModified = concatMap decode
    where 
        decode (Multiple n c) = replicate n c
        decode (Single c)     = c : []

{-
Problem 13:
(**) Run-length encoding of a list (direct solution).

Implement the so-called run-length encoding data compression method directly. 
I.e. don't explicitly create the sublists containing the duplicates, as in 
problem 9, but only count them. As in problem P11, simplify the result list 
by replacing the singleton lists (1 X) by X. 

P13> encodeDirect "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']
-}
-- TBD

{-
problem 14, Duplicate the elements of a list

dupli [1, 2, 3] 
[1,1,2,2,3,3]
-}
dupli :: [a] -> [a]
dupli = concatMap (replicate 2)

{-
problem 15, Replicate the elements of a list a given number of times.

repli "abc" 3 "aaabbbccc"
-}

repli :: Int -> [a] -> [a]
repli n = concatMap (replicate n)

{-
problem 16, Drop every N'th element from a list. 
*Main> dropEvery "abcdefghik" 3
    "abdeghk"
-}
dropEvery :: Int -> [a] -> [a]
dropEvery n [] = []
dropEvery n xs
    | length xs < n = xs
    | otherwise =
        let pre = (init . (take n)) xs
            post = drop n xs
        in pre ++ (dropEvery n post)

{-
Problem 17 -Split a list into two parts; the length of the first part is given.

Do not use any predefined predicates. 
-}
-- Introducing my convoluted implementation of splitAt
split :: [a] -> Int -> ([a], [a])
split xs n = (pre xs 0 n, post xs 0 (n-1))
    where pre :: [a] -> Int -> Int -> [a]
          pre [] _ _ = []
          pre xs dex n
                | dex > length xs = xs
                | n > length xs = xs
                | otherwise = if (dex < n) 
                                    then xs!!dex : pre xs (dex+1) n
                                    else [] 
          post :: [a] -> Int -> Int -> [a]
          post [] _ _ = []
          post (x:xs) dex n
                | n < 0 = (x:xs)
                | otherwise = if (dex < n)
                                then post xs (dex+1) n
                                else xs
                                
-- Or, to borrow from the solutions on Haskell.org 
-- (which is incorrectfor negative n, BTW)

split' :: [a] -> Int -> ([a], [a])
split' xs 0 = ([], xs)
split' (x:xs) n = let (f,l) = split xs (n-1) in (x : f, l)

{-
Problem 18 - Extract a slice from a list.

Given two indices, i and k, the slice is the list containing the elements 
between the i'th and k'th element of the original list (both limits included). 
Start counting the elements with 1. 

*Main> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
"cdefg"
-}
--(This one is so easy it feels like cheating...)
slice :: [a] -> Int -> Int -> [a]
slice xs i k = ((drop (i-1)) . reverse . (drop end) . reverse) xs
  where
    end = (length xs - k)
    

-- More efficient solution (from the solutions):

slice' xs i k | i>0 = take (k-i+1) $ drop (i-1) xs 
-- I forgot all about take when solving this...

{-
Problem 19 - Rotate a list N places to the left.

Hint: Use the predefined functions length and (++). 

*Main> rotate ['a','b','c','d','e','f','g','h'] 3
"defghabc"
 
*Main> rotate ['a','b','c','d','e','f','g','h'] (-2)
"ghabcdef"
-}
rotate xs n | n > 0 = (drop n xs) ++ (take n xs)
            | n <= 0 = (drop sizeRecip  xs) ++ (take sizeRecip xs)
               where sizeRecip = (length xs) + n

{-
Problem 20, Arrowed!
Remove the K'th element from a list

Trivial using a pure function. A bit more challenging if you use this 
problem to work up your Arrow-fu.
-}

list = "abcdefghijklmnop"

-- the trivial version
removeAt n xs = take (n-1) xs ++ (drop n) xs

-- now new and improved! with Arrows for ultra-obfuscation!
removeAt' :: (Arrow cat) => Int -> cat [a] [a]
removeAt' n = arr(\ xs -> (xs,xs)) >>> arr (take (n-1)) *** arr (drop n) 
                    >>> arr (uncurry (++)) >>> returnA

{-
   the arrowed function is easier to understand in its sugary form
   NOTE: the head and tail of the arrows are lambda values, so they
   can't be used inside the arrow's machinery. Also, to use this 
   notation, you'll need the LANGUAGE Arrows pragma
-}
removeAt'' :: Int -> [a] -> [a]
removeAt'' n = proc things -> do
    (begin, end) <- (\xs -> (xs, xs)) -< things
    begin' <- take (n-1) -< begin
    end' <- drop n -< end
    newlist <- uncurry (++) -< (begin', end')
    returnA -< newlist
    
-- This is equivalent, and shows how proc notation can
-- simplify things
removeAt''' :: Int -> [a] -> [a]
removeAt''' n = proc things -> do
    begin <- take (n-1) -< things
    end <- drop n -< things
    returnA -< begin ++ end

    
main = do
    putStrLn list
    putStrLn $ removeAt 3 list
    putStrLn $ removeAt' 5 list
    putStrLn $ removeAt'' 9 list