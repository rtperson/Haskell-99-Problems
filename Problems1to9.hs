import Data.List

list1 = [1,2,3,4,5,6]

-- Problem 1: find the last element of a list
lastList :: [a] -> a
lastList [] = error "list cannot be empty"
lastList (x:[]) = x
lastList (x:xs) = lastList xs

-- Problem 2: Find the last but one element of a list. 
lastButOne :: [a] -> a
lastButOne [] = error "list cannot be empty"
lastButOne (x:[]) = error "list must be larger than one"
lastButOne [x,_] = x
lastButOne (_:xs) = lastButOne xs


-- Problem 3: Find the K'th element of a list. The first element in the list is number 1. 
elementAt :: [a] -> Int -> a
elementAt [] n = error "list cannot be empty"
elementAt s@(x:xs) n 
    | n > (length s) = error "n cannot be larger than list"
    | otherwise = if (length s) == n then x else elementAt xs n 

-- Problem 4: find the number of elements of a list
countList :: [a] -> Int
countList [] = 0
countList (x:xs) = 1 + countList xs

-- Problem 5: reverse a list (I assume, not using "reverse")
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ (x : [])

-- Problem 6: find out whether a list is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome xs = let xs2 = reverse xs
                       in (xs2 == xs)
                       
-- Problem 7: flatten a nested list structure
-- NOTE: This doesn't strictly solve the problem. May want to revisit later.
flatten :: [[a]] -> [a]
flatten = foldr (++) []

{-
    Problem 8: Eliminate consecutive duplicates of list elements. 
    If a list contains repeated elements they should be replaced with 
    a single copy of the element. The order of the elements should not be changed. 
    
    > compress ["a","a","a","a","b","c","c","a","a","d","e","e","e","e"]
        ["a","b","c","a","d","e"]

-}

-- excellent opportunity to play around with mapAccumL
compress :: (Eq x) => [x] -> [x]
compress xs = fst $ mapAccumL (\ys s -> if (length ys > 0) && (last ys == s) then (ys, s) else (ys++s:[], s)) [] xs

-- the simplest solution from the Wiki 
compress' :: Eq a => [a] -> [a]
compress' = map head . group

{-
    Problem 9:  Pack consecutive duplicates of list elements into sublists. 
    If a list contains repeated elements they should be placed in separate sublists.
    
    *Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
        ["aaaa","b","cc","aa","d","eeee"]
-}

pack :: (Eq a) => [a] -> [[a]]
pack = group