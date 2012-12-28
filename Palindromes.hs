{- 
Credit where it's due: this is my "for-study" copy of Johan Jeuring's algorithm
found here: http://johanjeuring.blogspot.com/2007/08/finding-palindromes.html
-}

module Palindromes where

import Data.Array

longestPalindromesQ :: Eq a => Array Int a -> [Int]
longestPalindromesQ a = 
	let (afirst, alast) = bounds a
	    positions       = [0 .. 2*(alast-afirst+1)]
	in map (lengthPalindromeAround a) positions

lengthPalindromeAround :: Eq a => Array Int a -> Int -> Int
lengthPalindromeAround a position 
	| even position = 
		extendPalindromeAround (afirst+pos-1) (afirst+pos)
	| odd position = 
		extendPalindromeAround (afirst+pos-1) (afirst + pos + 1)
	| otherwise = 0

	where pos 			  = div position 2
	      (afirst, alast) = bounds a
	      extendPalindromeAround start end = 
	      		if start < 0
	      			|| end > alast-afirst
	      			|| a!start /= a!end
	      			then end-start-1
	      			else extendPalindromeAround (start-1) (end+1)

-- that was the naive quadratic approach. Here's where it gets real.

longestPalindromes :: Eq a => Array Int a -> [Int]
longestPalindromes a = 
	let (afirst, _) = bounds a
	in  extendTail a afirst 0 []

extendTail :: (Eq a, Num a1, Ix a1) =>
                           Array a1 a -> a1 -> a1 -> [a1] -> [a1]
extendTail a n currentTail centers
	| n > alast = 
		-- reached the end of the array
		finalCenters currentTail centers (currentTail : centers)
	| n-currentTail == afirst = 
		-- the current longest tail palindrome extends to the start of the array
		extendCenters a n (currentTail : centers) centers currentTail
	| a!n == a!(n-currentTail-1) = 
		-- the current longest tail palindrome can be extended
		extendTail a (n+1) (currentTail+2) centers
	| otherwise =
		-- the current longest tail palindrome cannot be extended
		extendCenters a n (currentTail : centers) centers currentTail
  where
  	(afirst, alast) = bounds a

extendCenters :: (Eq a, Num a1, Ix a1) =>
                              Array a1 a -> a1 -> [a1] -> [a1] -> a1 -> [a1]
extendCenters a n centers tcenters centerDistance
	| centerDistance == 0 =
		-- the last center is on the last element:
		-- try to extend the tail of length 1
		extendTail a (n+1) 1 centers
	| centerDistance - 1 == head tcenters = 
		-- the previous element in the center list
		-- reaches exactly to the end of the last tail palindrome.
		-- Use the mirror property of palindromes to find the longest tail 
		-- palindrome.
		extendTail a n (head tcenters) centers
	| otherwise = 
		-- move the centers one step.
		-- add the length of the longest palindrome to the centers
		extendCenters a n (min (head tcenters) (centerDistance-1):centers) 
					  (tail tcenters) (centerDistance-1)

finalCenters ::  (Num a, Ord a) => a -> [a] -> [a] -> [a]
finalCenters 0 _ centers = centers
finalCenters n tcenters centers = 
	finalCenters (n-1) (tail tcenters) (min (head tcenters) (n-1):centers)