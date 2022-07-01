{-# LANGUAGE DataKinds #-}

-- | 1, 246, 2, 123, 3, 82, 6, 41 are the divisors of number 246. Squaring these divisors we get: 1, 60516, 4,
--  15129, 9, 6724, 36, 1681. The sum of these squares is 84100 which is 290 * 290.
--
--  Task
--  Find all integers between m and n (m and n integers with 1 <= m <= n) such that the sum of their squared divisors
--  is itself a square.
--
--  We will return an array of subarrays or of tuples (in C an array of Pair) or a string.
--  The subarrays (or tuples or Pairs) will have two elements: first the number the squared divisors of which is a square and
-- then the sum of the squared divisors.
--
--  Example:
--  list_squared(1, 250) --> [[1, 1], [42, 2500], [246, 84100]]
--  list_squared(42, 250) --> [[42, 2500], [246, 84100]]
module Codewars.G964.Sumdivsq where

import Data.List (nub)

divisorsSquareSum :: Int -> Int
divisorsSquareSum m = sum $ map (^ 2) $ filter (\x -> (m `mod` x) == 0) [1 .. m]

isSquare :: Int -> Bool
isSquare n = fst $ binarySearchPerfectSquare n 1 n

binarySearchPerfectSquare :: Int -> Int -> Int -> (Bool, Int)
binarySearchPerfectSquare n low high
  | low > high = (False, 0)
  | n == midSquare = (True, midSquare)
  | n < midSquare = binarySearchPerfectSquare n low (mid - 1)
  | otherwise = binarySearchPerfectSquare n (mid + 1) high
  where
    mid = (high + low) `div` 2
    midSquare = mid * mid

listSquared :: Int -> Int -> [(Int, Int)]
listSquared m n = concatMap func [m .. n]
  where
    func x
      | fst result = [(x, snd result)]
      | otherwise = []
      where
        squareSum = divisorsSquareSum x
        result = binarySearchPerfectSquare squareSum 1 squareSum

-- best practice
divisors :: Int -> [Int]
divisors n = divs >>= keepDivisors
  where
    divs = takeWhile (\d -> d * d <= n) [1 ..]
    keepDivisors d = if n `mod` d == 0 then nub [d, n `div` d] else []

listSquared' :: Int -> Int -> [(Int, Int)]
listSquared' m n = filter (isSquare . snd) $ map (\x -> (x, sum . map (^ 2) $ divisors x)) [m .. n]
  where
    isSquare x = (round . sqrt $ fromIntegral x) ^ 2 == x
