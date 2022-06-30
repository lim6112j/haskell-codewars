-- | You are given an array (which will have a length of at least 3,
--  but could be very large) containing integers. The array is either entirely comprised of odd integers or entirely
--  comprised of even integers except for a single integer N. Write a method that takes the array as an argument and returns this
--  "outlier" N.
module Kata (findOutlier) where

import Data.List

findOutlier :: [Int] -> Int
findOutlier xs =
  let o = [x | x <- xs, odd x]
      e = [x | x <- xs, even x]
      result
        | length o > length e = e
        | otherwise = o
   in head result

findOutlier' :: [Int] -> Int
findOutlier' xs =
  case partition even xs of
    ([x], _) -> x
    (_, [x]) -> x
    otherwise -> error "invalid input"
