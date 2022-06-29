{-# LANGUAGE FlexibleContexts #-}

-- |
--
-- Given an array of integers, find the one that appears an odd number of times.
--
-- There will always be only one integer that appears an odd number of times.
--
-- Examples
-- [7] should return 7, because it occurs 1 time (which is odd).
-- [0] should return 0, because it occurs 1 time (which is odd).
-- [1,1,2] should return 2, because it occurs 1 time (which is odd).
-- [0,1,0,1,0] should return 0, because it occurs 3 times (which is odd).
-- [1,2,2,3,3,3,4,3,3,3,2,2,1] should return 4, because it appears 1 time (which is odd).
-- | Given a list, find the [Int] that appears an
--   odd number of times. The tests will always
--   provide such a number, and the list will
--   always contain at least one element.
module Codewars.Kata.FindOdd where

--findOdd :: [Int] -> Int

import Data.List
import qualified Data.Map as Map
import Debug.Trace (traceShow)

findOdd :: [Int] -> Int
findOdd = foldr (func . mapping) 0 . group . sort
  where
    mapping = (\l@(x : _) -> (x, length l))
    func (k, v) acc
      | odd v = k
      | otherwise = acc

findOdd' :: [Int] -> Int
findOdd' xs = head [x | x <- xs, odd . length . filter (== x) $ xs]
