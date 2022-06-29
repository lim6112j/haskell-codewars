{-# LANGUAGE FlexibleContexts #-}

-- -#Find the missing letter
--
-- Write a method that takes an array of consecutive (increasing) letters as input and that returns the missing letter in the array.
--
-- You will always get an valid array. And it will be always exactly one letter be missing. The length of the array will always be at least 2.
-- The array will always contain letters in only one case.
--
-- Example:
--
-- ['a','b','c','d','f'] -> 'e'
-- ['O','Q','R','S'] -> 'P'
module Kata where

import Control.Monad (join)
import Data.Char

findMissingLetter :: [Char] -> Char
findMissingLetter cs = head $ filter (`notElem` cs) $ take (length cs) [(head cs) ..]

findMissingLetter' :: [Char] -> Char
findMissingLetter' (x : xs)
  | head xs == next = findMissingLetter xs
  | otherwise = next
  where
    next = succ x
