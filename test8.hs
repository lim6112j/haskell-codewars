-- |
-- Complete the solution so that it splits the string into pairs of two characters. If the string contains an odd number of characters then it should replace the missing second character of the final pair with an underscore ('_').
--
-- Examples:
--
-- * 'abc' =>  ['ab', 'c_']
-- * 'abcdef' => ['ab', 'cd', 'ef']
module Codewars.Kata.SplitStrings where

solution :: String -> [String]
solution (x : y : xs) = [x, y] : solution xs
solution (x : null) = [[x, '_']]
solution [] = []
