--Complete the function scramble(str1, str2) that returns true if a portion of str1 characters can be rearranged to match str2,
-- otherwise returns false.
--
--Notes:
--
--Only lower case letters will be used (a-z). No punctuation or digits will be included.
--Performance needs to be considered.
--Examples
--scramble('rkqodlw', 'world') ==> True
--scramble('cedewaraaossoqqyt', 'codewars') ==> True
--scramble('katas', 'steak') ==> False

-- |
module Codewars.G964.Scramblies where

import Data.Function (on)
import Data.List (delete, group, isSubsequenceOf, lookup, sort, (\\))
import Debug.Trace (trace, traceShow)

scramble :: [Char] -> [Char] -> Bool
scramble s1 s2
  | s2 == [] = True
  | s1 == [] = False
  | head s2 `elem` s1 =
    let s' = delete (head s2) s1
     in scramble s' (tail s2)
  | otherwise = False

scramble' :: [Char] -> [Char] -> Bool
scramble' s1 s2 =
  let s2group = foldr mapping [] $ group $ sort s2
      s1group = foldr mapping [] $ group $ sort s1
      mapping = \l@(x : xs) acc -> (x, length l) : acc
      func ss1 ss2
        | ss2 == [] = True
        | ss1 == [] = False
        | boolval == True = func ss1 (tail ss2)
        | boolval == False = False
        where
          s1num = lookup (fst $ head ss2) ss1
          s2num = Just (snd $ head ss2)
          boolval = s1num >= s2num
   in func s1group s2group

scramble'' :: [Char] -> [Char] -> Bool
scramble'' = flip (isSubsequenceOf `on` sort)

scramble''' :: [Char] -> [Char] -> Bool
scramble''' a b = (sort b) \\ (sort a) == ""
