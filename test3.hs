-- | codewars train3
module Likes where

likes :: [String] -> String
likes arr@(x : y : z : xs)
  | arrLength == 0 = "No one likes this"
  | arrLength == 1 = x ++ " likes this"
  | arrLength == 2 = x ++ " and " ++ y ++ " like this"
  | arrLength == 3 = x ++ ", " ++ y ++ " and " ++ z ++ " like this"
  | otherwise =
    let remnant = arrLength -2
     in x ++ ", " ++ y ++ " and " ++ show remnant ++ " others like this"
  where
    arrLength = length arr
