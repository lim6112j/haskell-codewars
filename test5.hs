--For example, if we run 9119 through the function, 811181 will come out, because 92 is 81 and 12 is 1.
--
--Note: The function accepts an integer and returns an integer

-- | Welcome. In this kata, you are asked to square every digit of a number and concatenate them.
module SquareDigit where

import Data.Char

squareDigit :: Int -> Int
squareDigit n
  | n < 0 =
    (* (-1)) $ read $ foldr func "" values
  | n >= 0 = read $ foldr func "" values
  where
    values
      | n >= 0 = show $ toInteger n
      | otherwise = show $ toInteger (- n)
    func = \x acc -> numStr x ++ acc
    numStr str = show (v * v)
      where
        v = read [str] :: Int

squareDigit' :: Int -> Int
squareDigit' n
  | n < 0 = negate $ squareDigit (negate n)
  --  | otherwise = read (show n >>= show . (^ 2) . digitToInt)
  | otherwise = read (show n >>= (\x -> show $ (^ 2) $ digitToInt x))
