-- | Some numbers have funny properties. For example:
--
--  89 --> 8¹ + 9² = 89 * 1
--
--  695 --> 6² + 9³ + 5⁴= 1390 = 695 * 2
--
--  46288 --> 4³ + 6⁴+ 2⁵ + 8⁶ + 8⁷ = 2360688 = 46288 * 51
--
--  Given a positive integer n written as abcd... (a, b, c, d... being digits) and a positive integer p
--
--  we want to find a positive integer k, if it exists, such that the sum of the digits of n taken to the successive powers of p is equal to k * n.
--  In other words:
--
--  Is there an integer k such as : (a ^ p + b ^ (p+1) + c ^(p+2) + d ^ (p+3) + ...) = n * k
--
--  If it is the case we will return k, if not return -1.
--
--  Note: n and p will always be given as strictly positive integers.
module Digpow where

import Control.Monad.Identity
import Control.Monad.Writer
import Data.Char (digitToInt)
import Debug.Trace

digpow :: Integer -> Integer -> Integer
digpow n p
  | n > 1 =
    let sum = 0
        result = traceShow n traceShow p $ digpow' sum n p
        remnant = result `divMod` n
     in case remnant of
          (x, 0) -> x
          (_, _) -> traceShow result (-1)
  | otherwise = 1

digpow' :: Integer -> Integer -> Integer -> Integer
digpow' sum n p
  | n < 0 = error "only positive value needed"
  | n < 10 =
    let headval = toInteger $ digitToInt (head (show n)) ^ p
     in sum + headval
  | otherwise =
    let headval = toInteger $ digitToInt (head (show n)) ^ p
        tailval = read $ tail (show n)
        curLen = toInteger (length $ show n)
        tailLen = toInteger (length $ show tailval)
        nextP = p + curLen - tailLen
     in digpow' (sum + headval) tailval nextP

digpow''' :: Integer -> Integer -> Integer
digpow''' n p
  | sp `mod` n == 0 = sp `div` n
  | otherwise = -1
  where
    sp = fromIntegral $ sum $ zipWith (^) (map digitToInt $ show n) [p ..]

--
-- 1004 3     len 4, p 3
-- 1         len 4, p 3
-- 0          len 3, p 4
-- 0          len 2 , p 5     3 + (4 -2)
-- 4           len 1 , p 6    3 + (4 - 1)
