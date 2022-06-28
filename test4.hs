-- |
module DigitalRoot where

digitalRoot :: Integral a => a -> a
digitalRoot n
  | n >= 10 =
    let sum = foldr func 0 value
        value = show (toInteger n)
        func = (\x acc -> acc + read [x] :: Int)
     in digitalRoot $ fromInteger (toInteger sum)
  | otherwise = n
