-- |
module Tribonacci where

import Control.Monad.Writer

-- tribonacci (1,1,1) 5
tribonacci :: (Num a) => (a, a, a) -> Int -> Writer [a] [a]
tribonacci (a, b, c) 0 = writer ([a, b, c], [])
tribonacci (a, b, c) n = do
  let newVal = a + b + c
  tell [newVal]
  tribonacci (b, c, newVal) (n -1)

-- runWriter $ tribonacci' (1,1,1) 5
tribonacci' :: (Num a, Monad m) => (a, a, a) -> Int -> WriterT [a] m [a]
tribonacci' (a, b, c) 0 = WriterT $ return ([a, b, c], [])
tribonacci' (a, b, c) n = do
  let newVal = a + b + c
  tell [newVal]
  tribonacci' (b, c, newVal) (n -1)
