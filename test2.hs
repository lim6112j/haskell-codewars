-- |
module Tribonacci where

import Control.Monad.Writer

-- snd $ runWriter $ tribonacci (1,1,1) 5
tribonacci :: (Num a) => (a, a, a) -> Int -> Writer [a] [a]
tribonacci (a, b, c) 0 = writer ([a, b, c], [])
tribonacci (a, b, c) 3 = writer ([a, b, c], [])
tribonacci (a, b, c) n = do
  let newVal = a + b + c
  tell [newVal]
  tribonacci (b, c, newVal) (n -1)

-- snd $ runWriter $ tribonacci' (1,1,1) 5
tribonacci' :: (Num a, Monad m) => (a, a, a) -> Int -> WriterT [a] m [a]
tribonacci' (a, b, c) 3 = WriterT $ return ([a, b, c], [])
tribonacci' (a, b, c) n = do
  let newVal = a + b + c
  tell [newVal]
  tribonacci' (b, c, newVal) (n -1)

wrapFunc (a, b, c) n
  | n == 0 = do
    tribonacci (a, b, c) n
  | n > 0 = do
    tell [a, b, c]
    tribonacci (a, b, c) n

wrapFunc' (a, b, c) n
  | n <= 0 = do
    WriterT $ return ([a, b, c], [])
  | n == 1 = do
    tell [a]
    WriterT $ return ([a, b, c], [])
  | n == 2 = do
    tell [b]
    WriterT $ return ([a, b, c], [])
  | n >= 3 = do
    tell [a, b, c]
    tribonacci' (a, b, c) n

tribonacci'' :: (Num a) => (a, a, a) -> Int -> [a]
tribonacci'' _ n | n < 1 = []
tribonacci'' (a, b, c) n = a : tribonacci'' (b, c, a + b + c) (n -1)
