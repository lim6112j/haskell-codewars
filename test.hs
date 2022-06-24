{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Main where

import Data.Char

toCamelCase :: String -> String
toCamelCase = foldr func acc
  where
    acc = []
    func = \x acc -> case acc of
      [] -> case x of
        '-' -> []
        _ -> [x]
      (h : xs) -> case x of
        '-' -> toUpper h : xs
        '_' -> toUpper h : xs
        _ -> x : acc

main :: IO ()
main = putStrLn $ toCamelCase "the-Stealth-Warrior"
