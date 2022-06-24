module Main where

import Data.Char

toCamelCase :: String -> String
toCamelCase = foldr func acc
  where
    acc = []
    func '-' [] = []
    func x [] = [x]
    func '-' acc@(h:xs) = toUpper h:xs
    func '_' acc@(h:xs) = toUpper h:xs
    func x s@(h:xs) = x : s 
    -- func x []
      -- | x == '-' = []
      -- | otherwise = [x]
    -- func x acc@(h : xs)
      -- | x == '-' = toUpper h : xs
      -- | x == '_' = toUpper h : xs
      -- | otherwise = x : acc

--func = \x acc -> case acc of
--[] -> case x of
--'-' -> []
--_ -> [x]
--(h : xs) -> case x of
--'-' -> toUpper h : xs
--'_' -> toUpper h : xs
--_ -> x : acc

main :: IO ()
main = putStrLn $ toCamelCase "the-Stealth-Warrior"
