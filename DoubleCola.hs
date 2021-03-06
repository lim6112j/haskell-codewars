{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- Sheldon, Leonard, Penny, Rajesh and Howard are in the queue for a "Double Cola"
-- drink vending machine; there are no other people in the queue. The first one in the queue (Sheldon) buys a can,
-- drinks it and doubles! The resulting two Sheldons go to the end of the queue.
-- Then the next in the queue (Leonard) buys a can, drinks it and gets to the end of the queue as two Leonards,
-- and so on.
--
-- For example, Penny drinks the third can of cola and the queue will look like this:
--
-- Rajesh, Howard, Sheldon, Sheldon, Leonard, Leonard, Penny, Penny
--
-- Write a program that will return the name of the person who will drink the n-th cola
module DoubleCola where

import Data.List (group)

data Group = Group {size :: Int, name :: String} deriving (Show)

names :: [String]
names = ["Sheldon", "Leonard", "Penny", "Rajesh", "Howard"]

groups :: [Group]
groups = map (Group 1) names ++ map doubleGroup groups

doubleGroup :: Group -> Group
doubleGroup x = Group (size x * 2) (name x)

nth :: Int -> [Group] -> String
nth n (Group size name : rest)
  | n <= size = name
  | otherwise = nth (n - size) rest

doubleCola :: [String] -> Int -> String
doubleCola s n =
  let groups = map (Group 1) s ++ map doubleGroup groups
   in nth n groups
