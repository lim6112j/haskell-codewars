-- Examples:
--
-- "one" => 1
-- "twenty" => 20
-- "two hundred forty-six" => 246
-- "seven hundred eighty-three thousand nine hundred and nineteen" => 783919
-- Additional Notes:
--
-- The minimum number is "zero" (inclusively)
-- The maximum number, which must be supported is 1 million (inclusively)
-- The "and" in e.g. "one hundred and twenty-four" is optional, in some cases it's present and in others it's not
-- All tested numbers are valid, you don't need to validate them

-- | In this kata we want to convert a string into an integer. The strings simply represent the numbers in words.
module ParseInt where

--parseInt :: String -> Int
--parseInt :: String -> Int
--parseInt :: String -> Int
--parseInt :: String -> Int

import Data.List (delete)
import Data.Map hiding (delete, foldl, foldr, map)
import Debug.Trace (traceShow)
import Text.ParserCombinators.ReadP
import Prelude hiding (lookup)

parseInt s =
  let template =
        fromList
          [ ("one", 1),
            ("two", 2),
            ("three", 3),
            ("four", 4),
            ("five", 5),
            ("six", 6),
            ("seven", 7),
            ("eight", 8),
            ("nine", 9),
            ("ten", 10),
            ("eleven", 11),
            ("twenve", 12),
            ("thirteen", 13),
            ("fourteen", 14),
            ("fifteen", 15),
            ("sixteen", 16),
            ("seventeen", 17),
            ("eightteen", 18),
            ("nineteen", 19),
            ("twenty", 20),
            ("thirty", 30),
            ("forty", 40),
            ("fifty", 50),
            ("sixty", 60),
            ("seventy", 70),
            ("eighty", 80),
            ("ninety", 90),
            ("hundred", 100),
            ("thousand", 1000),
            ("million", 1000000),
            ("and", 0)
          ]
      listOfWords = words $ traceShow s customWords s
      wordsToVal = foldl (\acc x -> value x acc template) (0, 0) listOfWords
   in (fst wordsToVal) + (snd wordsToVal)
  where
    customWords = map repl
      where
        repl '-' = ' '
        repl c = c
    value x acc template =
      let maybeVal = lookup x template
          func Nothing = (0, 0)
          func (Just 1000000) = (1000000, 0)
          func (Just 1000)
            | snd acc == 0 = (1000 * (fst acc), 0)
            | otherwise = (1000 * (snd acc), 0)
          func (Just 100) = (fst acc, 100 * (snd acc))
          func (Just 0) = acc
          func (Just x) = (fst acc, snd acc + x)
       in traceShow maybeVal func maybeVal

parseInt' :: String -> Int
parseInt' = fst . head . Prelude.filter (Prelude.null . snd) . readP_to_S million
  where
    strToInt :: [Int] -> [String] -> ReadP Int
    strToInt is ts = skipSpaces *> (choice $ zipWith (<$) is $ map string ts)

    factor :: ReadP Int -> ReadP Int -> ReadP Int
    factor f s = (((+) .) . (*) <$> option 1 f <*> s <*> option 0 f') +++ f
      where
        f' = skipSpaces *> (optional $ string "and") *> f

    million, thousand, hundred, simpl, readOne, readTen, readTeen :: ReadP Int
    million = factor thousand $ strToInt [1000000] ["million"]
    thousand = factor hundred $ strToInt [1000] ["thousand"]
    hundred = factor simpl $ strToInt [100] ["hundred"]

    simpl = readTeen +++ readOne +++ readSum
      where
        readSum = (+) <$> readTen <*> option 0 (char '-' *> readOne)
    readOne = strToInt [1 ..] ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
    readTen = strToInt [0, 10 ..] ["zero", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
    readTeen = strToInt [11 ..] ["eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eightteen", "nineteen"]
