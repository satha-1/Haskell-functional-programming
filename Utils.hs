{-|
Module      : Utils
Description : Pure utility functions for string manipulation and list operations
License     : MIT
Maintainer  : student

This module contains pure helper functions used throughout the application.
All functions are side-effect free and can be safely used in parallel computations.
-}

module Utils
    ( splitBy
    , trim
    , safeReadInt
    , avgInts
    , avgDoubles
    , transposeSafe
    , padRight
    , formatDouble
    ) where

import Data.Char (isSpace)

-- | Split a string by a delimiter character (recursive implementation)
-- 
-- >>> splitBy ',' "a,b,c"
-- ["a","b","c"]
-- 
-- >>> splitBy ',' "a,,c"
-- ["a","","c"]
splitBy :: Char -> String -> [String]
splitBy _ "" = [""]
splitBy delim str = go str []
  where
    go :: String -> String -> [String]
    go [] acc = [reverse acc]
    go (c:cs) acc
        | c == delim = reverse acc : go cs []
        | otherwise  = go cs (c : acc)

-- | Remove leading and trailing whitespace from a string
-- 
-- >>> trim "  hello  "
-- "hello"
-- 
-- >>> trim "\t\n test \n\t"
-- "test"
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
  where
    -- dropWhileEnd is not in Prelude for older GHC, so we define it
    dropWhileEnd :: (a -> Bool) -> [a] -> [a]
    dropWhileEnd p = reverse . dropWhile p . reverse

-- | Safely parse an integer, returning Nothing for invalid input
-- 
-- >>> safeReadInt "42"
-- Just 42
-- 
-- >>> safeReadInt "abc"
-- Nothing
-- 
-- >>> safeReadInt ""
-- Nothing
safeReadInt :: String -> Maybe Int
safeReadInt s = case reads (trim s) of
    [(n, "")] -> Just n
    _         -> Nothing

-- | Calculate the average of a list of integers
-- Returns 0.0 for empty list to avoid division by zero
-- 
-- >>> avgInts [80, 90, 100]
-- 90.0
-- 
-- >>> avgInts []
-- 0.0
avgInts :: [Int] -> Double
avgInts [] = 0.0
avgInts xs = fromIntegral (sum xs) / fromIntegral (length xs)

-- | Calculate the average of a list of doubles
-- Returns 0.0 for empty list
-- 
-- >>> avgDoubles [80.0, 90.0, 100.0]
-- 90.0
avgDoubles :: [Double] -> Double
avgDoubles [] = 0.0
avgDoubles xs = sum xs / fromIntegral (length xs)

-- | Safely transpose a list of lists (recursive implementation)
-- Handles jagged lists by using the minimum row length
-- 
-- >>> transposeSafe [[1,2,3],[4,5,6]]
-- [[1,4],[2,5],[3,6]]
-- 
-- >>> transposeSafe [[1,2],[3,4,5]]  -- jagged: uses min length
-- [[1,3],[2,4]]
-- 
-- >>> transposeSafe []
-- []
transposeSafe :: [[a]] -> [[a]]
transposeSafe [] = []
transposeSafe ([]:_) = []
transposeSafe rows
    | any null rows = []
    | otherwise     = map head rows : transposeSafe (map tail rows)

-- | Pad a string to the right with spaces to reach a minimum width
-- 
-- >>> padRight 10 "hello"
-- "hello     "
padRight :: Int -> String -> String
padRight n s
    | length s >= n = s
    | otherwise     = s ++ replicate (n - length s) ' '

-- | Format a double to 2 decimal places
-- 
-- >>> formatDouble 85.6789
-- "85.68"
formatDouble :: Double -> String
formatDouble x = show (fromIntegral (round (x * 100) :: Int) / 100 :: Double)
