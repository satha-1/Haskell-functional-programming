{-|
Module      : Utils
Description : Pure utility functions for string manipulation and math
License     : MIT
Maintainer  : student

This module contains pure helper functions used throughout the application.
Includes statistical helpers for correlation calculation.
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
    , mean
    , covariance
    , stdDev
    ) where

import Data.Char (isSpace)

-- | Split a string by a delimiter character
splitBy :: Char -> String -> [String]
splitBy _ "" = [""]
splitBy delim str = go str []
  where
    go :: String -> String -> [String]
    go [] acc = [reverse acc]
    go (c:cs) acc
        | c == delim = reverse acc : go cs []
        | otherwise  = go cs (c : acc)

-- | Remove leading and trailing whitespace
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
  where
    dropWhileEnd :: (a -> Bool) -> [a] -> [a]
    dropWhileEnd p = reverse . dropWhile p . reverse

-- | Safely parse an integer
safeReadInt :: String -> Maybe Int
safeReadInt s = case reads (trim s) of
    [(n, "")] -> Just n
    _         -> Nothing

-- | Calculate the average of a list of integers
avgInts :: [Int] -> Double
avgInts [] = 0.0
avgInts xs = fromIntegral (sum xs) / fromIntegral (length xs)

-- | Calculate the average of a list of doubles
avgDoubles :: [Double] -> Double
avgDoubles [] = 0.0
avgDoubles xs = sum xs / fromIntegral (length xs)

-- | Alias for avgDoubles for clarity in statistical contexts
mean :: [Double] -> Double
mean = avgDoubles

-- | Calculate covariance between two lists of doubles
-- Returns 0.0 if lists lengths differ or are empty
covariance :: [Double] -> [Double] -> Double
covariance xs ys
    | length xs /= length ys || null xs = 0.0
    | otherwise =
        let meanX = mean xs
            meanY = mean ys
            n = fromIntegral (length xs)
            sumProdDiffs = sum $ zipWith (\x y -> (x - meanX) * (y - meanY)) xs ys
        in sumProdDiffs / n

-- | Calculate standard deviation of a sample
stdDev :: [Double] -> Double
stdDev xs
    | null xs = 0.0
    | otherwise =
        let m = mean xs
            sumSqDiffs = sum $ map (\x -> (x - m) ^ 2) xs
        in sqrt (sumSqDiffs / fromIntegral (length xs))

-- | Safely transpose a list of lists
transposeSafe :: [[a]] -> [[a]]
transposeSafe [] = []
transposeSafe ([]:_) = []
transposeSafe rows
    | any null rows = []
    | otherwise     = map head rows : transposeSafe (map tail rows)

-- | Pad a string to the right with spaces
padRight :: Int -> String -> String
padRight n s
    | length s >= n = s
    | otherwise     = s ++ replicate (n - length s) ' '

-- | Format a double to 2 decimal places
formatDouble :: Double -> String
formatDouble x = show (fromIntegral (round (x * 100) :: Int) / 100 :: Double)
