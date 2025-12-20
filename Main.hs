{-|
Module      : Main
Description : Entry point for the Student Performance Analytics System
License     : MIT
Maintainer  : student

Entry point for the application.
Ensures UTF-8 encoding for standard IO.
-}

module Main where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import IOHandler (runApp)

-- | Main entry point
main :: IO ()
main = do
    setLocaleEncoding utf8
    runApp
