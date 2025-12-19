{-|
Module      : Main
Description : Entry point for the Student Performance Analytics System
License     : MIT
Maintainer  : student

This is the main entry point that simply delegates to the IOHandler module.
The application is designed following functional programming best practices:

- Purity: All computations are pure except for IO operations
- Modularity: Functionality is separated into focused modules
- Parallelism: Uses safe parallel evaluation with parList rdeepseq
- Error Handling: Graceful handling of file and parsing errors

Compile with:
    ghc -O2 -threaded -rtsopts Main.hs -o student-analytics

Run with:
    ./student-analytics +RTS -N

The -N flag enables parallel execution on all available cores.
-}

module Main where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import IOHandler (runApp)

-- | Main entry point - delegates to the IO handler
main :: IO ()
main = do
    -- Fix for Windows console Unicode/Emoji support
    setLocaleEncoding utf8
    runApp
