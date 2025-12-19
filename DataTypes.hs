{-|
Module      : DataTypes
Description : Core data types for the Student Performance Analytics System
License     : MIT
Maintainer  : student

This module defines pure data types used throughout the application.
All types are designed to be immutable and support deep evaluation
for parallel processing.
-}

module DataTypes
    ( Student(..)
    , StudentResult(..)
    , Grade(..)
    , SubjectName
    , ParseError(..)
    ) where

import Control.DeepSeq (NFData(..))
import GHC.Generics (Generic)

-- | Type alias for subject names (e.g., "Math", "English")
type SubjectName = String

-- | Grade classification based on average marks
-- A >= 80, B >= 70, C >= 60, D >= 50, F < 50
data Grade = A | B | C | D | F
    deriving (Eq, Ord, Show, Read, Generic)

-- NFData instance for parallel evaluation
instance NFData Grade

-- | Raw student data parsed from CSV
data Student = Student
    { studentId   :: String        -- ^ Unique student identifier (e.g., "S01")
    , studentName :: String        -- ^ Student's full name
    , marks       :: [Int]         -- ^ List of marks for each subject
    } deriving (Eq, Show, Generic)

instance NFData Student

-- | Processed student result with computed analytics
data StudentResult = StudentResult
    { student     :: Student       -- ^ Original student data
    , totalMarks  :: Int           -- ^ Sum of all marks
    , avgMarks    :: Double        -- ^ Average of all marks
    , grade       :: Grade         -- ^ Computed grade based on average
    } deriving (Eq, Show, Generic)

instance NFData StudentResult

-- | Errors that can occur during CSV parsing
data ParseError
    = EmptyFile                    -- ^ No data in the file
    | InvalidHeader String         -- ^ Header row is malformed
    | InvalidRow Int String        -- ^ Row number and error description
    | InvalidMark Int String Int   -- ^ Row, column name, and value
    deriving (Eq, Show)
