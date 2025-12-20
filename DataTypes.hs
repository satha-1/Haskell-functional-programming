{-|
Module      : DataTypes
Description : Core data types for the Student Performance Analytics System
License     : MIT
Maintainer  : student

This module defines pure data types used throughout the application.
Updated to include attendance data and risk assessment.
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

instance NFData Grade

-- | Raw student data parsed from CSV
-- Now includes attendance information
data Student = Student
    { studentId         :: String    -- ^ Unique student identifier
    , studentName       :: String    -- ^ Student's full name
    , attendancePresent :: Int       -- ^ Days present
    , attendanceTotal   :: Int       -- ^ Total working days
    , marks             :: [Int]     -- ^ List of marks for each subject
    } deriving (Eq, Show, Generic)

instance NFData Student

-- | Processed student result with computed analytics
data StudentResult = StudentResult
    { student        :: Student      -- ^ Original student data
    , totalMarks     :: Int          -- ^ Sum of all marks
    , avgMarks       :: Double       -- ^ Average of all marks
    , grade          :: Grade        -- ^ Computed grade
    , attendanceRate :: Double       -- ^ Percentage (0.0 - 100.0)
    , isAtRisk       :: Bool         -- ^ True if student needs intervention
    } deriving (Eq, Show, Generic)

instance NFData StudentResult

-- | Errors that can occur during CSV parsing
data ParseError
    = EmptyFile                    -- ^ No data in the file
    | InvalidHeader String         -- ^ Header row is malformed
    | InvalidRow Int String        -- ^ Row number and error description
    | InvalidMark Int String Int   -- ^ Row, column name, and value
    | InvalidAttendance Int String -- ^ Row and error description for attendance
    deriving (Eq, Show)
