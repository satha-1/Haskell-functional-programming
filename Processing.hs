{-|
Module      : Processing
Description : Pure data processing functions with parallel evaluation support
License     : MIT
Maintainer  : student

This module contains pure functions for parsing CSV data, computing
student results, and performing analytics. All functions are pure
and support parallel evaluation using Control.Parallel.Strategies.
-}

module Processing
    ( parseCSV
    , computeResult
    , computeResultsParallel
    , classAverage
    , subjectAverages
    , topN
    , failing
    , determineGrade
    , getSubjectNames
    ) where

import Control.Parallel.Strategies (parList, rdeepseq, using)
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))

import DataTypes
import Utils

-- | Parse CSV content into a list of Students and subject names
-- Returns either a ParseError or a tuple of (subject names, students)
-- 
-- The first row is treated as the header with format:
-- id,name,Subject1,Subject2,...
parseCSV :: String -> Either ParseError ([SubjectName], [Student])
parseCSV content
    | null (lines content) = Left EmptyFile
    | length (lines content) < 2 = Left EmptyFile
    | otherwise = do
        let allLines = lines content
            headerLine = head allLines
            dataLines = tail allLines
        
        -- Parse header to get subject names
        subjects <- parseHeader headerLine
        
        -- Parse each data line
        students <- parseDataLines subjects dataLines 2  -- Start at line 2
        
        Right (subjects, students)

-- | Parse the header row to extract subject names
-- Expected format: id,name,Subject1,Subject2,...
parseHeader :: String -> Either ParseError [SubjectName]
parseHeader line =
    let cols = map trim $ splitBy ',' line
    in if length cols < 3
        then Left $ InvalidHeader "Header must have at least 3 columns (id, name, and at least one subject)"
        else Right $ drop 2 cols  -- Skip id and name columns

-- | Parse all data lines into Students
parseDataLines :: [SubjectName] -> [String] -> Int -> Either ParseError [Student]
parseDataLines _ [] _ = Right []
parseDataLines subjects (line:rest) lineNum
    | all isSpace line = parseDataLines subjects rest (lineNum + 1)  -- Skip empty lines
    | otherwise = do
        student <- parseStudentLine subjects line lineNum
        remainingStudents <- parseDataLines subjects rest (lineNum + 1)
        Right (student : remainingStudents)
  where
    isSpace c = c `elem` " \t\r\n"

-- | Parse a single data line into a Student
parseStudentLine :: [SubjectName] -> String -> Int -> Either ParseError Student
parseStudentLine subjects line lineNum =
    let cols = map trim $ splitBy ',' line
        expectedCols = 2 + length subjects
    in if length cols < expectedCols
        then Left $ InvalidRow lineNum $ 
            "Expected " ++ show expectedCols ++ " columns, got " ++ show (length cols)
        else do
            let sid = cols !! 0
                sname = cols !! 1
                markStrings = take (length subjects) $ drop 2 cols
            
            -- Parse all marks, collecting any errors
            parsedMarks <- parseMarks markStrings subjects lineNum
            
            Right Student
                { studentId = sid
                , studentName = sname
                , marks = parsedMarks
                }

-- | Parse a list of mark strings into integers
parseMarks :: [String] -> [SubjectName] -> Int -> Either ParseError [Int]
parseMarks [] _ _ = Right []
parseMarks (m:ms) (s:ss) lineNum =
    case safeReadInt m of
        Nothing -> Left $ InvalidMark lineNum s 0
        Just n  -> do
            rest <- parseMarks ms ss lineNum
            Right (n : rest)
parseMarks _ [] _ = Right []  -- Extra marks are ignored

-- | Determine grade based on average marks
-- A >= 80, B >= 70, C >= 60, D >= 50, F < 50
determineGrade :: Double -> Grade
determineGrade avg
    | avg >= 80 = A
    | avg >= 70 = B
    | avg >= 60 = C
    | avg >= 50 = D
    | otherwise = F

-- | Compute the result for a single student (pure function)
computeResult :: Student -> StudentResult
computeResult s =
    let total = sum (marks s)
        avg = avgInts (marks s)
        g = determineGrade avg
    in StudentResult
        { student = s
        , totalMarks = total
        , avgMarks = avg
        , grade = g
        }

-- | Compute results for all students in parallel
-- Uses Control.Parallel.Strategies for safe parallel evaluation
computeResultsParallel :: [Student] -> [StudentResult]
computeResultsParallel students =
    map computeResult students `using` parList rdeepseq

-- | Calculate the class average (average of all student averages)
classAverage :: [StudentResult] -> Double
classAverage [] = 0.0
classAverage results = avgDoubles $ map avgMarks results

-- | Calculate the average marks for each subject
-- Returns a list of (SubjectName, Average) pairs
subjectAverages :: [SubjectName] -> [Student] -> [(SubjectName, Double)]
subjectAverages subjects students =
    let allMarks = map marks students
        -- Transpose to get column-wise marks
        columnMarks = transposeSafe allMarks
        -- Calculate average for each column
        averages = map avgInts columnMarks
    in zip subjects averages

-- | Get top N students sorted by total marks (descending)
topN :: Int -> [StudentResult] -> [StudentResult]
topN n = take n . sortBy (comparing (Down . totalMarks))

-- | Get all students with grade F (failing)
failing :: [StudentResult] -> [StudentResult]
failing = filter ((== F) . grade)

-- | Extract subject names from parsed CSV result
getSubjectNames :: Either ParseError ([SubjectName], [Student]) -> [SubjectName]
getSubjectNames (Right (subjects, _)) = subjects
getSubjectNames (Left _) = []
