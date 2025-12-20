{-|
Module      : Processing
Description : Pure data processing functions with attendance analytics
License     : MIT
Maintainer  : student

This module contains pure functions for parsing CSV data, computing
student results including attendance, and performing advanced analytics.
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
    , computeAttendanceAnalytics
    , pearsonCorrelation
    , AttendanceGroup(..)
    ) where

import Control.Parallel.Strategies (parList, rdeepseq, using)
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))

import DataTypes
import Utils

-- | Enum for grouping students by attendance
data AttendanceGroup = HighAtt | MedAtt | LowAtt
    deriving (Eq, Show)

-- | Parse CSV content into a list of Students and subject names
-- Format: id,name,attPresent,attTotal,Subject1,Subject2,...
parseCSV :: String -> Either ParseError ([SubjectName], [Student])
parseCSV content
    | null (lines content) = Left EmptyFile
    | length (lines content) < 2 = Left EmptyFile
    | otherwise = do
        let allLines = lines content
            headerLine = head allLines
            dataLines = tail allLines
        
        subjects <- parseHeader headerLine
        students <- parseDataLines subjects dataLines 2
        
        Right (subjects, students)

-- | Header: id,name,attPresent,attTotal,Subj1,Subj2...
-- Returns just the subject names (cols 4+)
parseHeader :: String -> Either ParseError [SubjectName]
parseHeader line =
    let cols = map trim $ splitBy ',' line
    in if length cols < 5
        then Left $ InvalidHeader "Header must have at least 5 cols (id,name,present,total,subject...)"
        else Right $ drop 4 cols

-- | Parse all data lines
parseDataLines :: [SubjectName] -> [String] -> Int -> Either ParseError [Student]
parseDataLines _ [] _ = Right []
parseDataLines subjects (line:rest) lineNum
    | all isSpace line = parseDataLines subjects rest (lineNum + 1)
    | otherwise = do
        student <- parseStudentLine subjects line lineNum
        remainingStudents <- parseDataLines subjects rest (lineNum + 1)
        Right (student : remainingStudents)
  where
    isSpace c = c `elem` " \t\r\n"

-- | Parse a single data line
parseStudentLine :: [SubjectName] -> String -> Int -> Either ParseError Student
parseStudentLine subjects line lineNum =
    let cols = map trim $ splitBy ',' line
        expectedCols = 4 + length subjects
    in if length cols < expectedCols
        then Left $ InvalidRow lineNum $ 
            "Expected " ++ show expectedCols ++ " columns, got " ++ show (length cols)
        else do
            let sid = cols !! 0
                sname = cols !! 1
                presStr = cols !! 2
                totStr = cols !! 3
                markStrings = take (length subjects) $ drop 4 cols
            
            -- Validation for attendance
            (pres, tot) <- case (safeReadInt presStr, safeReadInt totStr) of
                (Just p, Just t) -> 
                    if t > 0 && p >= 0 && p <= t 
                    then Right (p, t)
                    else Left $ InvalidAttendance lineNum $ 
                        "Invalid attendance (Present: " ++ show p ++ ", Total: " ++ show t ++ ")"
                _ -> Left $ InvalidAttendance lineNum "Non-integer attendance values"

            parsedMarks <- parseMarks markStrings subjects lineNum
            
            Right Student
                { studentId = sid
                , studentName = sname
                , attendancePresent = pres
                , attendanceTotal = tot
                , marks = parsedMarks
                }

-- | Parse subject marks
parseMarks :: [String] -> [SubjectName] -> Int -> Either ParseError [Int]
parseMarks [] _ _ = Right []
parseMarks (m:ms) (s:ss) lineNum =
    case safeReadInt m of
        Nothing -> Left $ InvalidMark lineNum s 0
        Just n  -> do
            rest <- parseMarks ms ss lineNum
            Right (n : rest)
parseMarks _ [] _ = Right []

determineGrade :: Double -> Grade
determineGrade avg
    | avg >= 80 = A
    | avg >= 70 = B
    | avg >= 60 = C
    | avg >= 50 = D
    | otherwise = F

-- | Compute full result including attendance metrics
computeResult :: Student -> StudentResult
computeResult s =
    let total = sum (marks s)
        avg = avgInts (marks s)
        g = determineGrade avg
        
        -- Attendance calc
        attRate = if attendanceTotal s == 0 
                  then 0.0 
                  else (fromIntegral (attendancePresent s) / fromIntegral (attendanceTotal s)) * 100.0
        
        -- "At Risk" logic: Low attendance OR Failing grade OR Low average
        risk = attRate < 70.0 || g == F || avg < 50.0

    in StudentResult
        { student = s
        , totalMarks = total
        , avgMarks = avg
        , grade = g
        , attendanceRate = attRate
        , isAtRisk = risk
        }

-- | Parallel computation of results
computeResultsParallel :: [Student] -> [StudentResult]
computeResultsParallel students =
    map computeResult students `using` parList rdeepseq

-- | Class average marks
classAverage :: [StudentResult] -> Double
classAverage [] = 0.0
classAverage results = avgDoubles $ map avgMarks results

-- | Subject averages
subjectAverages :: [SubjectName] -> [Student] -> [(SubjectName, Double)]
subjectAverages subjects students =
    let allMarks = map marks students
        columnMarks = transposeSafe allMarks
        averages = map avgInts columnMarks
    in zip subjects averages

topN :: Int -> [StudentResult] -> [StudentResult]
topN n = take n . sortBy (comparing (Down . totalMarks))

failing :: [StudentResult] -> [StudentResult]
failing = filter ((== F) . grade)

getSubjectNames :: Either ParseError ([SubjectName], [Student]) -> [SubjectName]
getSubjectNames (Right (subjects, _)) = subjects
getSubjectNames (Left _) = []

-- | Calculate Pearson Correlation Coefficient r
-- r = Cov(X,Y) / (stdDev(X) * stdDev(Y))
pearsonCorrelation :: [Double] -> [Double] -> Maybe Double
pearsonCorrelation xs ys
    | length xs /= length ys || null xs = Nothing
    | otherwise = do
        let sdX = stdDev xs
            sdY = stdDev ys
            cov = covariance xs ys
        if sdX == 0 || sdY == 0
            then Nothing -- Avoid division by zero (no variance in data)
            else Just (cov / (sdX * sdY))

-- | Determine Attendance Group
getAttGroup :: Double -> AttendanceGroup
getAttGroup rate
    | rate >= 85.0 = HighAtt
    | rate >= 70.0 = MedAtt
    | otherwise    = LowAtt

-- | Compute Average Exam Average per Attendance Group
-- Returns (HighAvg, MedAvg, LowAvg) tuples
computeAttendanceAnalytics :: [StudentResult] -> (Double, Double, Double, Maybe Double)
computeAttendanceAnalytics results =
    let 
        -- Extract data vectors
        attRates = map attendanceRate results
        examAvgs = map avgMarks results
        
        -- Compute Correlation
        correlation = pearsonCorrelation attRates examAvgs
        
        -- Grouping
        highGroup = filter (\r -> getAttGroup (attendanceRate r) == HighAtt) results
        medGroup  = filter (\r -> getAttGroup (attendanceRate r) == MedAtt) results
        lowGroup  = filter (\r -> getAttGroup (attendanceRate r) == LowAtt) results
        
        -- Compute Group Averages
        highAvg = if null highGroup then 0.0 else mean (map avgMarks highGroup)
        medAvg  = if null medGroup then 0.0 else mean (map avgMarks medGroup)
        lowAvg  = if null lowGroup then 0.0 else mean (map avgMarks lowGroup)
        
    in (highAvg, medAvg, lowAvg, correlation)
