{-|
Module      : IOHandler
Description : IO operations and menu-driven CLI for the analytics system
License     : MIT
Maintainer  : student

This module contains all IO operations including file reading,
user interaction, and result display. Uses "Safe Mode" ASCII
characters ensuring compatibility across all terminals.
-}

module IOHandler
    ( runApp
    ) where

import System.IO (hFlush, stdout)
import Control.Exception (try, IOException)
import Text.Read (readMaybe)

import DataTypes
import Utils
import Processing

-- | Main application entry point
runApp :: IO ()
runApp = do
    printBanner
    putStrLn "Enter the path to the CSV file (or press Enter for 'students.csv'):"
    putStr "> "
    hFlush stdout
    input <- getLine
    let filePath = if null (trim input) then "students.csv" else trim input
    
    result <- try (readFile filePath) :: IO (Either IOException String)
    case result of
        Left err -> do
            putStrLn $ "\n[!] Error reading file: " ++ show err
            putStrLn "Please ensure the file exists and try again."
        Right content -> processCSVContent content

-- | Process the CSV content and run the menu
processCSVContent :: String -> IO ()
processCSVContent content =
    case parseCSV content of
        Left EmptyFile -> 
            putStrLn "\n[!] Error: The CSV file is empty or has no data rows."
        Left (InvalidHeader msg) ->
            putStrLn $ "\n[!] Error parsing header: " ++ msg
        Left (InvalidRow lineNum msg) ->
            putStrLn $ "\n[!] Error on line " ++ show lineNum ++ ": " ++ msg
        Left (InvalidMark lineNum subj _) ->
            putStrLn $ "\n[!] Error on line " ++ show lineNum ++ 
                       ": Invalid mark for subject '" ++ subj ++ "'"
        Left (InvalidAttendance lineNum msg) ->
            putStrLn $ "\n[!] Error on line " ++ show lineNum ++ ": " ++ msg
        Right (subjects, students) -> do
            putStrLn $ "\n[OK] Successfully loaded " ++ show (length students) ++ 
                       " students with " ++ show (length subjects) ++ " subjects."
            putStrLn "Computing results in parallel..."
            
            -- Compute results using parallel evaluation
            let results = computeResultsParallel students
            
            -- Force evaluation and run menu
            putStrLn $ "Processed " ++ show (length results) ++ " student results.\n"
            menuLoop subjects students results

-- | Interactive menu loop
menuLoop :: [SubjectName] -> [Student] -> [StudentResult] -> IO ()
menuLoop subjects students results = do
    printMenu
    putStr "Enter choice: "
    hFlush stdout
    choice <- getLine
    
    case trim choice of
        "1" -> do
            printSummaryAnalytics subjects students results
            menuLoop subjects students results
        "2" -> do
            printTopNStudents results
            menuLoop subjects students results
        "3" -> do
            printFailingStudents results
            menuLoop subjects students results
        "4" -> do
            printAttendanceAnalysis results
            menuLoop subjects students results
        "5" -> do
            putStrLn "\nThank you for using the Student Performance Analytics System!"
            putStrLn "Goodbye!\n"
        _ -> do
            putStrLn "\n[!] Invalid choice. Please enter 1-5.\n"
            menuLoop subjects students results

-- | Print the main menu options
printMenu :: IO ()
printMenu = do
    putStrLn "+----------------------------------------------+"
    putStrLn "|         STUDENT ANALYTICS MENU               |"
    putStrLn "+----------------------------------------------+"
    putStrLn "|  (1) Summary Analytics                       |"
    putStrLn "|  (2) Top N Students (by total marks)         |"
    putStrLn "|  (3) List Failing Students (Grade F)         |"
    putStrLn "|  (4) Attendance vs Marks Analytics           |"
    putStrLn "|  (5) Exit                                    |"
    putStrLn "+----------------------------------------------+"

-- | Print summary analytics report
printSummaryAnalytics :: [SubjectName] -> [Student] -> [StudentResult] -> IO ()
printSummaryAnalytics subjects students results = do
    putStrLn "\n+------------------------------------------------------------------+"
    putStrLn "|                    SUMMARY ANALYTICS REPORT                      |"
    putStrLn "+------------------------------------------------------------------+"
    
    -- Class Overview
    putStrLn "\n=== CLASS OVERVIEW ==="
    putStrLn $ "   Total Students:  " ++ show (length students)
    putStrLn $ "   Subjects:        " ++ unwords subjects
    putStrLn $ "   Class Avg Marks: " ++ formatDouble (classAverage results) ++ "%"
    
    -- Avg Attendance
    let avgAtt = mean (map attendanceRate results)
    putStrLn $ "   Class Avg Att:   " ++ formatDouble avgAtt ++ "%"
    
    -- Subject Averages
    putStrLn "\n=== SUBJECT AVERAGES ==="
    let subjAvgs = subjectAverages subjects students
    mapM_ printSubjectAverage subjAvgs
    
    -- Grade Distribution
    putStrLn "\n=== GRADE DISTRIBUTION ==="
    printGradeDistribution results
    
    -- All Students Table
    putStrLn "\n=== ALL STUDENTS ==="
    printStudentTable subjects results
    putStrLn ""

-- | Print a single subject average
printSubjectAverage :: (SubjectName, Double) -> IO ()
printSubjectAverage (subj, avg) =
    putStrLn $ "   " ++ padRight 12 subj ++ ": " ++ formatDouble avg ++ "%"

-- | Print grade distribution
printGradeDistribution :: [StudentResult] -> IO ()
printGradeDistribution results = do
    let countGrade g = length $ filter ((== g) . grade) results
        total = length results
        percentage g = if total == 0 then 0.0 
                       else 100.0 * fromIntegral (countGrade g) / fromIntegral total
        bar g = replicate (round (percentage g / 5) :: Int) '#'
    
    putStrLn $ "   A (>=80): " ++ padRight 3 (show (countGrade A)) ++ " " ++ bar A
    putStrLn $ "   B (>=70): " ++ padRight 3 (show (countGrade B)) ++ " " ++ bar B
    putStrLn $ "   C (>=60): " ++ padRight 3 (show (countGrade C)) ++ " " ++ bar C
    putStrLn $ "   D (>=50): " ++ padRight 3 (show (countGrade D)) ++ " " ++ bar D
    putStrLn $ "   F (<50):  " ++ padRight 3 (show (countGrade F)) ++ " " ++ bar F

-- | Print the student results table
printStudentTable :: [SubjectName] -> [StudentResult] -> IO ()
printStudentTable subjects results = do
    -- Header
    let idCol = padRight 6 "ID"
        nameCol = padRight 12 "Name"
        attCol = padRight 6 "Att%"
        subjCols = map (padRight 6) subjects
        totalCol = padRight 6 "Total"
        avgCol = padRight 7 "Avg"
        gradeCol = padRight 6 "Gra"
        statusCol = "Status"
        dashLine = replicate 84 '-'
    
    putStrLn $ "   +" ++ dashLine ++ "+"
    putStrLn $ "   | " ++ idCol ++ " " ++ nameCol ++ " " ++ attCol ++ " " ++
               unwords subjCols ++ " " ++ totalCol ++ " " ++ avgCol ++ " " ++ gradeCol ++ " " ++ statusCol ++ " |"
    putStrLn $ "   +" ++ dashLine ++ "+"
    
    -- Data rows
    mapM_ (printStudentRow subjects) results
    
    putStrLn $ "   +" ++ dashLine ++ "+"

-- | Print a single student row
printStudentRow :: [SubjectName] -> StudentResult -> IO ()
printStudentRow subjects res = do
    let s = student res
        idCol = padRight 6 (studentId s)
        nameCol = padRight 12 (take 11 $ studentName s)
        attCol = padRight 6 (formatDouble $ attendanceRate res)
        markCols = map (padRight 6 . show) (marks s)
        paddedMarks = markCols ++ replicate (length subjects - length markCols) (padRight 6 "-")
        totalCol = padRight 6 (show $ totalMarks res)
        avgCol = padRight 7 (formatDouble $ avgMarks res)
        gradeCol = padRight 6 (show $ grade res)
        status = if isAtRisk res then "AtRisk" else "OK"
        statusCol = status
    
    putStrLn $ "   | " ++ idCol ++ " " ++ nameCol ++ " " ++ attCol ++ " " ++
               unwords paddedMarks ++ " " ++ totalCol ++ " " ++ avgCol ++ " " ++ gradeCol ++ " " ++ statusCol ++ " |"

-- | Prompt for and print top N students
printTopNStudents :: [StudentResult] -> IO ()
printTopNStudents results = do
    putStr "\nEnter the number of top students to display: "
    hFlush stdout
    input <- getLine
    case readMaybe (trim input) :: Maybe Int of
        Nothing -> putStrLn "[!] Invalid number. Please enter a positive integer.\n"
        Just n
            | n <= 0 -> putStrLn "[!] Please enter a positive number.\n"
            | otherwise -> do
                let topStudents = topN n results
                putStrLn $ "\n=== TOP " ++ show (length topStudents) ++ " STUDENTS (by total marks) ==="
                putStrLn "   +------------------------------------------------------------+"
                putStrLn "   | Rank   ID       Name            Att%    Total   Grade      |"
                putStrLn "   +------------------------------------------------------------+"
                mapM_ printRankedStudent (zip [1..] topStudents)
                putStrLn "   +------------------------------------------------------------+"
                putStrLn ""

-- | Print a ranked student entry
printRankedStudent :: (Int, StudentResult) -> IO ()
printRankedStudent (rank, res) = do
    let s = student res
        rankStr = padRight 6 ("#" ++ show rank)
        idCol = padRight 8 (studentId s)
        nameCol = padRight 15 (take 14 $ studentName s)
        attCol = padRight 8 (formatDouble $ attendanceRate res)
        totalCol = padRight 7 (show $ totalMarks res)
        gradeCol = show (grade res)
    
    putStrLn $ "   | " ++ rankStr ++ " " ++ idCol ++ " " ++ nameCol ++ " " ++ attCol ++ " " ++
               totalCol ++ " " ++ gradeCol ++ "          |"

-- | Print failing students (Grade F)
printFailingStudents :: [StudentResult] -> IO ()
printFailingStudents results = do
    let failingStudents = failing results
    putStrLn "\n=== FAILING STUDENTS (Grade F) ==="
    
    if null failingStudents
        then putStrLn "   Congratulations! No students are failing.\n"
        else do
            putStrLn $ "   Found " ++ show (length failingStudents) ++ " failing student(s):\n"
            putStrLn "   +----------------------------------------------------+"
            putStrLn "   | ID       Name             Average   Att%    Status |"
            putStrLn "   +----------------------------------------------------+"
            mapM_ printFailingStudent failingStudents
            putStrLn "   +----------------------------------------------------+"
            putStrLn "\n   [*] These students need additional support."
            putStrLn ""

printFailingStudent :: StudentResult -> IO ()
printFailingStudent res = do
    let s = student res
        idCol = padRight 8 (studentId s)
        nameCol = padRight 16 (take 15 $ studentName s)
        avgCol = padRight 9 (formatDouble (avgMarks res) ++ "%")
        attCol = padRight 7 (formatDouble $ attendanceRate res)
        statusCol = "FAIL"
    
    putStrLn $ "   | " ++ idCol ++ " " ++ nameCol ++ " " ++ avgCol ++ " " ++ attCol ++ " " ++ statusCol ++ "   |"

-- | Print Attendance Analytics Report
printAttendanceAnalysis :: [StudentResult] -> IO ()
printAttendanceAnalysis results = do
    let (highAvg, medAvg, lowAvg, mCorr) = computeAttendanceAnalytics results
    
    putStrLn "\n+--------------------------------------------------------------+"
    putStrLn "|               ATTENDANCE vs MARKS ANALYSIS                   |"
    putStrLn "+--------------------------------------------------------------+"
    
    -- Correlation Section
    putStrLn "\n1. STATISTICAL CORRELATION"
    case mCorr of
        Nothing -> putStrLn "   [!] Insufficient data to calculate correlation."
        Just r -> do
            putStrLn $ "   Pearson Correlation (r): " ++ formatDouble r
            putStrLn $ "   Interpretation:          " ++ interpretCorrelation r
    
    -- Group Comparison
    putStrLn "\n2. GROUP COMPARISON (Avg Exam Performance)"
    putStrLn "   +---------------------+----------------+"
    putStrLn "   | Attendance Group    | Avg Exam Mark  |"
    putStrLn "   +---------------------+----------------+"
    putStrLn $ "   | High (>85%)         | " ++ padRight 14 (formatDouble highAvg ++ "%") ++ " |"
    putStrLn $ "   | Medium (70-85%)     | " ++ padRight 14 (formatDouble medAvg ++ "%") ++ " |"
    putStrLn $ "   | Low (<70%)          | " ++ padRight 14 (formatDouble lowAvg ++ "%") ++ " |"
    putStrLn "   +---------------------+----------------+"
    
    -- At Risk Summary
    let riskCount = length $ filter isAtRisk results
    putStrLn $ "\n3. RISK ASSESSMENT"
    putStrLn $ "   Students At Risk: " ++ show riskCount ++ " (Low attendance or poor performance)"
    putStrLn ""

interpretCorrelation :: Double -> String
interpretCorrelation r
    | r >= 0.5   = "Strong POSITIVE relationship (More attendance = Better marks)"
    | r >= 0.3   = "Moderate POSITIVE relationship"
    | r >= 0.1   = "Weak POSITIVE relationship"
    | r >= -0.1  = "NO clear relationship"
    | otherwise  = "NEGATIVE relationship (Unusual)"

-- | Print the application banner
printBanner :: IO ()
printBanner = do
    putStrLn ""
    putStrLn "+-------------------------------------------------------------------+"
    putStrLn "|                                                                   |"
    putStrLn "|     STUDENT PERFORMANCE ANALYTICS SYSTEM                          |"
    putStrLn "|     (v2.0 - Attendance & Statistics Module)                       |"
    putStrLn "|                                                                   |"
    putStrLn "+-------------------------------------------------------------------+"
    putStrLn ""
