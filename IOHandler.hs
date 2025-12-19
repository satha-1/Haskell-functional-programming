{-|
Module      : IOHandler
Description : IO operations and menu-driven CLI for the analytics system
License     : MIT
Maintainer  : student

This module contains all IO operations including file reading,
user interaction, and result display. Side effects are isolated
here while computation remains pure.
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
-- Reads the CSV file and runs the interactive menu loop
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
            putStrLn "\nThank you for using the Student Performance Analytics System!"
            putStrLn "Goodbye!\n"
        _ -> do
            putStrLn "\n[!] Invalid choice. Please enter 1, 2, 3, or 4.\n"
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
    putStrLn "|  (4) Exit                                    |"
    putStrLn "+----------------------------------------------+"

-- | Print summary analytics report
printSummaryAnalytics :: [SubjectName] -> [Student] -> [StudentResult] -> IO ()
printSummaryAnalytics subjects students results = do
    putStrLn "\n+------------------------------------------------------------------+"
    putStrLn "|                    SUMMARY ANALYTICS REPORT                      |"
    putStrLn "+------------------------------------------------------------------+"
    
    -- Class Overview
    putStrLn "\n=== CLASS OVERVIEW ==="
    putStrLn $ "   Total Students: " ++ show (length students)
    putStrLn $ "   Subjects: " ++ unwords subjects
    putStrLn $ "   Class Average: " ++ formatDouble (classAverage results) ++ "%"
    
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
        subjCols = map (padRight 8) subjects
        totalCol = padRight 8 "Total"
        avgCol = padRight 8 "Avg"
        gradeCol = "Grade"
        dashLine = replicate 74 '-'
    
    putStrLn $ "   +" ++ dashLine ++ "+"
    putStrLn $ "   | " ++ idCol ++ " " ++ nameCol ++ " " ++ 
               unwords subjCols ++ " " ++ totalCol ++ " " ++ avgCol ++ " " ++ gradeCol ++ " |"
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
        markCols = map (padRight 8 . show) (marks s)
        -- Pad marks if fewer than expected subjects
        paddedMarks = markCols ++ replicate (length subjects - length markCols) (padRight 8 "-")
        totalCol = padRight 8 (show $ totalMarks res)
        avgCol = padRight 8 (formatDouble $ avgMarks res)
        gradeCol = show (grade res)
    
    putStrLn $ "   | " ++ idCol ++ " " ++ nameCol ++ " " ++ 
               unwords paddedMarks ++ " " ++ totalCol ++ " " ++ avgCol ++ " " ++ gradeCol ++ "     |"

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
                putStrLn "   +------------------------------------------------+"
                putStrLn "   | Rank   ID       Name            Total   Grade  |"
                putStrLn "   +------------------------------------------------+"
                mapM_ printRankedStudent (zip [1..] topStudents)
                putStrLn "   +------------------------------------------------+"
                putStrLn ""

-- | Print a ranked student entry
printRankedStudent :: (Int, StudentResult) -> IO ()
printRankedStudent (rank, res) = do
    let s = student res
        rankStr = padRight 6 ("#" ++ show rank)
        idCol = padRight 8 (studentId s)
        nameCol = padRight 15 (take 14 $ studentName s)
        totalCol = padRight 7 (show $ totalMarks res)
        gradeCol = show (grade res)
    
    putStrLn $ "   | " ++ rankStr ++ " " ++ idCol ++ " " ++ nameCol ++ " " ++ 
               totalCol ++ " " ++ gradeCol ++ "     |"

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
            putStrLn "   | ID       Name             Average   Total   Status |"
            putStrLn "   +----------------------------------------------------+"
            mapM_ printFailingStudent failingStudents
            putStrLn "   +----------------------------------------------------+"
            putStrLn "\n   [*] These students need additional support."
            putStrLn ""

-- | Print a single failing student entry
printFailingStudent :: StudentResult -> IO ()
printFailingStudent res = do
    let s = student res
        idCol = padRight 8 (studentId s)
        nameCol = padRight 16 (take 15 $ studentName s)
        avgCol = padRight 9 (formatDouble (avgMarks res) ++ "%")
        totalCol = padRight 7 (show $ totalMarks res)
        statusCol = "FAIL"
    
    putStrLn $ "   | " ++ idCol ++ " " ++ nameCol ++ " " ++ avgCol ++ " " ++ 
               totalCol ++ " " ++ statusCol ++ "   |"

-- | Print the application banner
printBanner :: IO ()
printBanner = do
    putStrLn ""
    putStrLn "+-------------------------------------------------------------------+"
    putStrLn "|                                                                   |"
    putStrLn "|     STUDENT PERFORMANCE ANALYTICS SYSTEM                          |"
    putStrLn "|                                                                   |"
    putStrLn "|     A Functional Programming Approach with Parallel Processing    |"
    putStrLn "|                                                                   |"
    putStrLn "+-------------------------------------------------------------------+"
    putStrLn ""
