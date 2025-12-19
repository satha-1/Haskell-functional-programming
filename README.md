# 📚 Student Performance Analytics System

A Haskell mini-project demonstrating functional programming best practices, modular design, and safe parallel processing.

## 📋 Overview

This system reads student performance data from a CSV file, computes analytics, and provides an interactive menu-driven interface for exploring the results. Built using pure functional programming principles with side effects isolated to IO modules.

## 🏗️ Project Structure

```
student-performance/
├── Main.hs           # Entry point
├── DataTypes.hs      # Core data types (Student, StudentResult, Grade)
├── Utils.hs          # Pure utility functions
├── Processing.hs     # CSV parsing and parallel data processing
├── IOHandler.hs      # IO operations and menu interface
├── students.csv      # Sample input data
└── README.md         # This file
```

## 🎯 Features

1. **CSV Parsing** - Robust parsing with error handling for:
   - Empty files
   - Malformed headers
   - Invalid rows
   - Non-integer marks

2. **Student Analytics**
   - Total marks calculation
   - Average marks calculation
   - Grade assignment (A ≥ 80, B ≥ 70, C ≥ 60, D ≥ 50, F < 50)

3. **Class Analytics**
   - Class average (mean of student averages)
   - Subject-wise averages

4. **Interactive Menu**
   - Summary analytics with visualizations
   - Top N students by total marks
   - Failing students list
   - Graceful exit

5. **Parallel Processing**
   - Uses `Control.Parallel.Strategies`
   - `parList rdeepseq` for safe parallel evaluation
   - NFData instances for deep evaluation

## 🖥️ Requirements

- GHC (Glasgow Haskell Compiler) 8.0 or later
- The `parallel` package (usually included with GHC)
- The `deepseq` package (usually included with GHC)

## 🚀 Compilation & Running

### Compile the Project

```bash
ghc -O2 -threaded -rtsopts Main.hs -o student-analytics
```

Flags explained:
- `-O2`: Enable optimizations
- `-threaded`: Enable multi-threaded runtime
- `-rtsopts`: Allow RTS options at runtime

### Run the Application

**On Windows:**
```powershell
.\student-analytics.exe +RTS -N -RTS
```

**On Linux/macOS:**
```bash
./student-analytics +RTS -N
```

RTS options:
- `+RTS -N`: Use all available CPU cores for parallelism
- `+RTS -N4`: Use exactly 4 cores
- `+RTS -s`: Show runtime statistics

### Quick Test with GHCi

```bash
ghci Main.hs
> main
```

## 📊 Input Format

CSV file with the following structure:

```csv
id,name,Subject1,Subject2,Subject3,...
S01,Student Name,85,90,78,...
```

- First row: Header (id, name, followed by subject names)
- Subsequent rows: Student data
- Marks should be integers

### Example (students.csv)

```csv
id,name,Math,English,Science
S01,Ayesha,85,90,78
S02,Bilal,40,55,60
S03,Chen,92,88,95
S04,Dina,30,42,35
```

## 📐 Module Responsibilities

### DataTypes.hs
- Defines core data types: `Student`, `StudentResult`, `Grade`, `ParseError`
- Implements `NFData` instances for parallel evaluation
- Pure, no dependencies on other project modules

### Utils.hs
- Pure utility functions
- String manipulation: `splitBy`, `trim`
- Safe parsing: `safeReadInt`
- List operations: `avgInts`, `avgDoubles`, `transposeSafe`
- Formatting: `padRight`, `formatDouble`

### Processing.hs
- CSV parsing with error handling
- Grade computation logic
- Parallel result computation using `parList rdeepseq`
- Analytics: `classAverage`, `subjectAverages`, `topN`, `failing`

### IOHandler.hs
- All IO operations (file reading, console output)
- Menu loop and user interaction
- Formatted report printing
- Error message display

### Main.hs
- Minimal entry point
- Simply calls `runApp` from IOHandler

## 🎓 Functional Programming Principles

1. **Purity**: All computation functions are pure
2. **Immutability**: Data types are immutable
3. **Referential Transparency**: Functions always return same output for same input
4. **Side Effect Isolation**: IO confined to IOHandler module
5. **Higher-Order Functions**: Extensive use of `map`, `filter`, etc.
6. **Pattern Matching**: Used throughout for elegant data handling
7. **Type Safety**: Strong typing prevents runtime errors

## 🔄 Parallel Processing

The system uses Haskell's `Control.Parallel.Strategies` for safe parallelism:

```haskell
computeResultsParallel :: [Student] -> [StudentResult]
computeResultsParallel students =
    map computeResult students `using` parList rdeepseq
```

- `parList`: Evaluates list elements in parallel
- `rdeepseq`: Forces complete (deep) evaluation
- `NFData` instances: Enable deep evaluation for custom types

## 📝 Sample Output

```
╔═══════════════════════════════════════════════════════════════════╗
║     📚 STUDENT PERFORMANCE ANALYTICS SYSTEM 📊                   ║
╚═══════════════════════════════════════════════════════════════════╝

✅ Successfully loaded 10 students with 3 subjects.

╔══════════════════════════════════════════════╗
║         STUDENT ANALYTICS MENU               ║
╠══════════════════════════════════════════════╣
║  (1) Summary Analytics                       ║
║  (2) Top N Students (by total marks)         ║
║  (3) List Failing Students (Grade F)         ║
║  (4) Exit                                    ║
╚══════════════════════════════════════════════╝
```

## 📜 License

MIT License - Feel free to use for educational purposes.

## 👨‍💻 Author

Created as a functional programming demonstration project.
