module Main where

import Control.Monad
import System.Exit
import qualified ShellCheck.Analytics
import qualified ShellCheck.AnalyzerLib
import qualified ShellCheck.Checker
import qualified ShellCheck.Checks.Commands
import qualified ShellCheck.Checks.Custom
import qualified ShellCheck.Checks.ShellSupport
import qualified ShellCheck.Fixer
import qualified ShellCheck.Formatter.Diff
import qualified ShellCheck.Parser

main = do
    putStrLn "Running ShellCheck tests..."
    results <- sequence [
        ShellCheck.Analytics.runTests
        ,ShellCheck.AnalyzerLib.runTests
        ,ShellCheck.Checker.runTests
        ,ShellCheck.Checks.Commands.runTests
        ,ShellCheck.Checks.Custom.runTests
        ,ShellCheck.Checks.ShellSupport.runTests
        ,ShellCheck.Fixer.runTests
        ,ShellCheck.Formatter.Diff.runTests
        ,ShellCheck.Parser.runTests
      ]
    if and results
      then exitSuccess
      else exitFailure
