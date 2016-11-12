module Main where

import Control.Monad
import System.Exit
import qualified ShellCheck.Checker
import qualified ShellCheck.Analytics
import qualified ShellCheck.AnalyzerLib
import qualified ShellCheck.Parser
import qualified ShellCheck.Checks.Commands
import qualified ShellCheck.Checks.ShellSupport

main = do
    putStrLn "Running ShellCheck tests..."
    results <- sequence [
        ShellCheck.Checker.runTests,
        ShellCheck.Checks.Commands.runTests,
        ShellCheck.Checks.ShellSupport.runTests,
        ShellCheck.Analytics.runTests,
        ShellCheck.AnalyzerLib.runTests,
        ShellCheck.Parser.runTests
      ]
    if and results
      then exitSuccess
      else exitFailure
