module Main where

import Control.Monad
import System.Exit
import qualified ShellCheck.Checker
import qualified ShellCheck.Analytics
import qualified ShellCheck.Parser

main = do
    putStrLn "Running ShellCheck tests..."
    results <- sequence [
        ShellCheck.Checker.runTests,
        ShellCheck.Analytics.runTests,
        ShellCheck.Parser.runTests
      ]
    if and results
      then exitSuccess
      else exitFailure
