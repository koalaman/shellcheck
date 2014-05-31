module Main where

import Control.Monad
import System.Exit
import qualified ShellCheck.Simple
import qualified ShellCheck.Analytics
import qualified ShellCheck.Parser

main = do
    putStrLn "Running ShellCheck tests..."
    results <- sequence [ShellCheck.Simple.runTests,
                         ShellCheck.Analytics.runTests,
                         ShellCheck.Parser.runTests]
    if and results then exitSuccess
     else exitFailure

