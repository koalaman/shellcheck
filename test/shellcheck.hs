module Main where

import Control.Monad
import System.Exit
import qualified ShellCheck.Analytics
import qualified ShellCheck.AnalyzerLib
import qualified ShellCheck.ASTLib
import qualified ShellCheck.CFG
import qualified ShellCheck.CFGAnalysis
import qualified ShellCheck.Checker
import qualified ShellCheck.Checks.Commands
import qualified ShellCheck.Checks.ControlFlow
import qualified ShellCheck.Checks.Custom
import qualified ShellCheck.Checks.ShellSupport
import qualified ShellCheck.Fixer
import qualified ShellCheck.Formatter.Diff
import qualified ShellCheck.Parser

main = do
    putStrLn "Running ShellCheck tests..."
    failures <- filter (not . snd) <$> mapM sequenceA tests
    if null failures then exitSuccess else do
      putStrLn "Tests failed for the following module(s):"
      mapM (putStrLn . ("- ShellCheck." ++) . fst) failures
      exitFailure
  where
    tests =
      [ ("Analytics"          , ShellCheck.Analytics.runTests)
      , ("AnalyzerLib"        , ShellCheck.AnalyzerLib.runTests)
      , ("ASTLib"             , ShellCheck.ASTLib.runTests)
      , ("CFG"                , ShellCheck.CFG.runTests)
      , ("CFGAnalysis"        , ShellCheck.CFGAnalysis.runTests)
      , ("Checker"            , ShellCheck.Checker.runTests)
      , ("Checks.Commands"    , ShellCheck.Checks.Commands.runTests)
      , ("Checks.ControlFlow" , ShellCheck.Checks.ControlFlow.runTests)
      , ("Checks.Custom"      , ShellCheck.Checks.Custom.runTests)
      , ("Checks.ShellSupport", ShellCheck.Checks.ShellSupport.runTests)
      , ("Fixer"              , ShellCheck.Fixer.runTests)
      , ("Formatter.Diff"     , ShellCheck.Formatter.Diff.runTests)
      , ("Parser"             , ShellCheck.Parser.runTests)
      ]
