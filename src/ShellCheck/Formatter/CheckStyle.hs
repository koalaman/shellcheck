{-
    Copyright 2012-2015 Vidar Holen

    This file is part of ShellCheck.
    https://www.shellcheck.net

    ShellCheck is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    ShellCheck is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}
module ShellCheck.Formatter.CheckStyle (format) where

import ShellCheck.Interface
import ShellCheck.Formatter.Format

import Data.Char
import Data.List
import GHC.Exts
import System.IO

format :: IO Formatter
format = return Formatter {
    header = do
        putStrLn "<?xml version='1.0' encoding='UTF-8'?>"
        putStrLn "<checkstyle version='4.3'>",

    onFailure = outputError,
    onResult = outputResults,

    footer = putStrLn "</checkstyle>"
}

outputResults cr sys =
    if null comments
    then outputFile (crFilename cr) "" []
    else mapM_ outputGroup fileGroups
  where
    comments = crComments cr
    fileGroups = groupWith sourceFile comments
    outputGroup group = do
        let filename = sourceFile (head group)
        result <- (siReadFile sys) filename
        let contents = either (const "") id result
        outputFile filename contents group

outputFile filename contents warnings = do
    let comments = makeNonVirtual warnings contents
    putStrLn . formatFile filename $ comments

formatFile name comments = concat [
    "<file ", attr "name" name, ">\n",
        concatMap formatComment comments,
    "</file>"
    ]

formatComment c = concat [
    "<error ",
    attr "line" $ show . lineNo $ c,
    attr "column" $ show . colNo $ c,
    attr "severity" . severity $ severityText c,
    attr "message" $ messageText c,
    attr "source" $ "ShellCheck.SC" ++ show (codeNo c),
    "/>\n"
    ]

outputError file error = putStrLn $ concat [
    "<file ", attr "name" file, ">\n",
    "<error ",
        attr "line" "1",
        attr "column" "1",
        attr "severity" "error",
        attr "message" error,
        attr "source" "ShellCheck",
    "/>\n",
    "</file>"
    ]


attr s v = concat [ s, "='", escape v, "' " ]
escape = concatMap escape'
escape' c = if isOk c then [c] else "&#" ++ show (ord c) ++ ";"
isOk x = any ($x) [isAsciiUpper, isAsciiLower, isDigit, (`elem` " ./")]

severity "error" = "error"
severity "warning" = "warning"
severity _ = "info"
