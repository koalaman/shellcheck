{-
    Copyright 2012-2019 Vidar Holen

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
module ShellCheck.Formatter.GCC (format) where

import ShellCheck.Interface
import ShellCheck.Formatter.Format

import Data.List
import System.IO
import qualified Data.List.NonEmpty as NE

format :: IO Formatter
format = return Formatter {
    header = return (),
    footer = return (),
    onFailure = outputError,
    onResult = outputAll
}

outputError file error = hPutStrLn stderr $ file ++ ": " ++ error

outputAll cr sys = mapM_ f (commentsByFile comments)
  where
    comments = crComments cr
    f :: NE.NonEmpty PositionedComment -> IO ()
    f group = do
        (filename, fileComments, contents) <- readCommentsForFile sys group
        outputResult filename contents fileComments

outputResult filename contents warnings = do
    let comments = makeNonVirtual warnings contents
    mapM_ (putStrLn . formatComment filename) comments

formatComment filename c = concat [
    filename, ":",
    show $ lineNo c, ":",
    show $ colNo c, ": ",
    case severityText c of
        "error" -> "error"
        "warning" -> "warning"
        _ -> "note",
    ": ",
    concat . lines $ messageText c,
    " [SC", show $ codeNo c, "]"
  ]
