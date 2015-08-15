{-
    Copyright 2012-2015 Vidar Holen

    This file is part of ShellCheck.
    http://www.vidarholen.net/contents/shellcheck

    ShellCheck is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    ShellCheck is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
module ShellCheck.Formatter.GCC (format) where

import ShellCheck.Interface
import ShellCheck.Formatter.Format

import Data.List
import GHC.Exts
import System.IO

format :: IO Formatter
format = return Formatter {
    header = return (),
    footer = return (),
    onFailure = outputError,
    onResult = outputResult
}

outputError file error = hPutStrLn stderr $ file ++ ": " ++ error

outputResult result contents = do
    let comments = makeNonVirtual (crComments result) contents
    mapM_ (putStrLn . formatComment (crFilename result)) comments

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
