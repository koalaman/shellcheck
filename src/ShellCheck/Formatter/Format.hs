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
module ShellCheck.Formatter.Format where

import ShellCheck.Data
import ShellCheck.Interface
import ShellCheck.Fixer

import Control.Monad
import Data.Array
import Data.List
import System.IO
import System.Info

-- A formatter that carries along an arbitrary piece of data
data Formatter = Formatter {
    header ::  IO (),
    onResult :: CheckResult -> SystemInterface IO -> IO (),
    onFailure :: FilePath -> ErrorMessage -> IO (),
    footer :: IO ()
}

sourceFile = posFile . pcStartPos
lineNo = posLine . pcStartPos
endLineNo = posLine . pcEndPos
colNo  = posColumn . pcStartPos
endColNo = posColumn . pcEndPos
codeNo = cCode . pcComment
messageText = cMessage . pcComment

severityText :: PositionedComment -> String
severityText pc =
    case cSeverity (pcComment pc) of
        ErrorC   -> "error"
        WarningC -> "warning"
        InfoC    -> "info"
        StyleC   -> "style"

-- Realign comments from a tabstop of 8 to 1
makeNonVirtual comments contents =
    map fix comments
  where
    list = lines contents
    arr = listArray (1, length list) list
    untabbedFix f = newFix {
      fixReplacements = map (\r -> removeTabStops r arr) (fixReplacements f)
    }
    fix c = (removeTabStops c arr) {
      pcFix = fmap untabbedFix (pcFix c)
    }


shouldOutputColor :: ColorOption -> IO Bool
shouldOutputColor colorOption = do
    term <- hIsTerminalDevice stdout
    let windows = "mingw" `isPrefixOf` os
    let isUsableTty = term && not windows
    let useColor = case colorOption of
                       ColorAlways -> True
                       ColorNever -> False
                       ColorAuto -> isUsableTty
    return useColor
