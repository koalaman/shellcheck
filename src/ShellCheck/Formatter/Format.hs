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
module ShellCheck.Formatter.Format where

import ShellCheck.Data
import ShellCheck.Interface
import ShellCheck.Fixer
import Data.Array

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
    fix c =  realign c arr

-- Realign a Ranged from a tabstop of 8 to 1
realign :: Ranged a => a -> Array Int String -> a
realign range ls =
    let startColumn = realignColumn lineNo colNo range
        endColumn = realignColumn endLineNo endColNo range
        startPosition = (start range) { posColumn = startColumn }
        endPosition = (end range) { posColumn = endColumn } in
    setRange (startPosition, endPosition) range
  where
    realignColumn lineNo colNo c =
      if lineNo c > 0 && lineNo c <= fromIntegral (length ls)
      then real (ls ! fromIntegral (lineNo c)) 0 0 (colNo c)
      else colNo c
    real _ r v target | target <= v = r
    -- hit this case at the end of line, and if we don't hit the target
    -- return real + (target - v)
    real [] r v target = r + (target - v)
    real ('\t':rest) r v target = real rest (r+1) (v + 8 - (v `mod` 8)) target
    real (_:rest) r v target = real rest (r+1) (v+1) target
    lineNo = posLine . start
    endLineNo = posLine . end
    colNo = posColumn . start
    endColNo = posColumn . end

