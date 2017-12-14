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
module ShellCheck.Formatter.JSON (format) where

import ShellCheck.Interface
import ShellCheck.Formatter.Format

import Data.IORef
import GHC.Exts
import System.IO
import Text.JSON

format = do
    ref <- newIORef []
    return Formatter {
        header = return (),
        onResult = collectResult ref,
        onFailure = outputError,
        footer = finish ref
    }

instance JSON (PositionedComment) where
  showJSON comment@(PositionedComment start end (Comment level code string)) = makeObj [
      ("file", showJSON $ posFile start),
      ("line", showJSON $ posLine start),
      ("endLine", showJSON $ posLine end),
      ("column", showJSON $ posColumn start),
      ("endColumn", showJSON $ posColumn end),
      ("level", showJSON $ severityText comment),
      ("code", showJSON code),
      ("message", showJSON string)
      ]

  readJSON = undefined

outputError file msg = hPutStrLn stderr $ file ++ ": " ++ msg
collectResult ref result _ =
    modifyIORef ref (\x -> crComments result ++ x)

finish ref = do
    list <- readIORef ref
    putStrLn $ encodeStrict list

