{-
    Copyright 2019 Austin Voecks

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
module ShellCheck.Formatter.Quiet (format) where

import ShellCheck.Interface
import ShellCheck.Formatter.Format

import Control.Monad
import Data.IORef
import System.Exit

format :: FormatterOptions -> IO Formatter
format options =
    return Formatter {
        header = return (),
        footer = return (),
        onFailure = \ _ _ -> exitFailure,
        onResult  = \ result _ -> unless (null $ crComments result) exitFailure
    }
