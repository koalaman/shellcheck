{-
    Copyright 2022 Vidar Holen

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

-- Generic basic utility functions
module ShellCheck.Prelude where

import Data.Semigroup


-- Get element 0 or a default. Like `head` but safe.
headOrDefault _ (a:_) = a
headOrDefault def _   = def

-- Get the last element or a default. Like `last` but safe.
lastOrDefault def [] = def
lastOrDefault _ list = last list

--- Get element n of a list, or Nothing. Like `!!` but safe.
(!!!) list i =
    case drop i list of
        []    -> Nothing
        (r:_) -> Just r


-- Like mconcat but for Semigroups
sconcat1 :: (Semigroup t) => [t] -> t
sconcat1 [x] = x
sconcat1 (x:xs) = x <> sconcat1 xs

sconcatOrDefault def [] = def
sconcatOrDefault _ list = sconcat1 list

-- For more actionable "impossible" errors
pleaseReport str = "ShellCheck internal error, please report: " ++ str
