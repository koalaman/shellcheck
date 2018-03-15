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
{-# LANGUAGE FlexibleContexts #-}

-- Basically Text.Regex based on regex-tdfa instead of the buggy regex-posix.
module ShellCheck.Regex where

import Data.List
import Data.Maybe
import Control.Monad
import Text.Regex.TDFA

-- Precompile the regex
mkRegex :: String -> Regex
mkRegex str =
    let make :: RegexMaker Regex CompOption ExecOption String => String -> Regex
        make = makeRegex
    in
        make str

-- Does the regex match?
matches :: String -> Regex -> Bool
matches = flip match

-- Get all subgroups of the first match
matchRegex :: Regex -> String -> Maybe [String]
matchRegex re str = do
    (_, _, _, groups) <- matchM re str :: Maybe (String,String,String,[String])
    return groups

-- Get all full matches
matchAllStrings :: Regex -> String -> [String]
matchAllStrings re = unfoldr f
  where
    f :: String -> Maybe (String, String)
    f str = do
        (_, match, rest, _) <- matchM re str :: Maybe (String, String, String, [String])
        return (match, rest)

-- Get all subgroups from all matches
matchAllSubgroups :: Regex -> String -> [[String]]
matchAllSubgroups re = unfoldr f
  where
    f :: String -> Maybe ([String], String)
    f str = do
        (_, _, rest, groups) <- matchM re str :: Maybe (String, String, String, [String])
        return (groups, rest)

-- Replace regex in input with string
subRegex :: Regex -> String -> String -> String
subRegex re input replacement = f input
  where
    f str = fromMaybe str $ do
        (before, match, after) <- matchM re str :: Maybe (String, String, String)
        when (null match) $ error ("Internal error: substituted empty in " ++ str)
        return $ before ++ replacement ++ f after

-- Split a string based on a regex.
splitOn :: String -> Regex -> [String]
splitOn input re =
    case matchM re input :: Maybe (String, String, String) of
        Just (before, match, after) -> before : after `splitOn` re
        Nothing -> [input]
