{-# LANGUAGE OverloadedStrings #-}
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
module ShellCheck.Formatter.JSON1 (format) where

import ShellCheck.Interface
import ShellCheck.Formatter.Format

import Data.Aeson
import Data.IORef
import Data.Monoid
import GHC.Exts
import System.IO
import qualified Data.ByteString.Lazy.Char8 as BL

format :: IO Formatter
format = do
    ref <- newIORef []
    return Formatter {
        header = return (),
        onResult = collectResult ref,
        onFailure = outputError,
        footer = finish ref
    }

data Json1Output = Json1Output {
    comments :: [PositionedComment]
    }

instance ToJSON Json1Output where
    toJSON result = object [
        "comments" .= comments result
        ]
    toEncoding result = pairs (
        "comments" .= comments result
        )

instance ToJSON Replacement where
    toJSON replacement =
        let start = repStartPos replacement
            end = repEndPos replacement
            str = repString replacement in
        object [
          "precedence" .= repPrecedence replacement,
          "insertionPoint"  .=
            case repInsertionPoint replacement of
                InsertBefore -> "beforeStart" :: String
                InsertAfter  -> "afterEnd",
          "line" .= posLine start,
          "column" .= posColumn start,
          "endLine" .= posLine end,
          "endColumn" .= posColumn end,
          "replacement" .= str
        ]

instance ToJSON PositionedComment where
  toJSON comment =
    let start = pcStartPos comment
        end = pcEndPos comment
        c = pcComment comment in
    object [
      "file" .= posFile start,
      "line" .= posLine start,
      "endLine" .= posLine end,
      "column" .= posColumn start,
      "endColumn" .= posColumn end,
      "level" .= severityText comment,
      "code" .= cCode c,
      "message" .= cMessage c,
      "fix" .= pcFix comment
    ]

  toEncoding comment =
    let start = pcStartPos comment
        end = pcEndPos comment
        c = pcComment comment in
    pairs (
         "file" .= posFile start
      <> "line" .= posLine start
      <> "endLine" .= posLine end
      <> "column" .= posColumn start
      <> "endColumn" .= posColumn end
      <> "level" .= severityText comment
      <> "code" .= cCode c
      <> "message" .= cMessage c
      <> "fix" .= pcFix comment
    )

instance ToJSON Fix where
    toJSON fix = object [
        "replacements" .= fixReplacements fix
        ]

outputError file msg = hPutStrLn stderr $ file ++ ": " ++ msg

collectResult ref cr sys = mapM_ f groups
  where
    comments = crComments cr
    groups = groupWith sourceFile comments
    f :: [PositionedComment] -> IO ()
    f group = do
        let filename = sourceFile (head group)
        result <- siReadFile sys filename
        let contents = either (const "") id result
        let comments' = makeNonVirtual comments contents
        modifyIORef ref (\x -> comments' ++ x)

finish ref = do
    list <- readIORef ref
    BL.putStrLn $ encode $ Json1Output { comments = list }
