{-# LANGUAGE OverloadedStrings #-}
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

import Data.Aeson
import Data.IORef
import Data.Monoid
import GHC.Exts
import System.IO
import qualified Data.ByteString.Lazy.Char8 as BL

format = do
    ref <- newIORef []
    return Formatter {
        header = return (),
        onResult = collectResult ref,
        onFailure = outputError,
        footer = finish ref
    }

instance ToJSON (PositionedComment) where
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
      "message" .= cMessage c
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
    )

outputError file msg = hPutStrLn stderr $ file ++ ": " ++ msg
collectResult ref result _ =
    modifyIORef ref (\x -> crComments result ++ x)

finish ref = do
    list <- readIORef ref
    BL.putStrLn $ encode list

