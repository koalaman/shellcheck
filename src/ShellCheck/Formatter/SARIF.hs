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
module ShellCheck.Formatter.SARIF (format) where

import ShellCheck.Data
import ShellCheck.Interface
import ShellCheck.Formatter.Format

import Control.DeepSeq
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

data SarifOutput = SarifOutput {
    comments :: [PositionedComment]
    }

instance ToJSON SarifOutput where
    toJSON result = object [
        "version" .= ("2.1.0" :: String),
        "$schema" .= ("http://json.schemastore.org/sarif-2.1.0" :: String),
        "runs" .= [object [
            "tool" .= object [
                "driver" .= object [
                    "name" .= ("ShellCheck" :: String),
                    "semanticVersion" .= shellcheckVersion,
                    "informationUri" .= shellcheckWebsite
                    ]
                ],
            "results" .= comments result
            ]]
        ]

instance ToJSON Replacement where
    toJSON replacement =
        let start = repStartPos replacement
            end = repEndPos replacement in
        object [
            "artifactLocation" .= object [
                "uri" .= posFile start
                ],
            "replacements" .= [object [
                "deletedRegion" .= object [
                    "startLine" .= posLine start,
                    "startColumn" .= posColumn start,
                    "endLine" .= posLine end,
                    "endColumn" .= posColumn end
                    ],
                "insertedContent" .= object [
                    "text" .= repString replacement
                    ]
                ]]
            ]

instance ToJSON PositionedComment where
  toJSON comment =
    let start = pcStartPos comment
        end = pcEndPos comment
        c = pcComment comment
        replacements = maybe [] fixReplacements (pcFix comment) in
    object [
        "ruleId" .= code (cCode c),
        "level" .= level (cSeverity c),
        "message" .= object [
            "text" .= cMessage c
        ],
        "locations" .= [object [
            "physicalLocation" .= object [
                "artifactLocation" .= object [
                    "uri" .= posFile start
                ],
                "region" .= object [
                    "startLine" .= posLine start,
                    "startColumn" .= posColumn start,
                    "endLine" .= posLine end,
                    "endColumn" .= posColumn end
                ]
            ]
        ]],
        "fixes" .= [object [
            "artifactChanges" .= replacements
            ] | not (null replacements)]
    ]

code num = "SC" ++ show num

level severity = case severity of
    ErrorC -> "error" :: String
    WarningC -> "warning"
    InfoC -> "note"
    StyleC -> "none"

outputError file msg = hPutStrLn stderr $ file ++ ": " ++ msg

collectResult ref cr sys = mapM_ f groups
  where
    comments = crComments cr
    groups = groupWith sourceFile comments
    f :: [PositionedComment] -> IO ()
    f group = do
        let filename = sourceFile (head group)
        result <- siReadFile sys (Just True) filename
        let contents = either (const "") id result
        let comments' = makeNonVirtual comments contents
        deepseq comments' $ modifyIORef ref (\x -> comments' ++ x)

finish ref = do
    list <- readIORef ref
    BL.putStrLn $ encode $ SarifOutput { comments = list }
