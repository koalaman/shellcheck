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
module ShellCheck.Interface where

import ShellCheck.AST
import Control.Monad.Identity
import qualified Data.Map as Map


newtype SystemInterface m = SystemInterface {
    -- Read a file by filename, or return an error
    siReadFile :: String -> m (Either ErrorMessage String)
}

-- ShellCheck input and output
data CheckSpec = CheckSpec {
    csFilename :: String,
    csScript :: String,
    csCheckSourced :: Bool,
    csExcludedWarnings :: [Integer],
    csShellTypeOverride :: Maybe Shell
} deriving (Show, Eq)

data CheckResult = CheckResult {
    crFilename :: String,
    crComments :: [PositionedComment]
} deriving (Show, Eq)

emptyCheckSpec :: CheckSpec
emptyCheckSpec = CheckSpec {
    csFilename = "",
    csScript = "",
    csCheckSourced = False,
    csExcludedWarnings = [],
    csShellTypeOverride = Nothing
}

-- Parser input and output
data ParseSpec = ParseSpec {
    psFilename :: String,
    psScript :: String,
    psCheckSourced :: Bool
} deriving (Show, Eq)

data ParseResult = ParseResult {
    prComments :: [PositionedComment],
    prTokenPositions :: Map.Map Id Position,
    prRoot :: Maybe Token
} deriving (Show, Eq)

-- Analyzer input and output
data AnalysisSpec = AnalysisSpec {
    asScript :: Token,
    asShellType :: Maybe Shell,
    asExecutionMode :: ExecutionMode,
    asCheckSourced :: Bool
}

newtype AnalysisResult = AnalysisResult {
    arComments :: [TokenComment]
}


-- Formatter options
newtype FormatterOptions = FormatterOptions {
    foColorOption :: ColorOption
}


-- Supporting data types
data Shell = Ksh | Sh | Bash | Dash deriving (Show, Eq)
data ExecutionMode = Executed | Sourced deriving (Show, Eq)

type ErrorMessage = String
type Code = Integer

data Severity = ErrorC | WarningC | InfoC | StyleC deriving (Show, Eq, Ord)
data Position = Position {
    posFile :: String,    -- Filename
    posLine :: Integer,   -- 1 based source line
    posColumn :: Integer  -- 1 based source column, where tabs are 8
} deriving (Show, Eq)

data Comment = Comment Severity Code String deriving (Show, Eq)
data PositionedComment = PositionedComment Position Position Comment deriving (Show, Eq)
data TokenComment = TokenComment Id Comment deriving (Show, Eq)

data ColorOption =
    ColorAuto
    | ColorAlways
    | ColorNever
  deriving (Ord, Eq, Show)

-- For testing
mockedSystemInterface :: [(String, String)] -> SystemInterface Identity
mockedSystemInterface files = SystemInterface {
    siReadFile = rf
}
  where
    rf file =
        case filter ((== file) . fst) files of
            [] -> return $ Left "File not included in mock."
            [(_, contents)] -> return $ Right contents

