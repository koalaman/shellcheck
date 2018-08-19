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
module ShellCheck.Interface
    (
    SystemInterface(..)
    , CheckSpec(csFilename, csScript, csCheckSourced, csExcludedWarnings, csShellTypeOverride, csMinSeverity)
    , CheckResult(crFilename, crComments)
    , ParseSpec(psFilename, psScript, psCheckSourced, psShellTypeOverride)
    , ParseResult(prComments, prTokenPositions, prRoot)
    , AnalysisSpec(asScript, asShellType, asExecutionMode, asCheckSourced)
    , AnalysisResult(arComments)
    , FormatterOptions(foColorOption)
    , Shell(Ksh, Sh, Bash, Dash)
    , ExecutionMode(Executed, Sourced)
    , ErrorMessage
    , Code
    , Severity(ErrorC, WarningC, InfoC, StyleC)
    , Position(posFile, posLine, posColumn)
    , Comment(cSeverity, cCode, cMessage)
    , PositionedComment(pcStartPos , pcEndPos , pcComment)
    , ColorOption(ColorAuto, ColorAlways, ColorNever)
    , TokenComment(tcId, tcComment)
    , emptyCheckResult
    , newParseResult
    , newAnalysisSpec
    , newAnalysisResult
    , newFormatterOptions
    , newPosition
    , newTokenComment
    , mockedSystemInterface
    , newParseSpec
    , emptyCheckSpec
    , newPositionedComment
    , newComment
    ) where

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
    csShellTypeOverride :: Maybe Shell,
    csMinSeverity :: Severity
} deriving (Show, Eq)

data CheckResult = CheckResult {
    crFilename :: String,
    crComments :: [PositionedComment]
} deriving (Show, Eq)

emptyCheckResult :: CheckResult
emptyCheckResult = CheckResult {
    crFilename = "",
    crComments = []
}

emptyCheckSpec :: CheckSpec
emptyCheckSpec = CheckSpec {
    csFilename = "",
    csScript = "",
    csCheckSourced = False,
    csExcludedWarnings = [],
    csShellTypeOverride = Nothing,
    csMinSeverity = StyleC
}

newParseSpec :: ParseSpec
newParseSpec = ParseSpec {
    psFilename = "",
    psScript = "",
    psCheckSourced = False,
    psShellTypeOverride = Nothing
}

-- Parser input and output
data ParseSpec = ParseSpec {
    psFilename :: String,
    psScript :: String,
    psCheckSourced :: Bool,
    psShellTypeOverride :: Maybe Shell
} deriving (Show, Eq)

data ParseResult = ParseResult {
    prComments :: [PositionedComment],
    prTokenPositions :: Map.Map Id (Position, Position),
    prRoot :: Maybe Token
} deriving (Show, Eq)

newParseResult :: ParseResult
newParseResult = ParseResult {
    prComments = [],
    prTokenPositions = Map.empty,
    prRoot = Nothing
}

-- Analyzer input and output
data AnalysisSpec = AnalysisSpec {
    asScript :: Token,
    asShellType :: Maybe Shell,
    asExecutionMode :: ExecutionMode,
    asCheckSourced :: Bool
}

newAnalysisSpec token = AnalysisSpec {
    asScript = token,
    asShellType = Nothing,
    asExecutionMode = Executed,
    asCheckSourced = False
}

newtype AnalysisResult = AnalysisResult {
    arComments :: [TokenComment]
}

newAnalysisResult = AnalysisResult {
    arComments = []
}

-- Formatter options
newtype FormatterOptions = FormatterOptions {
    foColorOption :: ColorOption
}

newFormatterOptions = FormatterOptions {
    foColorOption = ColorAuto
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

newPosition :: Position
newPosition = Position {
    posFile   = "",
    posLine   = 1,
    posColumn = 1
}

data Comment = Comment {
    cSeverity :: Severity,
    cCode     :: Code,
    cMessage  :: String
} deriving (Show, Eq)

newComment :: Comment
newComment = Comment {
    cSeverity = StyleC,
    cCode     = 0,
    cMessage  = ""
}

data PositionedComment = PositionedComment {
    pcStartPos :: Position,
    pcEndPos   :: Position,
    pcComment  :: Comment
} deriving (Show, Eq)

newPositionedComment :: PositionedComment
newPositionedComment = PositionedComment {
    pcStartPos = newPosition,
    pcEndPos   = newPosition,
    pcComment  = newComment
}

data TokenComment = TokenComment {
    tcId :: Id,
    tcComment :: Comment
} deriving (Show, Eq)

newTokenComment = TokenComment {
    tcId = Id 0,
    tcComment = newComment
}

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

