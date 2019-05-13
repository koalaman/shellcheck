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
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module ShellCheck.Interface
    (
    SystemInterface(..)
    , CheckSpec(csFilename, csScript, csCheckSourced, csIncludedWarnings, csExcludedWarnings, csShellTypeOverride, csMinSeverity, csIgnoreRC, csOptionalChecks)
    , CheckResult(crFilename, crComments)
    , ParseSpec(psFilename, psScript, psCheckSourced, psIgnoreRC, psShellTypeOverride)
    , ParseResult(prComments, prTokenPositions, prRoot)
    , AnalysisSpec(asScript, asShellType, asFallbackShell, asExecutionMode, asCheckSourced, asTokenPositions, asOptionalChecks)
    , AnalysisResult(arComments)
    , FormatterOptions(foColorOption, foWikiLinkCount)
    , Shell(Ksh, Sh, Bash, Dash)
    , ExecutionMode(Executed, Sourced)
    , ErrorMessage
    , Code
    , Severity(ErrorC, WarningC, InfoC, StyleC)
    , Position(posFile, posLine, posColumn)
    , Comment(cSeverity, cCode, cMessage)
    , PositionedComment(pcStartPos , pcEndPos , pcComment, pcFix)
    , ColorOption(ColorAuto, ColorAlways, ColorNever)
    , TokenComment(tcId, tcComment, tcFix)
    , emptyCheckResult
    , newParseResult
    , newAnalysisSpec
    , newAnalysisResult
    , newFormatterOptions
    , newPosition
    , newTokenComment
    , mockedSystemInterface
    , mockRcFile
    , newParseSpec
    , emptyCheckSpec
    , newPositionedComment
    , newComment
    , Fix(fixReplacements)
    , newFix
    , InsertionPoint(InsertBefore, InsertAfter)
    , Replacement(repStartPos, repEndPos, repString, repPrecedence, repInsertionPoint)
    , newReplacement
    , CheckDescription(cdName, cdDescription, cdPositive, cdNegative)
    , newCheckDescription
    ) where

import ShellCheck.AST

import Control.DeepSeq
import Control.Monad.Identity
import Data.List
import Data.Monoid
import Data.Ord
import Data.Semigroup
import GHC.Generics (Generic)
import qualified Data.Map as Map


data SystemInterface m = SystemInterface {
    -- Read a file by filename, or return an error
    siReadFile :: String -> m (Either ErrorMessage String),
    -- Given:
    --   the current script,
    --   a list of source-path annotations in effect,
    --   and a sourced file,
    --   find the sourced file
    siFindSource :: String -> [String] -> String -> m FilePath,
    -- Get the configuration file (name, contents) for a filename
    siGetConfig :: String -> m (Maybe (FilePath, String))
}

-- ShellCheck input and output
data CheckSpec = CheckSpec {
    csFilename :: String,
    csScript :: String,
    csCheckSourced :: Bool,
    csIgnoreRC :: Bool,
    csExcludedWarnings :: [Integer],
    csIncludedWarnings :: Maybe [Integer],
    csShellTypeOverride :: Maybe Shell,
    csMinSeverity :: Severity,
    csOptionalChecks :: [String]
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
    csIgnoreRC = False,
    csExcludedWarnings = [],
    csIncludedWarnings = Nothing,
    csShellTypeOverride = Nothing,
    csMinSeverity = StyleC,
    csOptionalChecks = []
}

newParseSpec :: ParseSpec
newParseSpec = ParseSpec {
    psFilename = "",
    psScript = "",
    psCheckSourced = False,
    psIgnoreRC = False,
    psShellTypeOverride = Nothing
}

-- Parser input and output
data ParseSpec = ParseSpec {
    psFilename :: String,
    psScript :: String,
    psCheckSourced :: Bool,
    psIgnoreRC :: Bool,
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
    asFallbackShell :: Maybe Shell,
    asExecutionMode :: ExecutionMode,
    asCheckSourced :: Bool,
    asOptionalChecks :: [String],
    asTokenPositions :: Map.Map Id (Position, Position)
}

newAnalysisSpec token = AnalysisSpec {
    asScript = token,
    asShellType = Nothing,
    asFallbackShell = Nothing,
    asExecutionMode = Executed,
    asCheckSourced = False,
    asOptionalChecks = [],
    asTokenPositions = Map.empty
}

newtype AnalysisResult = AnalysisResult {
    arComments :: [TokenComment]
}

newAnalysisResult = AnalysisResult {
    arComments = []
}

-- Formatter options
data FormatterOptions = FormatterOptions {
    foColorOption :: ColorOption,
    foWikiLinkCount :: Integer
}

newFormatterOptions = FormatterOptions {
    foColorOption = ColorAuto,
    foWikiLinkCount = 3
}

data CheckDescription = CheckDescription {
    cdName :: String,
    cdDescription :: String,
    cdPositive :: String,
    cdNegative :: String
    }

newCheckDescription = CheckDescription {
    cdName = "",
    cdDescription = "",
    cdPositive = "",
    cdNegative = ""
    }

-- Supporting data types
data Shell = Ksh | Sh | Bash | Dash deriving (Show, Eq)
data ExecutionMode = Executed | Sourced deriving (Show, Eq)

type ErrorMessage = String
type Code = Integer

data Severity = ErrorC | WarningC | InfoC | StyleC
    deriving (Show, Eq, Ord, Generic, NFData)
data Position = Position {
    posFile :: String,    -- Filename
    posLine :: Integer,   -- 1 based source line
    posColumn :: Integer  -- 1 based source column, where tabs are 8
} deriving (Show, Eq, Generic, NFData, Ord)

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
} deriving (Show, Eq, Generic, NFData)

newComment :: Comment
newComment = Comment {
    cSeverity = StyleC,
    cCode     = 0,
    cMessage  = ""
}

-- only support single line for now
data Replacement = Replacement {
    repStartPos :: Position,
    repEndPos :: Position,
    repString :: String,
    -- Order in which the replacements should happen: highest precedence first.
    repPrecedence :: Int,
    -- Whether to insert immediately before or immediately after the specified region.
    repInsertionPoint :: InsertionPoint
} deriving (Show, Eq, Generic, NFData)

data InsertionPoint = InsertBefore | InsertAfter
    deriving (Show, Eq, Generic, NFData)

instance Ord Replacement where
    compare r1 r2 = (repStartPos r1) `compare` (repStartPos r2)

newReplacement = Replacement {
    repStartPos = newPosition,
    repEndPos = newPosition,
    repString = "",
    repPrecedence = 1,
    repInsertionPoint = InsertAfter
}

data Fix = Fix {
    fixReplacements :: [Replacement]
} deriving (Show, Eq, Generic, NFData)

newFix = Fix {
    fixReplacements = []
}

data PositionedComment = PositionedComment {
    pcStartPos :: Position,
    pcEndPos   :: Position,
    pcComment  :: Comment,
    pcFix      :: Maybe Fix
} deriving (Show, Eq, Generic, NFData)

newPositionedComment :: PositionedComment
newPositionedComment = PositionedComment {
    pcStartPos = newPosition,
    pcEndPos   = newPosition,
    pcComment  = newComment,
    pcFix      = Nothing
}

data TokenComment = TokenComment {
    tcId :: Id,
    tcComment :: Comment,
    tcFix :: Maybe Fix
} deriving (Show, Eq, Generic, NFData)

newTokenComment = TokenComment {
    tcId = Id 0,
    tcComment = newComment,
    tcFix = Nothing
}

data ColorOption =
    ColorAuto
    | ColorAlways
    | ColorNever
  deriving (Ord, Eq, Show)

-- For testing
mockedSystemInterface :: [(String, String)] -> SystemInterface Identity
mockedSystemInterface files = SystemInterface {
    siReadFile = rf,
    siFindSource = fs,
    siGetConfig = const $ return Nothing
}
  where
    rf file =
        case filter ((== file) . fst) files of
            [] -> return $ Left "File not included in mock."
            [(_, contents)] -> return $ Right contents
    fs _ _ file = return file

mockRcFile rcfile mock = mock {
    siGetConfig = const . return $ Just (".shellcheckrc", rcfile)
}

