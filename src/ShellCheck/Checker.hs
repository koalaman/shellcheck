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
module ShellCheck.Checker (checkScript) where

import ShellCheck.Interface
import ShellCheck.Parser
import ShellCheck.Analyzer

import Data.Either
import Data.Functor
import Data.List
import Data.Maybe
import Data.Ord
import Control.Monad.Identity
import qualified Data.Map as Map
import qualified System.IO
import Prelude hiding (readFile)
import Control.Monad

tokenToPosition startMap t = fromMaybe fail $ do
    span <- Map.lookup (tcId t) startMap
    return $ newPositionedComment {
        pcStartPos = fst span,
        pcEndPos = snd span,
        pcComment = tcComment t,
        pcFix = tcFix t
    }
  where
    fail = error "Internal shellcheck error: id doesn't exist. Please report!"

checkScript :: Monad m => SystemInterface m -> CheckSpec -> m CheckResult
checkScript sys spec = do
    results <- checkScript (csScript spec)
    return emptyCheckResult {
        crFilename = csFilename spec,
        crComments = results
    }
  where
    checkScript contents = do
        result <- parseScript sys newParseSpec {
            psFilename = csFilename spec,
            psScript = contents,
            psCheckSourced = csCheckSourced spec,
            psShellTypeOverride = csShellTypeOverride spec
        }
        let parseMessages = prComments result
        let tokenPositions = prTokenPositions result
        let analysisSpec root =
                as {
                    asScript = root,
                    asShellType = csShellTypeOverride spec,
                    asCheckSourced = csCheckSourced spec,
                    asExecutionMode = Executed,
                    asTokenPositions = tokenPositions
                } where as = newAnalysisSpec root
        let analysisMessages =
                fromMaybe [] $
                    (arComments . analyzeScript . analysisSpec)
                        <$> prRoot result
        let translator = tokenToPosition tokenPositions
        return . nub . sortMessages . filter shouldInclude $
            (parseMessages ++ map translator analysisMessages)

    shouldInclude pc =
        let code     = cCode (pcComment pc)
            severity = cSeverity (pcComment pc)
        in
            code `notElem` csExcludedWarnings spec &&
            severity <= csMinSeverity spec

    sortMessages = sortBy (comparing order)
    order pc =
        let pos = pcStartPos pc
            comment = pcComment pc in
        (posFile pos,
         posLine pos,
         posColumn pos,
         cSeverity comment,
         cCode comment,
         cMessage comment)
    getPosition = pcStartPos


getErrors sys spec =
    sort . map getCode . crComments $
        runIdentity (checkScript sys spec)
  where
    getCode = cCode . pcComment

check = checkWithIncludes []

checkWithSpec includes =
    getErrors (mockedSystemInterface includes)

checkWithIncludes includes src =
    checkWithSpec includes emptyCheckSpec {
        csScript = src,
        csExcludedWarnings = [2148]
    }

checkRecursive includes src =
    checkWithSpec includes emptyCheckSpec {
        csScript = src,
        csExcludedWarnings = [2148],
        csCheckSourced = True
    }

-- | Dummy binding for doctest to run
--
-- >>> check "echo \"$12\""
-- [1037]
--
-- >>> check "#shellcheck disable=SC1037\necho \"$12\""
-- []
--
-- >>> check "#shellcheck disable=SC1037\n#lol\necho \"$12\""
-- []
--
-- >>> check "echo $1"
-- [2086]
--
-- >>> check "#shellcheck disable=SC2086\necho $1"
-- []
--
-- >>> check "#shellcheck disable=SC2086\n#lol\necho $1"
-- []
--
-- >>> :{
--  getErrors
--     (mockedSystemInterface [])
--     emptyCheckSpec {
--         csScript = "echo $1",
--         csExcludedWarnings = [2148, 2086]
--     }
-- :}
-- []
--
-- >>> :{
--   getErrors
--     (mockedSystemInterface [])
--     emptyCheckSpec {
--         csScript = "echo \"$10\"",
--         csExcludedWarnings = [2148, 1037]
--     }
-- :}
-- []
--
-- >>> check "#!/usr/bin/python\ntrue $1\n"
-- [1071]
--
-- >>> :{
--   getErrors
--     (mockedSystemInterface [])
--     emptyCheckSpec {
--         csScript = "#!/usr/bin/python\ntrue\n",
--         csShellTypeOverride = Just Sh
--     }
-- :}
-- []
--
-- >>> check "#!/usr/bin/python\n# shellcheck shell=sh\ntrue\n"
-- []
--
-- >>> check "source /dev/null"
-- []
--
-- >>> check "source lol; echo \"$bar\""
-- [1091,2154]
--
-- >>> checkWithIncludes [("lib", "bar=1")] "source lib; echo \"$bar\""
-- []
--
-- >>> checkWithIncludes [("lib", "bar=1")] ". lib; echo \"$bar\""
-- []
--
-- >>> checkWithIncludes  [("lib", "source lib")] "source lib"
-- []
--
-- >>> checkWithIncludes [("lib", "for f; do")] "source lib; echo $1"
-- [1094,2086]
--
-- >>> checkWithIncludes [("lib", "")] ". \"$1\""
-- [1090]
--
-- >>> checkWithIncludes [("lib", "")] "source ~/foo"
-- [1090]
--
-- >>> checkWithIncludes [("lib", "")] "#shellcheck source=lib\n. \"$1\""
-- []
--
-- >>> checkRecursive [("lib", "echo $1")] "source lib"
-- [2086]
--
-- >>> checkRecursive [("lib", "echo \"$10\"")] "source lib"
-- [1037]
--
-- >>> checkWithIncludes [("foo", "source bar"), ("bar", "baz=3")] "#shellcheck source=foo\n. \"$1\"; echo \"$baz\""
-- []
--
-- >>> check "#!/bin/sh\necho $1"
-- [2086]
--
-- >>> check "#!/bin/sh\n# shellcheck disable=2086\necho $1"
-- []
--
-- >>> check "#!/bin/sh\n# shellcheck disable=2086\ntrue\necho $1"
-- []
--
-- >>> check "#!/bin/sh\n#unrelated\n# shellcheck disable=2086\ntrue\necho $1"
-- []
--
-- >>> check "#!/bin/sh\n# shellcheck disable=2086\n#unrelated\ntrue\necho $1"
-- []
--
-- >>> check "#!/bin/sh\n\n\n\n#shellcheck disable=2086\ntrue\necho $1"
-- []
--
-- >>> check "#shellcheck shell=sh\n#unrelated\n#shellcheck disable=2086\ntrue\necho $1"
-- []
--
-- >>> check "#!/bin/sh\n# shellcheck disable=2086\n#unrelated\ntrue\necho $1"
-- []
--
-- check "true\n[ $? == 0 ] && echo $1"
-- [2086, 2181]
--
-- check "# Disable $? warning\n#shellcheck disable=SC2181\n# Disable quoting warning\n#shellcheck disable=2086\ntrue\n[ $? == 0 ] && echo $1"
-- []
--
-- >>> 2039 `elem` checkWithIncludes [("./saywhat.sh", "echo foo")] "#!/bin/sh\nsource ./saywhat.sh"
-- True
--
-- >>> check "fun() {\n# shellcheck disable=SC2188\n> /dev/null\n}\n"
-- []
doctests :: ()
doctests = ()
