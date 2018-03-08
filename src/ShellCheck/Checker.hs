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
{-# LANGUAGE TemplateHaskell #-}
module ShellCheck.Checker (checkScript, ShellCheck.Checker.runTests) where

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

import Test.QuickCheck.All

tokenToPosition map (TokenComment id c) = fromMaybe fail $ do
    position <- Map.lookup id map
    return $ PositionedComment position position c
  where
    fail = error "Internal shellcheck error: id doesn't exist. Please report!"

checkScript :: Monad m => SystemInterface m -> CheckSpec -> m CheckResult
checkScript sys spec = do
    results <- checkScript (csScript spec)
    return CheckResult {
        crFilename = csFilename spec,
        crComments = results
    }
  where
    checkScript contents = do
        result <- parseScript sys ParseSpec {
            psFilename = csFilename spec,
            psScript = contents,
            psCheckSourced = csCheckSourced spec
        }
        let parseMessages = prComments result
        let analysisMessages =
                fromMaybe [] $
                    (arComments . analyzeScript . analysisSpec)
                        <$> prRoot result
        let translator = tokenToPosition (prTokenPositions result)
        return . nub . sortMessages . filter shouldInclude $
            (parseMessages ++ map translator analysisMessages)

    shouldInclude (PositionedComment _ _ (Comment _ code _)) =
        code `notElem` csExcludedWarnings spec

    sortMessages = sortBy (comparing order)
    order (PositionedComment pos _ (Comment severity code message)) =
        (posFile pos, posLine pos, posColumn pos, severity, code, message)
    getPosition (PositionedComment pos _ _) = pos

    analysisSpec root =
        AnalysisSpec {
            asScript = root,
            asShellType = csShellTypeOverride spec,
            asCheckSourced = csCheckSourced spec,
            asExecutionMode = Executed
         }

getErrors sys spec =
    sort . map getCode . crComments $
        runIdentity (checkScript sys spec)
  where
    getCode (PositionedComment _ _ (Comment _ code _)) = code

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

prop_findsParseIssue = check "echo \"$12\"" == [1037]

prop_commentDisablesParseIssue1 =
    null $ check "#shellcheck disable=SC1037\necho \"$12\""
prop_commentDisablesParseIssue2 =
    null $ check "#shellcheck disable=SC1037\n#lol\necho \"$12\""

prop_findsAnalysisIssue =
    check "echo $1" == [2086]
prop_commentDisablesAnalysisIssue1 =
    null $ check "#shellcheck disable=SC2086\necho $1"
prop_commentDisablesAnalysisIssue2 =
    null $ check "#shellcheck disable=SC2086\n#lol\necho $1"

prop_optionDisablesIssue1 =
    null $ getErrors
                (mockedSystemInterface [])
                emptyCheckSpec {
                    csScript = "echo $1",
                    csExcludedWarnings = [2148, 2086]
                }

prop_optionDisablesIssue2 =
    null $ getErrors
                (mockedSystemInterface [])
                emptyCheckSpec {
                    csScript = "echo \"$10\"",
                    csExcludedWarnings = [2148, 1037]
                }

prop_canParseDevNull =
    [] == check "source /dev/null"

prop_failsWhenNotSourcing =
    [1091, 2154] == check "source lol; echo \"$bar\""

prop_worksWhenSourcing =
    null $ checkWithIncludes [("lib", "bar=1")] "source lib; echo \"$bar\""

prop_worksWhenDotting =
    null $ checkWithIncludes [("lib", "bar=1")] ". lib; echo \"$bar\""

prop_noInfiniteSourcing =
    [] == checkWithIncludes  [("lib", "source lib")] "source lib"

prop_canSourceBadSyntax =
    [1094, 2086] == checkWithIncludes [("lib", "for f; do")] "source lib; echo $1"

prop_cantSourceDynamic =
    [1090] == checkWithIncludes [("lib", "")] ". \"$1\""

prop_cantSourceDynamic2 =
    [1090] == checkWithIncludes [("lib", "")] "source ~/foo"

prop_canSourceDynamicWhenRedirected =
    null $ checkWithIncludes [("lib", "")] "#shellcheck source=lib\n. \"$1\""

prop_recursiveAnalysis =
    [2086] == checkRecursive [("lib", "echo $1")] "source lib"

prop_recursiveParsing =
    [1037] == checkRecursive [("lib", "echo \"$10\"")] "source lib"

prop_sourceDirectiveDoesntFollowFile =
    null $ checkWithIncludes
                [("foo", "source bar"), ("bar", "baz=3")]
                "#shellcheck source=foo\n. \"$1\"; echo \"$baz\""

prop_filewideAnnotationBase = [2086] == check "#!/bin/sh\necho $1"
prop_filewideAnnotation1 = null $
    check "#!/bin/sh\n# shellcheck disable=2086\necho $1"
prop_filewideAnnotation2 = null $
    check "#!/bin/sh\n# shellcheck disable=2086\ntrue\necho $1"
prop_filewideAnnotation3 = null $
    check "#!/bin/sh\n#unerlated\n# shellcheck disable=2086\ntrue\necho $1"
prop_filewideAnnotation4 = null $
    check "#!/bin/sh\n# shellcheck disable=2086\n#unrelated\ntrue\necho $1"
prop_filewideAnnotation5 = null $
    check "#!/bin/sh\n\n\n\n#shellcheck disable=2086\ntrue\necho $1"
prop_filewideAnnotation6 = null $
    check "#shellcheck shell=sh\n#unrelated\n#shellcheck disable=2086\ntrue\necho $1"
prop_filewideAnnotation7 = null $
    check "#!/bin/sh\n# shellcheck disable=2086\n#unrelated\ntrue\necho $1"

prop_filewideAnnotationBase2 = [2086, 2181] == check "true\n[ $? == 0 ] && echo $1"
prop_filewideAnnotation8 = null $
    check "# Disable $? warning\n#shellcheck disable=SC2181\n# Disable quoting warning\n#shellcheck disable=2086\ntrue\n[ $? == 0 ] && echo $1"

return []
runTests = $quickCheckAll
