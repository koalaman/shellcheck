{-
    Copyright 2012-2015 Vidar Holen

    This file is part of ShellCheck.
    http://www.vidarholen.net/contents/shellcheck

    ShellCheck is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    ShellCheck is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
    return $ PositionedComment position c
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
            psScript = contents
        }
        let parseMessages = prComments result
        let analysisMessages =
                fromMaybe [] $
                    (arComments . analyzeScript . analysisSpec)
                        <$> prRoot result
        let translator = tokenToPosition (prTokenPositions result)
        return . nub . sortMessages . filter shouldInclude $
            (parseMessages ++ map translator analysisMessages)

    shouldInclude (PositionedComment _ (Comment _ code _)) =
        code `notElem` csExcludedWarnings spec

    sortMessages = sortBy (comparing order)
    order (PositionedComment pos (Comment severity code message)) =
        (posFile pos, posLine pos, posColumn pos, severity, code, message)
    getPosition (PositionedComment pos _) = pos

    analysisSpec root =
        AnalysisSpec {
            asScript = root,
            asShellType = csShellTypeOverride spec,
            asExecutionMode = Executed
         }

getErrors sys spec =
    sort . map getCode . crComments $
        runIdentity (checkScript sys spec)
  where
    getCode (PositionedComment _ (Comment _ code _)) = code

check str =
    getErrors
        (mockedSystemInterface [])
        emptyCheckSpec {
            csScript = str,
            csExcludedWarnings = [2148]
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

prop_failsWhenNotSourcing =
    [1091, 2154] == getErrors
                (mockedSystemInterface [])
                emptyCheckSpec {
                    csScript = "source lob; echo \"$bar\"",
                    csExcludedWarnings = [2148]
                }

prop_worksWhenSourcing =
    null $ getErrors
                (mockedSystemInterface [("lib", "bar=1")])
                emptyCheckSpec {
                    csScript = "source lib; echo \"$bar\"",
                    csExcludedWarnings = [2148]
                }

prop_worksWhenDotting =
    null $ getErrors
                (mockedSystemInterface [("lib", "bar=1")])
                emptyCheckSpec {
                    csScript = ". lib; echo \"$bar\"",
                    csExcludedWarnings = [2148]
                }

prop_noInfiniteSourcing =
    [] == getErrors
                (mockedSystemInterface [("lib", "source lib")])
                emptyCheckSpec {
                    csScript = "source lib",
                    csExcludedWarnings = [2148]
                }

prop_canSourceBadSyntax =
    [1094, 2086] == getErrors
                (mockedSystemInterface [("lib", "for f; do")])
               emptyCheckSpec {
                    csScript = "source lib; echo $1",
                    csExcludedWarnings = [2148]
                }

return []
runTests = $quickCheckAll
