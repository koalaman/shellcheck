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

shellFromFilename filename = listToMaybe candidates
  where
    shellExtensions = [(".ksh", Ksh)
                      ,(".bash", Bash)
                      ,(".bats", Bash)
                      ,(".dash", Dash)]
                      -- The `.sh` is too generic to determine the shell:
                      -- We fallback to Bash in this case and emit SC2148 if there is no shebang
    candidates =
        [sh | (ext,sh) <- shellExtensions, ext `isSuffixOf` filename]

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
            psIgnoreRC = csIgnoreRC spec,
            psShellTypeOverride = csShellTypeOverride spec
        }
        let parseMessages = prComments result
        let tokenPositions = prTokenPositions result
        let analysisSpec root =
                as {
                    asScript = root,
                    asShellType = csShellTypeOverride spec,
                    asFallbackShell = shellFromFilename $ csFilename spec,
                    asCheckSourced = csCheckSourced spec,
                    asExecutionMode = Executed,
                    asTokenPositions = tokenPositions,
                    asOptionalChecks = csOptionalChecks spec
                } where as = newAnalysisSpec root
        let analysisMessages =
                maybe []
                    (arComments . analyzeScript . analysisSpec)
                        $ prRoot result
        let translator = tokenToPosition tokenPositions
        return . nub . sortMessages . filter shouldInclude $
            (parseMessages ++ map translator analysisMessages)

    shouldInclude pc =
            severity <= csMinSeverity spec &&
            case csIncludedWarnings spec of
                Nothing -> code `notElem` csExcludedWarnings spec
                Just includedWarnings -> code `elem` includedWarnings
        where
            code     = cCode (pcComment pc)
            severity = cSeverity (pcComment pc)

    sortMessages = sortOn order
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

checkOptionIncludes includes src =
    checkWithSpec [] emptyCheckSpec {
        csScript = src,
        csIncludedWarnings = includes,
        csCheckSourced = True
    }

checkWithRc rc = getErrors
    (mockRcFile rc $ mockedSystemInterface [])

checkWithIncludesAndSourcePath includes mapper = getErrors
    (mockedSystemInterface includes) {
        siFindSource = mapper
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

prop_wontParseBadShell =
    [1071] == check "#!/usr/bin/python\ntrue $1\n"

prop_optionDisablesBadShebang =
    null $ getErrors
                (mockedSystemInterface [])
                emptyCheckSpec {
                    csScript = "#!/usr/bin/python\ntrue\n",
                    csShellTypeOverride = Just Sh
                }

prop_annotationDisablesBadShebang =
    null $ check "#!/usr/bin/python\n# shellcheck shell=sh\ntrue\n"


prop_canParseDevNull =
    null $ check "source /dev/null"

prop_failsWhenNotSourcing =
    [1091, 2154] == check "source lol; echo \"$bar\""

prop_worksWhenSourcing =
    null $ checkWithIncludes [("lib", "bar=1")] "source lib; echo \"$bar\""

prop_worksWhenSourcingWithDashDash =
    null $ checkWithIncludes [("lib", "bar=1")] "source -- lib; echo \"$bar\""

prop_worksWhenDotting =
    null $ checkWithIncludes [("lib", "bar=1")] ". lib; echo \"$bar\""

-- FIXME: This should really be giving [1093], "recursively sourced"
prop_noInfiniteSourcing =
    null $ checkWithIncludes  [("lib", "source lib")] "source lib"

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

prop_nonRecursiveAnalysis =
    null $ checkWithIncludes [("lib", "echo $1")] "source lib"

prop_nonRecursiveParsing =
    null $ checkWithIncludes [("lib", "echo \"$10\"")] "source lib"

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
    check "#!/bin/sh\n#unrelated\n# shellcheck disable=2086\ntrue\necho $1"
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

prop_sourcePartOfOriginalScript = -- #1181: -x disabled posix warning for 'source'
    2039 `elem` checkWithIncludes [("./saywhat.sh", "echo foo")] "#!/bin/sh\nsource ./saywhat.sh"

prop_spinBug1413 = null $ check "fun() {\n# shellcheck disable=SC2188\n> /dev/null\n}\n"

prop_deducesTypeFromExtension = null result
  where
    result = checkWithSpec [] emptyCheckSpec {
        csFilename = "file.ksh",
        csScript = "(( 3.14 ))"
    }

prop_deducesTypeFromExtension2 = result == [2079]
  where
    result = checkWithSpec [] emptyCheckSpec {
        csFilename = "file.bash",
        csScript = "(( 3.14 ))"
    }

prop_canDisableShebangWarning = null $ result
  where
    result = checkWithSpec [] emptyCheckSpec {
        csFilename = "file.sh",
        csScript = "#shellcheck disable=SC2148\nfoo"
    }

prop_canDisableParseErrors = null $ result
  where
    result = checkWithSpec [] emptyCheckSpec {
        csFilename = "file.sh",
        csScript = "#shellcheck disable=SC1073,SC1072,SC2148\n()"
    }

prop_shExtensionDoesntMatter = result == [2148]
  where
    result = checkWithSpec [] emptyCheckSpec {
        csFilename = "file.sh",
        csScript = "echo 'hello world'"
    }

prop_sourcedFileUsesOriginalShellExtension = result == [2079]
  where
    result = checkWithSpec [("file.ksh", "(( 3.14 ))")] emptyCheckSpec {
        csFilename = "file.bash",
        csScript = "source file.ksh",
        csCheckSourced = True
    }

prop_canEnableOptionalsWithSpec = result == [2244]
  where
    result = checkWithSpec [] emptyCheckSpec {
        csFilename = "file.sh",
        csScript = "#!/bin/sh\n[ \"$1\" ]",
        csOptionalChecks = ["avoid-nullary-conditions"]
    }

prop_optionIncludes1 =
    -- expect 2086, but not included, so nothing reported
    null $ checkOptionIncludes (Just [2080]) "#!/bin/sh\n var='a b'\n echo $var"

prop_optionIncludes2 =
    -- expect 2086, included, so it is reported
    [2086] == checkOptionIncludes (Just [2086]) "#!/bin/sh\n var='a b'\n echo $var"

prop_optionIncludes3 =
    -- expect 2086, no inclusions provided, so it is reported
    [2086] == checkOptionIncludes Nothing "#!/bin/sh\n var='a b'\n echo $var"

prop_optionIncludes4 =
    -- expect 2086 & 2154, only 2154 included, so only that's reported
    [2154] == checkOptionIncludes (Just [2154]) "#!/bin/sh\n var='a b'\n echo $var\n echo $bar"


prop_readsRcFile = null result
  where
    result = checkWithRc "disable=2086" emptyCheckSpec {
        csScript = "#!/bin/sh\necho $1",
        csIgnoreRC = False
    }

prop_canUseNoRC = result == [2086]
  where
    result = checkWithRc "disable=2086" emptyCheckSpec {
        csScript = "#!/bin/sh\necho $1",
        csIgnoreRC = True
    }

prop_NoRCWontLookAtFile = result == [2086]
  where
    result = checkWithRc (error "Fail") emptyCheckSpec {
        csScript = "#!/bin/sh\necho $1",
        csIgnoreRC = True
    }

prop_brokenRcGetsWarning = result == [1134, 2086]
  where
    result = checkWithRc "rofl" emptyCheckSpec {
        csScript = "#!/bin/sh\necho $1",
        csIgnoreRC = False
    }

prop_canEnableOptionalsWithRc = result == [2244]
  where
    result = checkWithRc "enable=avoid-nullary-conditions" emptyCheckSpec {
        csScript = "#!/bin/sh\n[ \"$1\" ]"
    }

prop_sourcePathRedirectsName = result == [2086]
  where
    f "dir/myscript" _ "lib" = return "foo/lib"
    result = checkWithIncludesAndSourcePath [("foo/lib", "echo $1")] f emptyCheckSpec {
        csScript = "#!/bin/bash\nsource lib",
        csFilename = "dir/myscript",
        csCheckSourced = True
    }

prop_sourcePathAddsAnnotation = result == [2086]
  where
    f "dir/myscript" ["mypath"] "lib" = return "foo/lib"
    result = checkWithIncludesAndSourcePath [("foo/lib", "echo $1")] f emptyCheckSpec {
        csScript = "#!/bin/bash\n# shellcheck source-path=mypath\nsource lib",
        csFilename = "dir/myscript",
        csCheckSourced = True
    }

prop_sourcePathRedirectsDirective = result == [2086]
  where
    f "dir/myscript" _ "lib" = return "foo/lib"
    f _ _ _ = return "/dev/null"
    result = checkWithIncludesAndSourcePath [("foo/lib", "echo $1")] f emptyCheckSpec {
        csScript = "#!/bin/bash\n# shellcheck source=lib\nsource kittens",
        csFilename = "dir/myscript",
        csCheckSourced = True
    }

return []
runTests = $quickCheckAll
