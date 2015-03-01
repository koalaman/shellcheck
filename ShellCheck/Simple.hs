{-
    This file is part of ShellCheck.
    http://www.vidarholen.net/contents/shellcheck

    ShellCheck is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    ShellCheck is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}
module ShellCheck.Simple (shellCheck, ShellCheckComment, scLine, scColumn, scSeverity, scCode, scMessage, runTests) where

import Data.List
import Data.Maybe
import ShellCheck.Analytics hiding (runTests)
import ShellCheck.Options
import ShellCheck.Parser hiding (runTests)
import Test.QuickCheck.All (quickCheckAll)
import Text.Parsec.Pos

shellCheck :: AnalysisOptions -> String -> [ShellCheckComment]
shellCheck options script =
    let (ParseResult result notes) = parseShell options "-" script in
        let allNotes = notes ++ concat (maybeToList $ do
            (tree, posMap) <- result
            let list = runAnalytics options tree
            return $ map (noteToParseNote posMap) $ filterByAnnotation tree list
            )
        in
            map formatNote $ nub $ sortNotes allNotes

data ShellCheckComment = ShellCheckComment { scLine :: Int, scColumn :: Int, scSeverity :: String, scCode :: Int, scMessage :: String }

instance Show ShellCheckComment where
    show c = concat ["(", show $ scLine c, ",", show $ scColumn c, ") ", scSeverity c, ": ", show (scCode c), " ", scMessage c]

severityToString s =
    case s of
        ErrorC -> "error"
        WarningC -> "warning"
        InfoC -> "info"
        StyleC -> "style"

formatNote (ParseNote pos severity code text) =
    ShellCheckComment (sourceLine pos) (sourceColumn pos) (severityToString severity) (fromIntegral code) text

testCheck = shellCheck defaultAnalysisOptions { optionExcludes = [2148] } -- Ignore #! warnings
prop_findsParseIssue =
    let comments = testCheck "echo \"$12\"" in
        length comments == 1 && scCode (head comments) == 1037
prop_commentDisablesParseIssue1 =
    null $ testCheck "#shellcheck disable=SC1037\necho \"$12\""
prop_commentDisablesParseIssue2 =
    null $ testCheck "#shellcheck disable=SC1037\n#lol\necho \"$12\""

prop_findsAnalysisIssue =
    let comments = testCheck "echo $1" in
        length comments == 1 && scCode (head comments) == 2086
prop_commentDisablesAnalysisIssue1 =
    null $ testCheck "#shellcheck disable=SC2086\necho $1"
prop_commentDisablesAnalysisIssue2 =
    null $ testCheck "#shellcheck disable=SC2086\n#lol\necho $1"

prop_optionDisablesIssue1 =
    null $ shellCheck (defaultAnalysisOptions { optionExcludes = [2086, 2148] }) "echo $1"

prop_optionDisablesIssue2 =
    null $ shellCheck (defaultAnalysisOptions { optionExcludes = [2148, 1037] }) "echo \"$10\""

return []
runTests = $quickCheckAll

