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
module ShellCheck.Simple (shellCheck, ShellCheckComment, scLine, scColumn, scSeverity, scMessage) where

import ShellCheck.Parser
import ShellCheck.Analytics
import Data.Maybe
import Text.Parsec.Pos
import Data.List

shellCheck :: String -> [ShellCheckComment]
shellCheck script =
    let (ParseResult result notes) = parseShell "-" script in
        let allNotes = notes ++ (concat $ maybeToList $ do
            (tree, map) <- result
            let newMap = runAllAnalytics tree map
            return $ notesFromMap newMap
            )
        in
            map formatNote $ nub $ sortNotes allNotes

data ShellCheckComment = ShellCheckComment { scLine :: Int, scColumn :: Int, scSeverity :: String, scMessage :: String }

instance Show ShellCheckComment where
    show c = concat ["(", show $ scLine c, ",", show $ scColumn c, ") ", scSeverity c, ": ", scMessage c]

severityToString s =
    case s of
        ErrorC -> "error"
        WarningC -> "warning"
        InfoC -> "info"
        StyleC -> "style"

formatNote (ParseNote pos severity text) = ShellCheckComment (sourceLine pos) (sourceColumn pos) (severityToString severity) text
