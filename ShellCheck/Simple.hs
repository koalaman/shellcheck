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
