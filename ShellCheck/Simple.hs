module ShellCheck.Simple (shellcheckCheck, ShellCheckComment, shellcheckLine, shellcheckColumn, shellcheckSeverity, shellcheckComment) where

import ShellCheck.Parser
import ShellCheck.Analytics
import Data.Maybe
import Text.Parsec.Pos
import Data.List

shellcheckCheck :: String -> [ShellCheckComment]
shellcheckCheck script =
    let (ParseResult result notes) = parseShell "-" script in
        let allNotes = notes ++ (concat $ maybeToList $ do
            (tree, map) <- result
            let newMap = runAllAnalytics tree map
            return $ notesFromMap newMap
            )
        in
            map formatNote $ nub $ sortNotes allNotes

data ShellCheckComment = ShellCheckComment { shellcheckLine :: Int, shellcheckColumn :: Int, shellcheckSeverity :: String, shellcheckComment :: String }


instance Show ShellCheckComment where
    show c = concat ["(", show $ shellcheckLine c, ",", show $ shellcheckColumn c, ") ", shellcheckSeverity c, ": ", shellcheckComment c]

severityToString s =
    case s of
        ErrorC -> "error"
        WarningC -> "warning"
        InfoC -> "info"
        StyleC -> "style"

formatNote (ParseNote pos severity text) = ShellCheckComment (sourceLine pos) (sourceColumn pos) (severityToString severity) text
