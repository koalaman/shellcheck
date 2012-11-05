module Shpell.Simple (shpellCheck, ShpellComment, shpellLine, shpellColumn, shpellSeverity, shpellComment) where

import Shpell.Parser
import Shpell.Analytics
import Data.Maybe
import Text.Parsec.Pos

shpellCheck :: String -> [ShpellComment]
shpellCheck script =
    let (ParseResult result notes) = parseShell "-" script in
        let allNotes = notes ++ (concat $ maybeToList $ do
            (tree, map) <- result
            let newMap = runAllAnalytics tree map
            return $ notesFromMap newMap
            )
        in
            map formatNote $ sortNotes allNotes

data ShpellComment = ShpellComment { shpellLine :: Int, shpellColumn :: Int, shpellSeverity :: String, shpellComment :: String }


instance Show ShpellComment where
    show c = concat ["(", show $ shpellLine c, ",", show $ shpellColumn c, ") ", shpellSeverity c, ": ", shpellComment c]

severityToString s =
    case s of
        ErrorC -> "error"
        WarningC -> "warning"
        InfoC -> "info"
        StyleC -> "style"

formatNote (ParseNote pos severity text) = ShpellComment (sourceLine pos) (sourceColumn pos) (severityToString severity) text
