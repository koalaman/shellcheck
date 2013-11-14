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
import Control.Monad
import GHC.Exts
import GHC.IO.Device
import ShellCheck.Simple
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.IO
import Text.JSON
import qualified Data.Map as Map

data Flag = Flag String String

header = "Usage: shellcheck [OPTIONS...] FILES..."
options = [
    Option ['f'] ["format"]
        (ReqArg (Flag "format") "FORMAT") "output format"
    ]

printErr = hPutStrLn stderr

instance JSON ShellCheckComment where
  showJSON c = makeObj [
      ("line", showJSON $ scLine c),
      ("column", showJSON $ scColumn c),
      ("level", showJSON $ scSeverity c),
      ("code", showJSON $ scCode c),
      ("message", showJSON $ scMessage c)
      ]
  readJSON = undefined

parseArguments argv =
    case getOpt Permute options argv of
        (opts, files, []) ->
            if not $ null files
              then
                return $ Just (opts, files)
              else do
                printErr "No files specified.\n"
                printErr $ usageInfo header options
                return $ Nothing

        (_, _, errors) -> do
            printErr $ (concat errors) ++ "\n" ++ usageInfo header options
            return Nothing

formats = Map.fromList [
    ("json", forJson),
    ("gcc", forGcc),
    ("tty", forTty)
    ]

forTty options files = do
    output <- mapM doFile files
    return $ and output
  where
    clear = ansi 0
    ansi n = "\x1B[" ++ (show n) ++ "m"

    colorForLevel "error" = 31 -- red
    colorForLevel "warning" = 33 -- yellow
    colorForLevel "info" = 32 -- green
    colorForLevel "style" = 32 -- green
    colorForLevel "message" = 1 -- bold
    colorForLevel "source" = 0 -- none
    colorForLevel _ = 0 -- none

    colorComment level comment = (ansi $ colorForLevel level) ++ comment ++ clear

    doFile path = do
        contents <- readContents path
        doInput path contents

    doInput filename contents = do
        let fileLines = lines contents
        let lineCount = length fileLines
        let comments = shellCheck contents
        let groups = groupWith scLine comments
        colorFunc <- getColorFunc
        mapM_ (\x -> do
            let lineNum = scLine (head x)
            let line = if lineNum < 1 || lineNum > lineCount
                            then ""
                            else fileLines !! (lineNum - 1)
            putStrLn ""
            putStrLn $ colorFunc "message" ("In " ++ filename ++" line " ++ (show $ lineNum) ++ ":")
            putStrLn (colorFunc "source" line)
            mapM (\c -> putStrLn (colorFunc (scSeverity c) $ cuteIndent c)) x
            putStrLn ""
          ) groups
        return $ null comments

    cuteIndent comment =
        (replicate ((scColumn comment) - 1) ' ') ++ "^-- " ++ (code $ scCode comment) ++ ": " ++ (scMessage comment)

    code code = "SC" ++ (show code)

    getColorFunc = do
        term <- hIsTerminalDevice stdout
        return $ if term then colorComment else const id

-- This totally ignores the filenames. Fixme?
forJson options files = do
    comments <- liftM concat $ mapM commentsFor files
    putStrLn $ encodeStrict $ comments
    return . null $ comments

--- Mimic GCC "file:line:col: (error|warning|note): message" format
forGcc options files = do
    files <- mapM process files
    return $ and files
  where
    process file = do
        comments <- commentsFor file
        mapM_ (putStrLn . format file) comments
        return $ null comments

    format filename c = concat [
            filename, ":",
            show $ scLine c, ":",
            show $ scColumn c, ": ",
            case scSeverity c of 
                "error" -> "error"
                "warning" -> "warning"
                _ -> "note",
            ": ",
            concat . lines $ scMessage c,
            " [SC", show $ scCode c, "]"
      ]
        
commentsFor file = liftM shellCheck $ readContents file
readContents file = if file == "-" then getContents else readFile file

getOption [] _ def = def
getOption ((Flag var val):_) name _ | name == var = val
getOption (_:rest) flag def = getOption rest flag def

main = do
    args <- getArgs
    parsedArgs <- parseArguments args
    status <- process parsedArgs
    if status then exitSuccess else exitFailure

process Nothing = return False
process (Just (options, files)) =
  let format = getOption options "format" "tty" in
    case Map.lookup format formats of
        Nothing -> do
            printErr $ "Unknown format " ++ format
            printErr $ "Supported formats:"
            mapM_ (printErr . write) $ Map.keys formats
            return False
          where write s = "  " ++ s
        Just f -> do
            f options files

