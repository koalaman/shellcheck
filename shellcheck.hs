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
import System.Directory
import System.Environment
import System.Exit
import System.IO

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

doFile path colorFunc = do
    let actualPath = if path == "-" then "/dev/stdin" else path
    exists <- doesFileExist actualPath
    if exists then do
        contents <- readFile actualPath
        doInput path contents colorFunc
      else do
        hPutStrLn stderr (colorFunc "error" $ "No such file: " ++ actualPath)
        return False

doInput filename contents colorFunc = do
    let fileLines = lines contents
    let lineCount = length fileLines
    let comments = shellCheck contents
    let groups = groupWith scLine comments
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

main = do
    args <- getArgs
    colors <- getColorFunc
    if null args then do
        hPutStrLn stderr "shellcheck -- bash/sh script static analysis tool"
        hPutStrLn stderr "Usage: shellcheck filenames..."
        exitFailure
      else do
        statuses <- mapM (\f -> doFile f colors) args
        if and statuses then exitSuccess else exitFailure

