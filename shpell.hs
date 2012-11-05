import Control.Monad
import GHC.Exts
import GHC.IO.Device
import Shpell.Simple
import System.Directory
import System.Environment
import System.Exit
import System.IO


clear = ansi 0
ansi n = "\x1B[" ++ (show n) ++ "m"

colorForLevel "error" = 31 -- red
colorForLevel "warning" = 33 -- yellow
colorForLevel "info" = 33 -- yellow
colorForLevel "style" = 31 -- green
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
        putStrLn (colorFunc "error" $ "No such file: " ++ actualPath)

doInput filename contents colorFunc = do
    let fileLines = lines contents
    let lineCount = length fileLines
    let comments = shpellCheck contents
    let groups = groupWith shpellLine comments
    if not $ null comments then do
        mapM_ (\x -> do
            let lineNum = shpellLine (head x)
            let line = if lineNum < 1 || lineNum >= lineCount
                            then ""
                            else fileLines !! (lineNum - 1)
            putStrLn ""
            putStrLn $ colorFunc "message" ("In " ++ filename ++" line " ++ (show $ lineNum) ++ ":")
            putStrLn (colorFunc "source" line)
            mapM (\c -> putStrLn (colorFunc (shpellSeverity c) $ cuteIndent c)) x
            putStrLn ""
          ) groups
      else do
        putStrLn ("No comments for " ++ filename)

cuteIndent comment =
    (replicate ((shpellColumn comment) - 1) ' ') ++ "^-- " ++ (shpellComment comment)

getColorFunc = do
    term <- hIsTerminalDevice stdout
    return $ if term then colorComment else const id

main = do
    args <- getArgs
    colors <- getColorFunc
    if null args then do
        hPutStrLn stderr "shpell -- bash/sh shell script static analysis tool"
        hPutStrLn stderr "Usage: shpell filenames..."
        exitFailure
      else
        mapM (\f -> doFile f colors) args

