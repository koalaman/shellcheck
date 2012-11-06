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
colorForLevel "info" = 33 -- yellow
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
        putStrLn (colorFunc "error" $ "No such file: " ++ actualPath)

doInput filename contents colorFunc = do
    let fileLines = lines contents
    let lineCount = length fileLines
    let comments = shellcheckCheck contents
    let groups = groupWith shellcheckLine comments
    if not $ null comments then do
        mapM_ (\x -> do
            let lineNum = shellcheckLine (head x)
            let line = if lineNum < 1 || lineNum > lineCount
                            then ""
                            else fileLines !! (lineNum - 1)
            putStrLn ""
            putStrLn $ colorFunc "message" ("In " ++ filename ++" line " ++ (show $ lineNum) ++ ":")
            putStrLn (colorFunc "source" line)
            mapM (\c -> putStrLn (colorFunc (shellcheckSeverity c) $ cuteIndent c)) x
            putStrLn ""
          ) groups
      else do
        putStrLn ("No comments for " ++ filename)

cuteIndent comment =
    (replicate ((shellcheckColumn comment) - 1) ' ') ++ "^-- " ++ (shellcheckComment comment)

getColorFunc = do
    term <- hIsTerminalDevice stdout
    return $ if term then colorComment else const id

main = do
    args <- getArgs
    colors <- getColorFunc
    if null args then do
        hPutStrLn stderr "shellcheck -- bash/sh shell script static analysis tool"
        hPutStrLn stderr "Usage: shellcheck filenames..."
        exitFailure
      else
        mapM (\f -> doFile f colors) args

