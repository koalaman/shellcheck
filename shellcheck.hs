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
import Control.Exception
import Control.Monad
import Data.Char
import GHC.Exts
import GHC.IO.Device
import Prelude hiding (catch)
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
        (ReqArg (Flag "format") "FORMAT") "output format",
    Option ['e'] ["exclude"]
        (ReqArg (Flag "exclude") "CODE1,CODE2..") "exclude types of warnings"
    ]

printErr = hPutStrLn stderr

syntaxFailure = ExitFailure 3
supportFailure = ExitFailure 4

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
                exitWith syntaxFailure

        (_, _, errors) -> do
            printErr $ (concat errors) ++ "\n" ++ usageInfo header options
            exitWith syntaxFailure

formats = Map.fromList [
    ("json", forJson),
    ("gcc", forGcc),
    ("checkstyle", forCheckstyle),
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
        let comments = getComments options contents
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
    comments <- liftM concat $ mapM (commentsFor options) files
    putStrLn $ encodeStrict $ comments
    return . null $ comments

-- Mimic GCC "file:line:col: (error|warning|note): message" format
forGcc options files = do
    files <- mapM process files
    return $ and files
  where
    process file = do
        contents <- readContents file
        let comments = makeNonVirtual (getComments options contents) contents
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

-- Checkstyle compatible output. A bit of a hack to avoid XML dependencies
forCheckstyle options files = do
    putStrLn "<?xml version='1.0' encoding='UTF-8'?>"
    putStrLn "<checkstyle version='4.3'>"
    statuses <- mapM (\x -> process x `catch` report) files
    putStrLn "</checkstyle>"
    return $ and statuses
  where
    process file = do
        comments <- commentsFor options file
        putStrLn (formatFile file comments)
        return $ null comments
    report error = do
        printErr $ show (error :: SomeException)
        return False

    severity "error" = "error"
    severity "warning" = "warning"
    severity _ = "info"
    attr s v = concat [ s, "='", escape v, "' " ]
    escape msg = concatMap escape' msg
    escape' c = if isOk c then [c] else "&#" ++ (show $ ord c) ++ ";"
    isOk x = any ($x) [isAsciiUpper, isAsciiLower, isDigit, (`elem` " ./")]

    formatFile name comments = concat [
        "<file ", attr "name" name, ">\n",
            concatMap format comments,
        "</file>"
        ]

    format c = concat [
        "<error ",
        attr "line" $ show . scLine $ c,
        attr "column" $ show . scColumn $ c,
        attr "severity" $ severity . scSeverity $ c,
        attr "message" $ scMessage c,
        attr "source" $ "ShellCheck.SC" ++ (show $ scCode c),
        "/>\n"
        ]

commentsFor options file =
    liftM (getComments options) $ readContents file

getComments options contents =
    excludeCodes (getExclusions options) $ shellCheck contents

readContents file = if file == "-" then getContents else readFile file

-- Realign comments from a tabstop of 8 to 1
makeNonVirtual comments contents =
    map fix comments
  where
    ls = lines contents
    fix c = c { scColumn = real (ls !! (scLine c - 1)) 0 0 (scColumn c) }
    real _ r v target | target <= v = r
    real [] r v _ = r -- should never happen
    real ('\t':rest) r v target =
        real rest (r+1) (v + 8 - (v `mod` 8)) target
    real (_:rest) r v target = real rest (r+1) (v+1) target

getOption [] _ def = def
getOption ((Flag var val):_) name _ | name == var = val
getOption (_:rest) flag def = getOption rest flag def

getOptions options name =
    map (\(Flag _ val) -> val) . filter (\(Flag var _) -> var == name) $ options

split char str =
    split' str []
  where
    split' (a:rest) element =
        if a == char
        then (reverse element) : split' rest []
        else split' rest (a:element)
    split' [] element = [reverse element]

getExclusions options =
    let elements = concatMap (split ',') $ getOptions options "exclude"
        clean = dropWhile (not . isDigit)
    in
        map (Prelude.read . clean) elements :: [Int]

excludeCodes codes comments =
    filter (not . hasCode) comments
  where
    hasCode c = scCode c `elem` codes

main = do
    args <- getArgs
    parsedArgs <- parseArguments args
    code <- do
        status <- process parsedArgs
        return $ if status then ExitSuccess else ExitFailure 1
     `catch` return
     `catch` \err -> do
        printErr $ show (err :: SomeException)
        return $ ExitFailure 2
    exitWith code

process Nothing = return False
process (Just (options, files)) =
  let format = getOption options "format" "tty" in
    case Map.lookup format formats of
        Nothing -> do
            printErr $ "Unknown format " ++ format
            printErr $ "Supported formats:"
            mapM_ (printErr . write) $ Map.keys formats
            exitWith supportFailure
          where write s = "  " ++ s
        Just f -> do
            f options files

