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
import Control.Monad.Trans
import Control.Monad.Trans.Error
import Control.Monad.Trans.List
import Data.Char
import Data.Maybe
import Data.Monoid
import GHC.Exts
import GHC.IO.Device
import Prelude hiding (catch)
import ShellCheck.Data
import ShellCheck.Options
import ShellCheck.Simple
import ShellCheck.Analytics
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.IO
import Text.JSON
import qualified Data.Map as Map

data Flag = Flag String String
data Status = NoProblems | SomeProblems | BadInput | SupportFailure | SyntaxFailure | RuntimeException deriving (Ord, Eq)

data JsonComment = JsonComment FilePath ShellCheckComment

instance Error Status where
    noMsg = RuntimeException

instance Monoid Status where
    mempty = NoProblems
    mappend = max

header = "Usage: shellcheck [OPTIONS...] FILES..."
options = [
    Option "e" ["exclude"]
        (ReqArg (Flag "exclude") "CODE1,CODE2..") "exclude types of warnings",
    Option "f" ["format"]
        (ReqArg (Flag "format") "FORMAT") "output format",
    Option "s" ["shell"]
        (ReqArg (Flag "shell") "SHELLNAME") "Specify dialect (bash,sh,ksh)",
    Option "V" ["version"]
        (NoArg $ Flag "version" "true") "Print version information"
    ]

printErr = hPutStrLn stderr


instance JSON (JsonComment) where
  showJSON (JsonComment filename c) = makeObj [
      ("file", showJSON $ filename),
      ("line", showJSON $ scLine c),
      ("column", showJSON $ scColumn c),
      ("level", showJSON $ scSeverity c),
      ("code", showJSON $ scCode c),
      ("message", showJSON $ scMessage c)
      ]
  readJSON = undefined

parseArguments :: [String] -> ErrorT Status IO ([Flag], [FilePath])
parseArguments argv =
    case getOpt Permute options argv of
        (opts, files, []) -> return (opts, files)
        (_, _, errors) -> do
            liftIO . printErr $ concat errors ++ "\n" ++ usageInfo header options
            throwError SyntaxFailure

formats :: Map.Map String (AnalysisOptions -> [FilePath] -> IO Status)
formats = Map.fromList [
    ("json", forJson),
    ("gcc", forGcc),
    ("checkstyle", forCheckstyle),
    ("tty", forTty)
    ]

toStatus = liftM (either id (const NoProblems)) . runErrorT

catchExceptions :: IO Status -> IO Status
catchExceptions action = action -- action `catch` handler
  where
    handler err = do
        printErr $ show (err :: SomeException)
        return RuntimeException

checkComments comments = if null comments then NoProblems else SomeProblems

forTty :: AnalysisOptions -> [FilePath] -> IO Status
forTty options files = do
    output <- mapM doFile files
    return $ mconcat output
  where
    clear = ansi 0
    ansi n = "\x1B[" ++ show n ++ "m"

    colorForLevel "error" = 31 -- red
    colorForLevel "warning" = 33 -- yellow
    colorForLevel "info" = 32 -- green
    colorForLevel "style" = 32 -- green
    colorForLevel "message" = 1 -- bold
    colorForLevel "source" = 0 -- none
    colorForLevel _ = 0 -- none

    colorComment level comment =
        ansi (colorForLevel level) ++ comment ++ clear

    doFile path = catchExceptions $ do
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
            putStrLn $ colorFunc "message"
                ("In " ++ filename ++" line " ++ show lineNum ++ ":")
            putStrLn (colorFunc "source" line)
            mapM_ (\c -> putStrLn (colorFunc (scSeverity c) $ cuteIndent c)) x
            putStrLn ""
          ) groups
        return . checkComments $ comments

    cuteIndent comment =
        replicate (scColumn comment - 1) ' ' ++
            "^-- " ++ code (scCode comment) ++ ": " ++ scMessage comment

    code code = "SC" ++ show code

    getColorFunc = do
        term <- hIsTerminalDevice stdout
        return $ if term then colorComment else const id

forJson :: AnalysisOptions -> [FilePath] -> IO Status
forJson options files = catchExceptions $ do
    comments <- runListT $ do
        file <- ListT $ return files
        comment <- ListT $ commentsFor options file
        return $ JsonComment file comment
    putStrLn $ encodeStrict comments
    return $ checkComments comments

-- Mimic GCC "file:line:col: (error|warning|note): message" format
forGcc :: AnalysisOptions -> [FilePath] -> IO Status
forGcc options files = do
    files <- mapM process files
    return $ mconcat files
  where
    process file = catchExceptions $ do
        contents <- readContents file
        let comments = makeNonVirtual (getComments options contents) contents
        mapM_ (putStrLn . format file) comments
        return $ checkComments comments

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
forCheckstyle :: AnalysisOptions -> [FilePath] -> IO Status
forCheckstyle options files = do
    putStrLn "<?xml version='1.0' encoding='UTF-8'?>"
    putStrLn "<checkstyle version='4.3'>"
    statuses <- mapM process files
    putStrLn "</checkstyle>"
    return $ mconcat statuses
  where
    process file = catchExceptions $ do
        comments <- commentsFor options file
        putStrLn (formatFile file comments)
        return $ checkComments comments

    severity "error" = "error"
    severity "warning" = "warning"
    severity _ = "info"
    attr s v = concat [ s, "='", escape v, "' " ]
    escape = concatMap escape'
    escape' c = if isOk c then [c] else "&#" ++ show (ord c) ++ ";"
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
        attr "source" $ "ShellCheck.SC" ++ show (scCode c),
        "/>\n"
        ]

commentsFor options file = liftM (getComments options) $ readContents file

getComments = shellCheck

readContents :: FilePath -> IO String
readContents file =
    if file == "-"
    then getContents
    else readFile file

-- Realign comments from a tabstop of 8 to 1
makeNonVirtual comments contents =
    map fix comments
  where
    ls = lines contents
    fix c = c {
        scColumn =
            if scLine c > 0 && scLine c <= length ls
            then real (ls !! (scLine c - 1)) 0 0 (scColumn c)
            else scColumn c
    }
    real _ r v target | target <= v = r
    real [] r v _ = r -- should never happen
    real ('\t':rest) r v target =
        real rest (r+1) (v + 8 - (v `mod` 8)) target
    real (_:rest) r v target = real rest (r+1) (v+1) target

getOption [] _ = Nothing
getOption (Flag var val:_) name | name == var = return val
getOption (_:rest) flag = getOption rest flag

getOptions options name =
    map (\(Flag _ val) -> val) . filter (\(Flag var _) -> var == name) $ options

split char str =
    split' str []
  where
    split' (a:rest) element =
        if a == char
        then reverse element : split' rest []
        else split' rest (a:element)
    split' [] element = [reverse element]

getExclusions options =
    let elements = concatMap (split ',') $ getOptions options "exclude"
        clean = dropWhile (not . isDigit)
    in
        map (Prelude.read . clean) elements :: [Int]

excludeCodes codes =
    filter (not . hasCode)
  where
    hasCode c = scCode c `elem` codes

main = do
    args <- getArgs
    status <- toStatus $ do
        (flags, files) <- parseArguments args
        process flags files
    exitWith $ statusToCode status

statusToCode status =
    case status of
        NoProblems -> ExitSuccess
        SomeProblems -> ExitFailure 1
        BadInput -> ExitFailure 5
        SyntaxFailure -> ExitFailure 3
        SupportFailure -> ExitFailure 4
        RuntimeException -> ExitFailure 2

process :: [Flag] -> [FilePath] -> ErrorT Status IO ()
process flags files = do
    options <- foldM (flip parseOption) defaultAnalysisOptions flags
    verifyFiles files
    let format = fromMaybe "tty" $ getOption flags "format"
    case Map.lookup format formats of
        Nothing -> do
            liftIO $ do
                printErr $ "Unknown format " ++ format
                printErr "Supported formats:"
                mapM_ (printErr . write) $ Map.keys formats
            throwError SupportFailure
          where write s = "  " ++ s
        Just f -> ErrorT $ liftM Left $ f options files

parseOption flag options =
    case flag of
        Flag "shell" str ->
                fromMaybe (die $ "Unknown shell: " ++ str) $ do
                    shell <- shellForExecutable str
                    return $ return options { optionShellType = Just shell }

        Flag "exclude" str -> do
            new <- mapM parseNum $ split ',' str
            let old = optionExcludes options
            return options { optionExcludes = new ++ old }

        Flag "version" _ -> do
            liftIO printVersion
            throwError NoProblems

        _ -> return options
  where
    die s = do
        liftIO $ printErr s
        throwError SupportFailure
    parseNum ('S':'C':str) = parseNum str
    parseNum num = do
        unless (all isDigit num) $ do
            liftIO . printErr $ "Bad exclusion: " ++ num
            throwError SyntaxFailure
        return (Prelude.read num :: Integer)

verifyFiles files =
    when (null files) $ do
        liftIO $ printErr "No files specified.\n"
        liftIO $ printErr $ usageInfo header options
        throwError SyntaxFailure

printVersion = do
    putStrLn   "ShellCheck - shell script analysis tool"
    putStrLn $ "version: " ++ shellcheckVersion
    putStrLn   "license: GNU Affero General Public License, version 3"
    putStrLn   "website: http://www.shellcheck.net"
