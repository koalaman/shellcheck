{-
    Copyright 2012-2015 Vidar Holen

    This file is part of ShellCheck.
    http://www.vidarholen.net/contents/shellcheck

    ShellCheck is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    ShellCheck is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
import ShellCheck.Data
import ShellCheck.Checker
import ShellCheck.Interface

import Control.Exception
import Control.Monad
import Control.Monad.Except
import Data.Char
import Data.Functor
import Data.Either
import Data.IORef
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import GHC.Exts
import GHC.IO.Device
import Prelude hiding (catch)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.Info
import Text.JSON
import qualified Data.Map as Map

data Flag = Flag String String
data Status =
    NoProblems
    | SomeProblems
    | BadInput
    | SupportFailure
    | SyntaxFailure
    | RuntimeException
  deriving (Ord, Eq)

instance Monoid Status where
    mempty = NoProblems
    mappend = max

lineNo (PositionedComment pos _) = posLine pos
colNo  (PositionedComment pos _) = posColumn pos
codeNo (PositionedComment _ (Comment _ code _)) = code
messageText (PositionedComment _ (Comment _ _ t)) = t

severityText :: PositionedComment -> String
severityText (PositionedComment _ (Comment c _ _)) =
    case c of
        ErrorC   -> "error"
        WarningC -> "warning"
        InfoC    -> "info"
        StyleC   -> "style"

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

printOut = lift . hPutStrLn stdout
printErr = lift . hPutStrLn stderr

instance JSON (PositionedComment) where
  showJSON comment@(PositionedComment pos (Comment level code string)) = makeObj [
      ("file", showJSON $ posFile pos),
      ("line", showJSON $ posLine pos),
      ("column", showJSON $ posColumn pos),
      ("level", showJSON $ severityText comment),
      ("code", showJSON code),
      ("message", showJSON string)
      ]
    where

  readJSON = undefined


parseArguments :: [String] -> ExceptT Status IO ([Flag], [FilePath])
parseArguments argv =
    case getOpt Permute options argv of
        (opts, files, []) -> return (opts, files)
        (_, _, errors) -> do
            printErr $ concat errors ++ "\n" ++ usageInfo header options
            throwError SyntaxFailure

formats = Map.fromList [
{-
    ("json", forJson),
    ("gcc", forGcc),
    ("checkstyle", forCheckstyle),
-}
    ("tty", forTty)
    ]

forTty :: SystemInterface IO -> CheckSpec -> [FilePath] -> ExceptT Status IO ()
forTty sys spec files = mapM_ doFile files
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

    doFile filename = do
        contents <- lift $ inputFile filename
        comments <- lift (crComments <$> checkScript sys spec { csScript = contents })
        let fileLines = lines contents
        let lineCount = fromIntegral $ length fileLines
        let groups = groupWith lineNo comments
        colorFunc <- getColorFunc
        mapM_ (\x -> do
            let lineNum = lineNo (head x)
            let line = if lineNum < 1 || lineNum > lineCount
                            then ""
                            else fileLines !! (fromIntegral $ lineNum - 1)
            printOut ""
            printOut $ colorFunc "message"
                ("In " ++ filename ++" line " ++ show lineNum ++ ":")
            printOut (colorFunc "source" line)
            mapM_ (\c -> printOut (colorFunc (severityText c) $ cuteIndent c)) x
            printOut ""
          ) groups

    cuteIndent :: PositionedComment -> String
    cuteIndent comment =
        replicate (fromIntegral $ colNo comment - 1) ' ' ++
            "^-- " ++ code (codeNo comment) ++ ": " ++ messageText comment

    code code = "SC" ++ show code

    getColorFunc = do
        term <- lift $ hIsTerminalDevice stdout
        let windows = "mingw" `isPrefixOf` os
        return $ if term && not windows then colorComment else const id

{-
forJson :: a -> Formatter
forJson _ result = do
    let comments = concatMap getComments (crComments result)
    lift $ putStrLn $ encodeStrict comments
  where
    getComments (_, FileResult comments) = comments
    getComments (file, FileError str) = [
        PositionedComment
            Position {
                posFile = file,
                posLine = 1,
                posColumn = 1
            }
            (Comment ErrorC 1000 str)
        ]

-- Mimic GCC "file:line:col: (error|warning|note): message" format
forGcc :: SystemInterface IO -> Formatter
forGcc io result = do
    mapM_ (uncurry process) (crComments result)
  where
    process filename (FileError string) = do
        printErr $ string

    process filename (FileResult result) = do
        fileInput <- lift $ siReadFile io filename
        when (isLeft fileInput) $ do
            printErr $ "Failed to re-open " ++ filename
            throwError RuntimeException
        let contents = fromRight fileInput
        let comments = makeNonVirtual result contents
        mapM_ (printOut . format filename) comments

    format filename c = concat [
            filename, ":",
            show $ lineNo c, ":",
            show $ colNo c, ": ",
            case severityText c of
                "error" -> "error"
                "warning" -> "warning"
                _ -> "note",
            ": ",
            concat . lines $ messageText c,
            " [SC", show $ codeNo c, "]"
      ]

-- Checkstyle compatible output. A bit of a hack to avoid XML dependencies
forCheckstyle :: SystemInterface IO -> Formatter
forCheckstyle _ result = do
    printOut "<?xml version='1.0' encoding='UTF-8'?>"
    printOut "<checkstyle version='4.3'>"
    statuses <- mapM process (crComments result)
    printOut "</checkstyle>"
    return $ mconcat statuses
  where
    process (file, FileError str) =
        printOut (formatError file str)

    process (file, FileResult comments) =
        printOut (formatFile file comments)

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
        attr "line" $ show . lineNo $ c,
        attr "column" $ show . colNo $ c,
        attr "severity" . severity $ severityText c,
        attr "message" $ messageText c,
        attr "source" $ "ShellCheck.SC" ++ show (codeNo c),
        "/>\n"
        ]

    formatError file msg = concat [
        "<file ", attr "name" file, ">\n",
        "<error ",
            attr "line" "1",
            attr "column" "1",
            attr "severity" $ severity "error",
            attr "message" msg,
            attr "source" "ShellCheck",
        "/>\n",
        "</file>"
        ]
-}


-- Realign comments from a tabstop of 8 to 1
makeNonVirtual comments contents =
    map fix comments
  where
    ls = lines contents
    fix c@(PositionedComment pos comment) = PositionedComment pos {
        posColumn =
            if lineNo c > 0 && lineNo c <= fromIntegral (length ls)
            then real (ls !! (fromIntegral $ lineNo c - 1)) 0 0 (colNo c)
            else colNo c
    } comment
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
    hasCode c = codeNo c `elem` codes

toStatus = liftM (either id (const NoProblems)) . runExceptT

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

process :: [Flag] -> [FilePath] -> ExceptT Status IO ()
process flags files = do
    options <- foldM (flip parseOption) emptyCheckSpec flags
    verifyFiles files
    let format = fromMaybe "tty" $ getOption flags "format"
    formatter <-
        case Map.lookup format formats of
            Nothing -> do
                printErr $ "Unknown format " ++ format
                printErr "Supported formats:"
                mapM_ (printErr . write) $ Map.keys formats
                throwError SupportFailure
              where write s = "  " ++ s
            Just f -> ExceptT $ fmap Right $ return f
    let sys = ioInterface (const False)
    formatter sys options files

parseOption flag options =
    case flag of
        Flag "shell" str ->
            fromMaybe (die $ "Unknown shell: " ++ str) $ do
                shell <- shellForExecutable str
                return $ return options { csShellTypeOverride = Just shell }

        Flag "exclude" str -> do
            new <- mapM parseNum $ split ',' str
            let old = csExcludedWarnings options
            return options { csExcludedWarnings = new ++ old }

        Flag "version" _ -> do
            liftIO printVersion
            throwError NoProblems

        _ -> return options
  where
    die s = do
        printErr s
        throwError SupportFailure
    parseNum ('S':'C':str) = parseNum str
    parseNum num = do
        unless (all isDigit num) $ do
            printErr $ "Bad exclusion: " ++ num
            throwError SyntaxFailure
        return (Prelude.read num :: Integer)

ioInterface filter = do
    SystemInterface {
        siReadFile = get
    }
  where
    get file = do
        if filter file
          then (Right <$> inputFile file) `catch` handler
          else return $ Left (file ++ " was not specified as input.")

    handler :: IOException -> IO (Either ErrorMessage String)
    handler ex = return . Left $ show ex

inputFile file = do
    contents <-
            if file == "-"
            then getContents
            else readFile file
    return contents

verifyFiles files =
    when (null files) $ do
        printErr "No files specified.\n"
        printErr $ usageInfo header options
        throwError SyntaxFailure

printVersion = do
    putStrLn   "ShellCheck - shell script analysis tool"
    putStrLn $ "version: " ++ shellcheckVersion
    putStrLn   "license: GNU General Public License, version 3"
    putStrLn   "website: http://www.shellcheck.net"
