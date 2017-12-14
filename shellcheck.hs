{-
    Copyright 2012-2015 Vidar Holen

    This file is part of ShellCheck.
    https://www.shellcheck.net

    ShellCheck is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    ShellCheck is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}
import ShellCheck.Data
import ShellCheck.Checker
import ShellCheck.Interface
import ShellCheck.Regex

import ShellCheck.Formatter.Format
import qualified ShellCheck.Formatter.CheckStyle
import qualified ShellCheck.Formatter.GCC
import qualified ShellCheck.Formatter.JSON
import qualified ShellCheck.Formatter.TTY

import Control.Exception
import Control.Monad
import Control.Monad.Except
import Data.Bits
import Data.Char
import Data.Either
import Data.Functor
import Data.IORef
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Prelude hiding (catch)
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.IO

data Flag = Flag String String
data Status =
    NoProblems
    | SomeProblems
    | SupportFailure
    | SyntaxFailure
    | RuntimeException
  deriving (Ord, Eq, Show)

instance Monoid Status where
    mempty = NoProblems
    mappend = max

data Options = Options {
    checkSpec :: CheckSpec,
    externalSources :: Bool,
    formatterOptions :: FormatterOptions
}

defaultOptions = Options {
    checkSpec = emptyCheckSpec,
    externalSources = False,
    formatterOptions = FormatterOptions {
        foColorOption = ColorAuto
    }
}

usageHeader = "Usage: shellcheck [OPTIONS...] FILES..."
options = [
    Option "a" ["check-sourced"]
        (NoArg $ Flag "sourced" "false") "Include warnings from sourced files",
    Option "C" ["color"]
        (OptArg (maybe (Flag "color" "always") (Flag "color")) "WHEN")
        "Use color (auto, always, never)",
    Option "e" ["exclude"]
        (ReqArg (Flag "exclude") "CODE1,CODE2..") "Exclude types of warnings",
    Option "f" ["format"]
        (ReqArg (Flag "format") "FORMAT") $
        "Output format (" ++ formatList ++ ")",
    Option "s" ["shell"]
        (ReqArg (Flag "shell") "SHELLNAME")
        "Specify dialect (sh, bash, dash, ksh)",
    Option "V" ["version"]
        (NoArg $ Flag "version" "true") "Print version information",
    Option "x" ["external-sources"]
        (NoArg $ Flag "externals" "true") "Allow 'source' outside of FILES"
    ]

printErr = lift . hPutStrLn stderr

parseArguments :: [String] -> ExceptT Status IO ([Flag], [FilePath])
parseArguments argv =
    case getOpt Permute options argv of
        (opts, files, []) -> return (opts, files)
        (_, _, errors) -> do
            printErr $ concat errors ++ "\n" ++ usageInfo usageHeader options
            throwError SyntaxFailure

formats :: FormatterOptions -> Map.Map String (IO Formatter)
formats options = Map.fromList [
    ("checkstyle", ShellCheck.Formatter.CheckStyle.format),
    ("gcc",  ShellCheck.Formatter.GCC.format),
    ("json", ShellCheck.Formatter.JSON.format),
    ("tty",  ShellCheck.Formatter.TTY.format options)
    ]

formatList = intercalate ", " names
  where
    names = Map.keys $ formats (formatterOptions defaultOptions)

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

toStatus = fmap (either id id) . runExceptT

getEnvArgs = do
    opts <- getEnv "SHELLCHECK_OPTS" `catch` cantWaitForLookupEnv
    return . filter (not . null) $ opts `splitOn` mkRegex " +"
  where
    cantWaitForLookupEnv :: IOException -> IO String
    cantWaitForLookupEnv = const $ return ""

main = do
    params <- getArgs
    envOpts  <- getEnvArgs
    let args = envOpts ++ params
    status <- toStatus $ do
        (flags, files) <- parseArguments args
        process flags files
    exitWith $ statusToCode status

statusToCode status =
    case status of
        NoProblems -> ExitSuccess
        SomeProblems -> ExitFailure 1
        SyntaxFailure -> ExitFailure 3
        SupportFailure -> ExitFailure 4
        RuntimeException -> ExitFailure 2

process :: [Flag] -> [FilePath] -> ExceptT Status IO Status
process flags files = do
    options <- foldM (flip parseOption) defaultOptions flags
    verifyFiles files
    let format = fromMaybe "tty" $ getOption flags "format"
    let formatters = formats $ formatterOptions options
    formatter <-
        case Map.lookup format formatters of
            Nothing -> do
                printErr $ "Unknown format " ++ format
                printErr "Supported formats:"
                mapM_ (printErr . write) $ Map.keys formatters
                throwError SupportFailure
              where write s = "  " ++ s
            Just f -> ExceptT $ fmap Right f
    sys <- lift $ ioInterface options files
    lift $ runFormatter sys formatter options files

runFormatter :: SystemInterface IO -> Formatter -> Options -> [FilePath]
            -> IO Status
runFormatter sys format options files = do
    header format
    result <- foldM f NoProblems files
    footer format
    return result
  where
    f :: Status -> FilePath -> IO Status
    f status file = do
        newStatus <- process file `catch` handler file
        return $ status `mappend` newStatus
    handler :: FilePath -> IOException -> IO Status
    handler file e = reportFailure file (show e)
    reportFailure file str = do
        onFailure format file str
        return RuntimeException

    process :: FilePath -> IO Status
    process filename = do
        input <- (siReadFile sys) filename
        either (reportFailure filename) check input
      where
        check contents = do
            let checkspec = (checkSpec options) {
                csFilename = filename,
                csScript = contents
            }
            result <- checkScript sys checkspec
            onResult format result sys
            return $
                if null (crComments result)
                then NoProblems
                else SomeProblems

parseColorOption colorOption =
    case colorOption of
        "auto" -> ColorAuto
        "always" -> ColorAlways
        "never" -> ColorNever
        _ -> error $ "Bad value for --color `" ++ colorOption ++ "'"

parseOption flag options =
    case flag of
        Flag "shell" str ->
            fromMaybe (die $ "Unknown shell: " ++ str) $ do
                shell <- shellForExecutable str
                return $ return options {
                            checkSpec = (checkSpec options) {
                                csShellTypeOverride = Just shell
                            }
                        }

        Flag "exclude" str -> do
            new <- mapM parseNum $ split ',' str
            let old = csExcludedWarnings . checkSpec $ options
            return options {
                checkSpec = (checkSpec options) {
                    csExcludedWarnings = new ++ old
                }
            }

        Flag "version" _ -> do
            liftIO printVersion
            throwError NoProblems

        Flag "externals" _ ->
            return options {
                externalSources = True
            }

        Flag "color" color ->
            return options {
                formatterOptions = (formatterOptions options) {
                    foColorOption = parseColorOption color
                }
            }

        Flag "sourced" _ ->
            return options {
                checkSpec = (checkSpec options) {
                    csCheckSourced = True
                }
            }

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

ioInterface options files = do
    inputs <- mapM normalize files
    cache <- newIORef emptyCache
    return SystemInterface {
        siReadFile = get cache inputs
    }
  where
    emptyCache :: Map.Map FilePath String
    emptyCache = Map.empty
    get cache inputs file = do
        map <- readIORef cache
        case Map.lookup file map of
            Just x -> return $ Right x
            Nothing -> fetch cache inputs file

    fetch cache inputs file = do
        ok <- allowable inputs file
        if ok
          then (do
            (contents, shouldCache) <- inputFile file
            when shouldCache $
                modifyIORef cache $ Map.insert file contents
            return $ Right contents
            ) `catch` handler
          else return $ Left (file ++ " was not specified as input (see shellcheck -x).")

      where
        handler :: IOException -> IO (Either ErrorMessage String)
        handler ex = return . Left $ show ex

    allowable inputs x =
        if externalSources options
        then return True
        else do
            path <- normalize x
            return $ path `elem` inputs

    normalize x =
        canonicalizePath x `catch` fallback x
      where
        fallback :: FilePath -> IOException -> IO FilePath
        fallback path _ = return path

inputFile file = do
    (handle, shouldCache) <-
            if file == "-"
            then return (stdin, True)
            else do
                h <- openBinaryFile file ReadMode
                reopenable <- hIsSeekable h
                return (h, not reopenable)

    hSetBinaryMode handle True
    contents <- decodeString <$> hGetContents handle -- closes handle

    seq (length contents) $
        return (contents, shouldCache)

-- Decode a char8 string into a utf8 string, with fallback on
-- ISO-8859-1. This avoids depending on additional libraries.
decodeString = decode
  where
    decode [] = []
    decode (c:rest) | isAscii c = c : decode rest
    decode (c:rest) =
        let num = (fromIntegral $ ord c) :: Int
            next = case num of
                _ | num >= 0xF8 -> Nothing
                  | num >= 0xF0 -> construct (num .&. 0x07) 3 rest
                  | num >= 0xE0 -> construct (num .&. 0x0F) 2 rest
                  | num >= 0xC0 -> construct (num .&. 0x1F) 1 rest
                  | True -> Nothing
        in
            case next of
                Just (n, remainder) -> chr n : decode remainder
                Nothing -> c : decode rest

    construct x 0 rest = do
        guard $ x <= 0x10FFFF
        return (x, rest)
    construct x n (c:rest) =
        let num = (fromIntegral $ ord c) :: Int in
            if num >= 0x80 && num <= 0xBF
            then construct ((x `shiftL` 6) .|. (num .&. 0x3f)) (n-1) rest
            else Nothing
    construct _ _ _ = Nothing


verifyFiles files =
    when (null files) $ do
        printErr "No files specified.\n"
        printErr $ usageInfo usageHeader options
        throwError SyntaxFailure

printVersion = do
    putStrLn   "ShellCheck - shell script analysis tool"
    putStrLn $ "version: " ++ shellcheckVersion
    putStrLn   "license: GNU General Public License, version 3"
    putStrLn   "website: https://www.shellcheck.net"
