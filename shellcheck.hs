{-
    Copyright 2012-2019 Vidar Holen

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
import qualified ShellCheck.Analyzer
import           ShellCheck.Checker
import           ShellCheck.Data
import           ShellCheck.Interface
import           ShellCheck.Regex

import qualified ShellCheck.Formatter.CheckStyle
import           ShellCheck.Formatter.Format
import qualified ShellCheck.Formatter.Diff
import qualified ShellCheck.Formatter.GCC
import qualified ShellCheck.Formatter.JSON
import qualified ShellCheck.Formatter.JSON1
import qualified ShellCheck.Formatter.TTY
import qualified ShellCheck.Formatter.Quiet

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Except
import           Data.Bits
import           Data.Char
import           Data.Either
import           Data.Functor
import           Data.IORef
import           Data.List
import qualified Data.Map                        as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Semigroup                  (Semigroup (..))
import           Prelude                         hiding (catch)
import           System.Console.GetOpt
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO

data Flag = Flag String String
data Status =
    NoProblems
    | SomeProblems
    | SupportFailure
    | SyntaxFailure
    | RuntimeException
  deriving (Ord, Eq, Show)

instance Semigroup Status where
    (<>) = max

instance Monoid Status where
    mempty = NoProblems
    mappend = (Data.Semigroup.<>)

data Options = Options {
    checkSpec        :: CheckSpec,
    externalSources  :: Bool,
    sourcePaths      :: [FilePath],
    formatterOptions :: FormatterOptions,
    minSeverity      :: Severity,
    rcfile           :: Maybe FilePath
}

defaultOptions = Options {
    checkSpec = emptyCheckSpec,
    externalSources = False,
    sourcePaths = [],
    formatterOptions = newFormatterOptions {
        foColorOption = ColorAuto
    },
    minSeverity = StyleC,
    rcfile = Nothing
}

usageHeader = "Usage: shellcheck [OPTIONS...] FILES..."
options = [
    Option "a" ["check-sourced"]
        (NoArg $ Flag "sourced" "false") "Include warnings from sourced files",
    Option "C" ["color"]
        (OptArg (maybe (Flag "color" "always") (Flag "color")) "WHEN")
        "Use color (auto, always, never)",
    Option "i" ["include"]
        (ReqArg (Flag "include") "CODE1,CODE2..") "Consider only given types of warnings",
    Option "e" ["exclude"]
        (ReqArg (Flag "exclude") "CODE1,CODE2..") "Exclude types of warnings",
    Option "" ["extended-analysis"]
        (ReqArg (Flag "extended-analysis") "bool") "Perform dataflow analysis (default true)",
    Option "f" ["format"]
        (ReqArg (Flag "format") "FORMAT") $
        "Output format (" ++ formatList ++ ")",
    Option "" ["list-optional"]
        (NoArg $ Flag "list-optional" "true") "List checks disabled by default",
    Option "" ["norc"]
        (NoArg $ Flag "norc" "true") "Don't look for .shellcheckrc files",
    Option "" ["rcfile"]
        (ReqArg (Flag "rcfile") "RCFILE")
        "Prefer the specified configuration file over searching for one",
    Option "o" ["enable"]
        (ReqArg (Flag "enable") "check1,check2..")
        "List of optional checks to enable (or 'all')",
    Option "P" ["source-path"]
        (ReqArg (Flag "source-path") "SOURCEPATHS")
        "Specify path when looking for sourced files (\"SCRIPTDIR\" for script's dir)",
    Option "s" ["shell"]
        (ReqArg (Flag "shell") "SHELLNAME")
        "Specify dialect (sh, bash, dash, ksh, busybox)",
    Option "S" ["severity"]
        (ReqArg (Flag "severity") "SEVERITY")
        "Minimum severity of errors to consider (error, warning, info, style)",
    Option "V" ["version"]
        (NoArg $ Flag "version" "true") "Print version information",
    Option "W" ["wiki-link-count"]
        (ReqArg (Flag "wiki-link-count") "NUM")
        "The number of wiki links to show, when applicable",
    Option "x" ["external-sources"]
        (NoArg $ Flag "externals" "true") "Allow 'source' outside of FILES",
    Option "" ["help"]
        (NoArg $ Flag "help" "true") "Show this usage summary and exit"
    ]
getUsageInfo = usageInfo usageHeader options

printErr = lift . hPutStrLn stderr

parseArguments :: [String] -> ExceptT Status IO ([Flag], [FilePath])
parseArguments argv =
    case getOpt Permute options argv of
        (opts, files, []) -> return (opts, files)
        (_, _, errors) -> do
            printErr $ concat errors ++ "\n" ++ getUsageInfo
            throwError SyntaxFailure

formats :: FormatterOptions -> Map.Map String (IO Formatter)
formats options = Map.fromList [
    ("checkstyle", ShellCheck.Formatter.CheckStyle.format),
    ("diff",  ShellCheck.Formatter.Diff.format options),
    ("gcc",  ShellCheck.Formatter.GCC.format),
    ("json", ShellCheck.Formatter.JSON.format),
    ("json1", ShellCheck.Formatter.JSON1.format),
    ("tty",  ShellCheck.Formatter.TTY.format options),
    ("quiet",  ShellCheck.Formatter.Quiet.format options)
    ]

formatList = intercalate ", " names
  where
    names = Map.keys $ formats (formatterOptions defaultOptions)

getOption [] _                  = Nothing
getOption (Flag var val:_) name | name == var = return val
getOption (_:rest) flag         = getOption rest flag

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
        NoProblems       -> ExitSuccess
        SomeProblems     -> ExitFailure 1
        SyntaxFailure    -> ExitFailure 3
        SupportFailure   -> ExitFailure 4
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
        return $! status `mappend` newStatus
    handler :: FilePath -> IOException -> IO Status
    handler file e = reportFailure file (show e)
    reportFailure file str = do
        onFailure format file str
        return RuntimeException

    process :: FilePath -> IO Status
    process filename = do
        input <- siReadFile sys Nothing filename
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

parseEnum name value list =
    case lookup value list of
        Just value -> return value
        Nothing -> do
            printErr $ "Unknown value for --" ++ name ++ ". " ++
                       "Valid options are: " ++ (intercalate ", " $ map fst list)
            throwError SupportFailure

parseColorOption value =
    parseEnum "color" value [
        ("auto",   ColorAuto),
        ("always", ColorAlways),
        ("never",  ColorNever)
        ]

parseSeverityOption value =
    parseEnum "severity" value [
        ("error",   ErrorC),
        ("warning", WarningC),
        ("info",    InfoC),
        ("style",   StyleC)
        ]

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
            new <- mapM parseNum $ filter (not . null) $ split ',' str
            let old = csExcludedWarnings . checkSpec $ options
            return options {
                checkSpec = (checkSpec options) {
                    csExcludedWarnings = new ++ old
                }
            }

        Flag "include" str -> do
            new <- mapM parseNum $ filter (not . null) $ split ',' str
            let old = csIncludedWarnings . checkSpec $ options
            return options {
                checkSpec = (checkSpec options) {
                    csIncludedWarnings =
                      if null new
                        then old
                        else Just new `mappend` old
                }
            }

        Flag "version" _ -> do
            liftIO printVersion
            throwError NoProblems

        Flag "list-optional" _ -> do
            liftIO printOptional
            throwError NoProblems

        Flag "help" _ -> do
            liftIO $ putStrLn getUsageInfo
            throwError NoProblems

        Flag "externals" _ ->
            return options {
                externalSources = True
            }

        Flag "color" color -> do
            option <- parseColorOption color
            return options {
                formatterOptions = (formatterOptions options) {
                    foColorOption = option
                }
            }

        Flag "source-path" str -> do
            let paths = splitSearchPath str
            return options {
                sourcePaths = (sourcePaths options) ++ paths
            }

        Flag "sourced" _ ->
            return options {
                checkSpec = (checkSpec options) {
                    csCheckSourced = True
                }
            }

        Flag "severity" severity -> do
            option <- parseSeverityOption severity
            return options {
                checkSpec = (checkSpec options) {
                    csMinSeverity = option
                }
            }

        Flag "wiki-link-count" countString -> do
            count <- parseNum countString
            return options {
                formatterOptions = (formatterOptions options) {
                    foWikiLinkCount = count
                }
            }

        Flag "norc" _ ->
            return options {
                checkSpec = (checkSpec options) {
                    csIgnoreRC = True
                }
            }

        Flag "rcfile" str -> do
            return options {
                rcfile = Just str
            }

        Flag "enable" value ->
            let cs = checkSpec options in return options {
                checkSpec = cs {
                    csOptionalChecks = (csOptionalChecks cs) ++ split ',' value
                }
            }

        Flag "extended-analysis" str -> do
            value <- parseBool str
            return options {
                checkSpec = (checkSpec options) {
                    csExtendedAnalysis = Just value
                }
            }

        -- This flag is handled specially in 'process'
        Flag "format" _ -> return options

        Flag str _ -> do
            printErr $ "Internal error for --" ++ str ++ ". Please file a bug :("
            return options
  where
    die s = do
        printErr s
        throwError SupportFailure
    parseNum ('S':'C':str) = parseNum str
    parseNum num = do
        unless (all isDigit num) $ do
            printErr $ "Invalid number: " ++ num
            throwError SyntaxFailure
        return (Prelude.read num :: Integer)

    parseBool str = do
        case str of
            "true" -> return True
            "false" -> return False
            _ -> do
                printErr $ "Invalid boolean, expected true/false: " ++ str
                throwError SyntaxFailure

ioInterface :: Options -> [FilePath] -> IO (SystemInterface IO)
ioInterface options files = do
    inputs <- mapM normalize files
    cache <- newIORef emptyCache
    configCache <- newIORef ("", Nothing)
    return (newSystemInterface :: SystemInterface IO) {
        siReadFile = get cache inputs,
        siFindSource = findSourceFile inputs (sourcePaths options),
        siGetConfig = getConfig configCache
    }
  where
    emptyCache :: Map.Map FilePath String
    emptyCache = Map.empty

    get cache inputs rcSuggestsExternal file = do
        map <- readIORef cache
        case Map.lookup file map of
            Just x  -> return $ Right x
            Nothing -> fetch cache inputs rcSuggestsExternal file

    fetch cache inputs rcSuggestsExternal file = do
        ok <- allowable rcSuggestsExternal inputs file
        if ok
          then (do
            (contents, shouldCache) <- inputFile file
            when shouldCache $
                modifyIORef cache $ Map.insert file contents
            return $ Right contents
            ) `catch` handler
          else
            if rcSuggestsExternal == Just False
            then return $ Left (file ++ " was not specified as input, and external files were disabled via directive.")
            else return $ Left (file ++ " was not specified as input (see shellcheck -x).")
      where
        handler :: IOException -> IO (Either ErrorMessage String)
        handler ex = return . Left $ show ex

    allowable rcSuggestsExternal inputs x =
        if fromMaybe (externalSources options) rcSuggestsExternal
        then return True
        else do
            path <- normalize x
            return $ path `elem` inputs

    normalize x =
        canonicalizePath x `catch` fallback x
      where
        fallback :: FilePath -> IOException -> IO FilePath
        fallback path _ = return path


    -- Returns the name and contents of .shellcheckrc for the given file
    getConfig cache filename =
        case rcfile options of
            Just file -> do
                -- We have a specified rcfile. Ignore normal rcfile resolution.
                (path, result) <- readIORef cache
                if path == "/"
                  then return result
                  else do
                    result <- readConfig file
                    when (isNothing result) $
                        hPutStrLn stderr $ "Warning: unable to read --rcfile " ++ file
                    writeIORef cache ("/", result)
                    return result

            Nothing -> do
                path <- normalize filename
                let dir = takeDirectory path
                (previousPath, result) <- readIORef cache
                if dir == previousPath
                  then return result
                  else do
                    paths <- getConfigPaths dir
                    result <- findConfig paths
                    writeIORef cache (dir, result)
                    return result

    findConfig paths =
        case paths of
            (file:rest) -> do
                contents <- readConfig file
                if isJust contents
                  then return contents
                  else findConfig rest
            [] -> return Nothing

    -- Get a list of candidate filenames. This includes .shellcheckrc
    -- in all parent directories, plus the user's home dir and xdg dir.
    -- The dot is optional for Windows and Snap users.
    getConfigPaths dir = do
        let next = takeDirectory dir
        rest <- if next /= dir
                then getConfigPaths next
                else defaultPaths `catch`
                        ((const $ return []) :: IOException -> IO [FilePath])
        return $ (dir </> ".shellcheckrc") : (dir </> "shellcheckrc") : rest

    defaultPaths = do
        home <- getAppUserDataDirectory "shellcheckrc"
        xdg <- getXdgDirectory XdgConfig "shellcheckrc"
        return [home, xdg]

    readConfig file = do
        exists <- doesFileExist file
        if exists
          then do
            (contents, _) <- inputFile file `catch` handler file
            return $ Just (file, contents)
          else
            return Nothing
      where
        handler :: FilePath -> IOException -> IO (String, Bool)
        handler file err = do
            hPutStrLn stderr $ file ++ ": " ++ show err
            return ("", True)

    andM a b arg = do
        first <- a arg
        if not first then return False else b arg

    findM p = foldr go (pure Nothing)
      where
        go x acc = do
            b <- p x
            if b then pure (Just x) else acc

    findSourceFile inputs sourcePathFlag currentScript rcSuggestsExternal sourcePathAnnotation original =
        if isAbsolute original
        then
            let (_, relative) = splitDrive original
            in find relative original
        else
            find original original
      where
        find filename deflt = do
            sources <- findM ((allowable rcSuggestsExternal inputs) `andM` doesFileExist) $
                        (adjustPath filename):(map ((</> filename) . adjustPath) $ sourcePathFlag ++ sourcePathAnnotation)
            case sources of
                Nothing -> return deflt
                Just first -> return first
        scriptdir = dropFileName currentScript
        adjustPath str =
            case (splitDirectories str) of
                ("SCRIPTDIR":rest) -> joinPath (scriptdir:rest)
                _ -> str

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
                Nothing             -> c : decode rest

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

printOptional = do
    mapM f list
  where
    list = sortOn cdName ShellCheck.Analyzer.optionalChecks
    f item = do
        putStrLn $ "name:    " ++ cdName item
        putStrLn $ "desc:    " ++ cdDescription item
        putStrLn $ "example: " ++ cdPositive item
        putStrLn $ "fix:     " ++ cdNegative item
        putStrLn ""
