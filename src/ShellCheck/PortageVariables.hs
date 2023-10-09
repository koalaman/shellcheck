{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module ShellCheck.PortageVariables (
    readPortageVariables
  ) where

import ShellCheck.Regex

import Control.Exception
import Control.Monad
import Data.Maybe
import System.Directory (listDirectory)
import System.Exit (ExitCode(..))
import System.FilePath
import System.IO
import System.Process

import qualified Data.ByteString as B
import qualified Data.Map as M

type RepoName = String
type RepoPath = String
type EclassName = String
type EclassVar = String

-- | This is used for looking up what eclass variables are inherited,
--   keyed by the name of the eclass.
type EclassMap = M.Map EclassName [EclassVar]

data Repository = Repository
    { repositoryName :: RepoName
    , repositoryLocation :: RepoPath
    , repositoryEclasses :: [Eclass]
    } deriving (Show, Eq, Ord)

data Eclass = Eclass
    { eclassName :: EclassName
    , eclassVars :: [EclassVar]
    } deriving (Show, Eq, Ord)

readPortageVariables :: IO (M.Map String [String])
readPortageVariables = portageVariables <$> scanRepos

-- | Map from eclass names to a list of eclass variables
portageVariables :: [Repository] -> EclassMap
portageVariables = foldMap $ foldMap go . repositoryEclasses
  where
    go e = M.singleton (eclassName e) (eclassVars e)

-- | Run @portageq@ to gather a list of repo names and paths, then scan each
--   one for eclasses and ultimately eclass metadata.
scanRepos :: IO [Repository]
scanRepos = do
    let cmd = "portageq"
    let args = ["repos_config", "/"]
    out <- runOrDie cmd args
    forM (reposParser $ lines out) $ \(n,p) -> Repository n p <$> getEclasses p

-- | Get the name of the repo and its path from blocks outputted by
--   @portageq@. If the path doesn't exist, this will return @Nothing@.
reposParser :: [String] -> [(RepoName, RepoPath)]
reposParser = f ""
  where
    segmentRegex = mkRegex "^\\[(.*)\\].*"
    locationRegex = mkRegex "^[[:space:]]*location[[:space:]]*=[[:space:]]*(.*)[[:space:]]*$"
    f name [] = []
    f name (line:rest) =
        case (matchRegex segmentRegex line, matchRegex locationRegex line) of
            (Just [next], _) -> f next rest
            (_, Just [location]) -> (name, location) : f name rest
            _ -> f name rest

-- | Scan the repo path for @*.eclass@ files in @eclass/@, then run
--   'eclassParser' on each of them to produce @[Eclass]@.
--
--   If the @eclass/@ directory doesn't exist, the scan is skipped for that
--   repo.
getEclasses :: RepoPath -> IO [Eclass]
getEclasses repoLoc = do
    let eclassDir = repoLoc </> "eclass"

    files <- handle catcher $ listDirectory eclassDir
    let names = filter (\(_, e) -> e == ".eclass") $ map splitExtension files

    forM (names :: [(String, String)]) $ \(name, ext) -> do
        contents <- withFile (eclassDir </> name <.> ext) ReadMode readFully
        return $ Eclass name $ eclassParser (lines contents)

  where
    catcher :: IOException -> IO [String]
    catcher e = do
        hPutStrLn stderr $ "Unable to find .eclass files: " ++ show e
        return []

-- | Scan a @.eclass@ file for any @@@ECLASS_VARIABLE:@ comments, generating
--   a list of eclass variables.
eclassParser :: [String] -> [String]
eclassParser lines = mapMaybe match lines
  where
    varRegex = mkRegex "^[[:space:]]*#[[:space:]]*@ECLASS_VARIABLE:[[:space:]]*([^[:space:]]*)[[:space:]]*$"
    match str = head <$> matchRegex varRegex str

-- | Run the command and return the full stdout string (stdin is ignored).
--
--   If the command exits with a non-zero exit code, this will throw an
--   error including the captured contents of stdout and stderr.
runOrDie :: FilePath -> [String] -> IO String
runOrDie cmd args = bracket acquire release $ \(_,o,e,p) -> do
    ot <- readFully (fromJust o)
    et <- readFully (fromJust e)
    ec <- waitForProcess p
    case ec of
        ExitSuccess -> pure ot
        ExitFailure i -> fail $ unlines $ map unwords
            $   [ [ show cmd ]
                    ++ map show args
                    ++ [ "failed with exit code", show i]
                , [ "stdout:" ], [ ot ]
                , [ "stderr:" ], [ et ]
                ]
  where
    acquire = createProcess (proc cmd args)
        { std_in = NoStream
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
    release (i,o,e,p) = do
        _ <- waitForProcess p
        forM_ [i,o,e] $ mapM_ hClose

readFully :: Handle -> IO String
readFully handle = do
    hSetBinaryMode handle True
    str <- hGetContents handle
    length str `seq` return str
