{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module ShellCheck.PortageVariables (
    readPortageVariables
  ) where

import Control.Applicative
import Control.Exception (bracket)
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString.Char8 hiding (takeWhile)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Char (ord)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import System.Directory (listDirectory)
import System.Exit (ExitCode(..))
import System.FilePath
import System.IO (hClose)
import System.Process

import Prelude hiding (takeWhile)

type RepoName = ByteString
type RepoPath = ByteString
type EclassName = String
type EclassVar = ByteString

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
readPortageVariables = M.map (map decodeLenient) <$> portageVariables <$> scanRepos

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
    case parseOnly reposParser out of
        Left pe -> fail $ show pe
        Right nps -> do
            forM nps $ \(n,p) -> Repository n p <$> getEclasses p

-- | Get the name of the repo and its path from blocks outputted by
--   @portageq@. If the path doesn't exist, this will return @Nothing@.
reposParser :: Parser [(RepoName, RepoPath)]
reposParser =
    choice
        [ [] <$ endOfInput
        , repoName >>= repoBlock
        ]
  where
    -- Get the name of the repo at the top of the block
    repoName :: Parser RepoName
    repoName = do
        _ <- char '['
        n <- takeWhile (/= fromIntegral (ord ']'))
        _ <- char ']'
        _ <- endOfLine
        pure n

    -- Parse the block for location field
    repoBlock :: RepoName -> Parser [(RepoName, RepoPath)]
    repoBlock n = choice
        [ do
            l <- "location = " *> takeLine
            -- Found the location, skip the rest of the block
            skipMany miscLine *> endOfBlock
            insert (n,l)
          -- Did not find the location, keep trying
        , miscLine *> repoBlock n
          -- Reached the end of the block, no location field
        , endOfBlock *> ignore
        ]

    miscLine :: Parser ()
    miscLine = skipNonEmptyLine

    -- A block either ends with an empty line or eof
    endOfBlock :: Parser ()
    endOfBlock = endOfLine <|> endOfInput

    -- cons the repo and continue parsing
    insert :: (RepoName, RepoPath) -> Parser [(RepoName, RepoPath)]
    insert r = (r:) <$> reposParser

    -- skip the repo and continue parsing
    ignore :: Parser [(RepoName, RepoPath)]
    ignore = reposParser

-- | Scan the repo path for @*.eclass@ files in @eclass/@, then run
--   'eclassParser' on each of them to produce @[Eclass]@.
--
--   If the @eclass/@ directory doesn't exist, the scan is skipped for that
--   repo.
getEclasses :: RepoPath -> IO [Eclass]
getEclasses repoLoc = fmap (maybe [] id) $ runMaybeT $ do
    let eclassDir = (decodeLenient repoLoc) </> "eclass"

    -- Silently fail if the repo doesn't have an eclass dir
    fs <- MaybeT $ Just <$> listDirectory eclassDir <|> pure Nothing
    let fs' = filter (\(_,e) -> e == ".eclass") $ map splitExtensions fs

    forM fs' $ \(n,e) -> do
        evs <- lift $ parseFromFile eclassParser (eclassDir </> n <.> e)
        case evs of
            Left pe -> lift $ fail $ show pe
            Right vs -> pure $ Eclass n vs

-- | Scan a @.eclass@ file for any @@@ECLASS_VARIABLE:@ comments, generating
--   a list of eclass variables.
eclassParser :: Parser [EclassVar]
eclassParser = choice
        [ -- cons the EclassVar to the list and continue
          liftA2 (:) eclassVar eclassParser
          -- or skip the line and continue
        , skipLine *> eclassParser
          -- or end the list on eof
        , [] <$ endOfInput
        ]
  where
    -- Scans for @ECLASS_VARIABLE comments rather than parsing the raw bash
    eclassVar :: Parser EclassVar
    eclassVar = "# @ECLASS_VARIABLE: " *> takeLine

takeLine :: Parser ByteString
takeLine = A.takeWhile (not . isEndOfLine) <* endOfLine

-- | Fails if next char is 'endOfLine'
skipNonEmptyLine :: Parser ()
skipNonEmptyLine = A.satisfy (not . isEndOfLine) *> skipLine

skipLine :: Parser ()
skipLine = A.skipWhile (not . isEndOfLine) <* endOfLine

parseFromFile :: Parser a -> FilePath -> IO (Either String a)
parseFromFile p = fmap (parseOnly p) . B.readFile

-- | Run the command and return the full stdout string (stdin is ignored).
--
--   If the command exits with a non-zero exit code, this will throw an
--   error including the captured contents of stdout and stderr.
runOrDie :: FilePath -> [String] -> IO ByteString
runOrDie cmd args = bracket acquire release $ \(_,o,e,p) -> do
    ot <- B.hGetContents (fromJust o)
    et <- B.hGetContents (fromJust e)
    ec <- waitForProcess p
    case ec of
        ExitSuccess -> pure ot
        ExitFailure i -> fail $ unlines $ map unwords
            $   [ [ show cmd ]
                    ++ map show args
                    ++ [ "failed with exit code", show i]
                , [ "stdout:" ], [ decodeLenient ot ]
                , [ "stderr:" ], [ decodeLenient et ]
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

decodeLenient :: ByteString -> String
decodeLenient = T.unpack . T.decodeUtf8With T.lenientDecode
