
module ShellCheck.PortageVariables
    ( RepoName
    , RepoPath
    , EclassName
    , EclassVar
    , EclassMap
    , Repository(..)
    , Eclass(..)
    , portageVariables
    , scanRepos
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import System.Directory (listDirectory)
import System.Exit (ExitCode(..))
import System.FilePath
import System.Process
import Text.Parsec hiding ((<|>))
import Text.Parsec.String

type RepoName = String
type RepoPath = FilePath
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

-- | Map from eclass names to a list of eclass variables
portageVariables :: [Repository] -> EclassMap
portageVariables = foldMap $ foldMap go . repositoryEclasses
  where
    go e = M.singleton (eclassName e) (eclassVars e)

-- | Run @portageq@ to gather a list of repo names and paths, then scan each
--   one for eclasses and ultimately eclass metadata.
scanRepos :: IO [Repository]
scanRepos = do
    let cmd = "/usr/bin/portageq"
    let args = ["repos_config", "/"]
    out <- runOrDie cmd args
    case parse reposParser "scanRepos" out of
        Left pe -> fail $ show pe
        Right nps -> do
            forM nps $ \(n,p) -> Repository n p <$> getEclasses p

-- | Get the name of the repo and its path from blocks outputted by
--   @portageq@. If the path doesn't exist, this will return @Nothing@.
reposParser :: Parser [(RepoName, RepoPath)]
reposParser =
    choice
        [ [] <$ eof
        , repoName >>= repoBlock
        ]
  where
    -- Get the name of the repo at the top of the block
    repoName :: Parser RepoName
    repoName
        =  char '['
        *> manyTill anyChar (try (char ']'))
        <* endOfLine

    -- Parse the block for location field
    repoBlock :: RepoName -> Parser [(RepoName, RepoPath)]
    repoBlock n = choice
        [ try $ do
            l <- string "location = " *> takeLine
            -- Found the location, skip the rest of the block
            skipMany miscLine *> endOfBlock
            insert (n,l)
          -- Did not find the location, keep trying
        , try $ miscLine *> repoBlock n
          -- Reached the end of the block, no location field
        , endOfBlock *> ignore
        ]

    miscLine :: Parser ()
    miscLine = skipNonEmptyLine

    -- A block ends with an eol or eof
    endOfBlock :: Parser ()
    endOfBlock = void endOfLine <|> eof

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
    let eclassDir = repoLoc </> "eclass"

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
          try $ liftA2 (:) eclassVar eclassParser
          -- or skip the line and continue
        , skipLine *> eclassParser
          -- or end the list on eof
        , [] <$ eof
        ]
  where
    -- Scans for @ECLASS_VARIABLE comments rather than parsing the raw bash
    eclassVar :: Parser EclassVar
    eclassVar = string "# @ECLASS_VARIABLE: " *> takeLine

takeLine :: Parser String
takeLine = manyTill anyChar (try endOfLine)

-- | Fails if next char is 'endOfLine'
skipNonEmptyLine :: Parser ()
skipNonEmptyLine = notFollowedBy endOfLine *> skipLine

skipLine :: Parser ()
skipLine = void takeLine

-- | Run the command and return the full stdout string (stdin is ignored).
--
--   If the command exits with a non-zero exit code, this will throw an
--   error including the captured contents of stdout and stderr.
runOrDie :: FilePath -> [String] -> IO String
runOrDie cmd args = do
    (ec, o, e) <- readProcessWithExitCode cmd args ""
    case ec of
        ExitSuccess -> pure o
        ExitFailure i -> fail $ unlines $ map unwords
            $   [ [ show cmd ]
                    ++ map show args
                    ++ [ "failed with exit code", show i]
                , [ "stdout:" ], [ o ]
                , [ "stderr:" ], [ e ]
                ]
