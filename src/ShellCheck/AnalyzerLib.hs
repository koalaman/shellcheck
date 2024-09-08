{-
    Copyright 2012-2022 Vidar Holen

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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
module ShellCheck.AnalyzerLib where

import ShellCheck.AST
import ShellCheck.ASTLib
import qualified ShellCheck.CFGAnalysis as CF
import ShellCheck.Data
import ShellCheck.Interface
import ShellCheck.Parser
import ShellCheck.Prelude
import ShellCheck.Regex

import Control.Arrow (first)
import Control.DeepSeq
import Control.Monad
import Control.Monad.Identity
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Writer
import Data.Char
import Data.List
import Data.Maybe
import Data.Semigroup
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map

import Test.QuickCheck.All (forAllProperties)
import Test.QuickCheck.Test (maxSuccess, quickCheckWithResult, stdArgs)

type Analysis = AnalyzerM ()
type AnalyzerM a = RWS Parameters [TokenComment] Cache a
nullCheck = const $ return ()


data Checker = Checker {
    perScript :: Root -> Analysis,
    perToken  :: Token -> Analysis
}

runChecker :: Parameters -> Checker -> [TokenComment]
runChecker params checker = notes
    where
        root = rootNode params
        check = perScript checker `composeAnalyzers` (\(Root x) -> void $ doAnalysis (perToken checker) x)
        notes = snd $ evalRWS (check $ Root root) params Cache

instance Semigroup Checker where
    (<>) x y = Checker {
        perScript = perScript x `composeAnalyzers` perScript y,
        perToken = perToken x `composeAnalyzers` perToken y
        }

instance Monoid Checker where
    mempty = Checker {
        perScript = nullCheck,
        perToken = nullCheck
        }
    mappend = (Data.Semigroup.<>)

composeAnalyzers :: (a -> Analysis) -> (a -> Analysis) -> a -> Analysis
composeAnalyzers f g x = f x >> g x

data Parameters = Parameters {
    -- Whether this script has the 'lastpipe' option set/default.
    hasLastpipe        :: Bool,
    -- Whether this script has the 'inherit_errexit' option set/default.
    hasInheritErrexit  :: Bool,
    -- Whether this script has 'set -e' anywhere.
    hasSetE            :: Bool,
    -- Whether this script has 'set -o pipefail' anywhere.
    hasPipefail        :: Bool,
    -- A linear (bad) analysis of data flow
    variableFlow       :: [StackData],
    -- A map from Id to Token
    idMap              :: Map.Map Id Token,
    -- A map from Id to parent Token
    parentMap          :: Map.Map Id Token,
    -- The shell type, such as Bash or Ksh
    shellType          :: Shell,
    -- True if shell type was forced via flags
    shellTypeSpecified :: Bool,
    -- The root node of the AST
    rootNode           :: Token,
    -- map from token id to start and end position
    tokenPositions     :: Map.Map Id (Position, Position),
    -- Result from Control Flow Graph analysis (including data flow analysis)
    cfgAnalysis :: Maybe CF.CFGAnalysis
    } deriving (Show)

-- TODO: Cache results of common AST ops here
data Cache = Cache {}

data Scope = SubshellScope String | NoneScope deriving (Show, Eq)
data StackData =
    StackScope Scope
    | StackScopeEnd
    -- (Base expression, specific position, var name, assigned values)
    | Assignment (Token, Token, String, DataType)
    | Reference (Token, Token, String)
  deriving (Show)

data DataType = DataString DataSource | DataArray DataSource
  deriving (Show)

data DataSource =
    SourceFrom [Token]
    | SourceExternal
    | SourceDeclaration
    | SourceInteger
    | SourceChecked
  deriving (Show)

data VariableState = Dead Token String | Alive deriving (Show)

defaultSpec pr = spec {
    asShellType = Nothing,
    asCheckSourced = False,
    asExecutionMode = Executed,
    asTokenPositions = prTokenPositions pr
} where spec = newAnalysisSpec (fromJust $ prRoot pr)

pScript s =
  let
    pSpec = newParseSpec {
        psFilename = "script",
        psScript = s
    }
  in runIdentity $ parseScript (mockedSystemInterface []) pSpec

-- For testing. If parsed, returns whether there are any comments
producesComments :: Checker -> String -> Maybe Bool
producesComments c s = do
        let pr = pScript s
        prRoot pr
        let spec = defaultSpec pr
        let params = makeParameters spec
        return . not . null $ filterByAnnotation spec params $ runChecker params c

makeComment :: Severity -> Id -> Code -> String -> TokenComment
makeComment severity id code note =
    newTokenComment {
        tcId = id,
        tcComment = newComment {
            cSeverity = severity,
            cCode = code,
            cMessage = note
        }
    }

addComment note = note `deepseq` tell [note]

warn :: MonadWriter [TokenComment] m => Id -> Code -> String -> m ()
warn  id code str = addComment $ makeComment WarningC id code str
err   id code str = addComment $ makeComment ErrorC id code str
info  id code str = addComment $ makeComment InfoC id code str
style id code str = addComment $ makeComment StyleC id code str

errWithFix :: MonadWriter [TokenComment] m => Id -> Code -> String -> Fix -> m ()
errWithFix  = addCommentWithFix ErrorC
warnWithFix :: MonadWriter [TokenComment] m => Id -> Code -> String -> Fix -> m ()
warnWithFix  = addCommentWithFix WarningC
infoWithFix :: MonadWriter [TokenComment] m => Id -> Code -> String -> Fix -> m ()
infoWithFix = addCommentWithFix InfoC
styleWithFix :: MonadWriter [TokenComment] m => Id -> Code -> String -> Fix -> m ()
styleWithFix = addCommentWithFix StyleC

addCommentWithFix :: MonadWriter [TokenComment] m => Severity -> Id -> Code -> String -> Fix -> m ()
addCommentWithFix severity id code str fix =
    addComment $ makeCommentWithFix severity id code str fix

makeCommentWithFix :: Severity -> Id -> Code -> String -> Fix -> TokenComment
makeCommentWithFix severity id code str fix =
    let comment = makeComment severity id code str
        withFix = comment {
            -- If fix is empty, pretend it wasn't there.
            tcFix = if null (fixReplacements fix) then Nothing else Just fix
        }
    in force withFix

-- makeParameters :: CheckSpec -> Parameters
makeParameters spec = params
  where
    extendedAnalysis = fromMaybe True $ msum [asExtendedAnalysis spec, getExtendedAnalysisDirective root]
    params = Parameters {
        rootNode = root,
        shellType = fromMaybe (determineShell (asFallbackShell spec) root) $ asShellType spec,
        hasSetE = containsSetE root,
        hasLastpipe =
            case shellType params of
                Bash -> isOptionSet "lastpipe" root
                Dash -> False
                BusyboxSh -> False
                Sh   -> False
                Ksh  -> True,
        hasInheritErrexit =
            case shellType params of
                Bash -> isOptionSet "inherit_errexit" root
                Dash -> True
                BusyboxSh -> True
                Sh   -> True
                Ksh  -> False,
        hasPipefail =
            case shellType params of
                Bash -> isOptionSet "pipefail" root
                Dash -> True
                BusyboxSh -> isOptionSet "pipefail" root
                Sh   -> True
                Ksh  -> isOptionSet "pipefail" root,
        shellTypeSpecified = isJust (asShellType spec) || isJust (asFallbackShell spec),
        idMap = getTokenMap root,
        parentMap = getParentTree root,
        variableFlow = getVariableFlow params root,
        tokenPositions = asTokenPositions spec,
        cfgAnalysis = do
            guard extendedAnalysis
            return $ CF.analyzeControlFlow cfParams root
    }
    cfParams = CF.CFGParameters {
        CF.cfLastpipe = hasLastpipe params,
        CF.cfPipefail = hasPipefail params
    }
    root = asScript spec


-- Does this script mention 'set -e' anywhere?
-- Used as a hack to disable certain warnings.
containsSetE root = isNothing $ doAnalysis (guard . not . isSetE) root
  where
    isSetE t =
        case t of
            T_Script _ (T_Literal _ str) _ -> str `matches` re
            T_SimpleCommand {}  ->
                t `isUnqualifiedCommand` "set" &&
                    ("errexit" `elem` oversimplify t ||
                        "e" `elem` map snd (getAllFlags t))
            _ -> False
    re = mkRegex "[[:space:]]-[^-]*e"


containsSetOption opt root = isNothing $ doAnalysis (guard . not . isPipefail) root
  where
    isPipefail t =
        case t of
            T_SimpleCommand {}  ->
                t `isUnqualifiedCommand` "set" &&
                    (opt `elem` oversimplify t ||
                        "o" `elem` map snd (getAllFlags t))
            _ -> False

containsShopt shopt root =
        isNothing $ doAnalysis (guard . not . isShoptLastPipe) root
    where
        isShoptLastPipe t =
            case t of
                T_SimpleCommand {}  ->
                    t `isUnqualifiedCommand` "shopt" &&
                        (shopt `elem` oversimplify t)
                _ -> False

-- Does this script mention 'shopt -s $opt' or 'set -o $opt' anywhere?
isOptionSet opt root = containsShopt opt root || containsSetOption opt root


prop_determineShell0 = determineShellTest "#!/bin/sh" == Sh
prop_determineShell1 = determineShellTest "#!/usr/bin/env ksh" == Ksh
prop_determineShell2 = determineShellTest "" == Bash
prop_determineShell3 = determineShellTest "#!/bin/sh -e" == Sh
prop_determineShell4 = determineShellTest "#!/bin/ksh\n#shellcheck shell=sh\nfoo" == Sh
prop_determineShell5 = determineShellTest "#shellcheck shell=sh\nfoo" == Sh
prop_determineShell6 = determineShellTest "#! /bin/sh" == Sh
prop_determineShell7 = determineShellTest "#! /bin/ash" == Dash
prop_determineShell8 = determineShellTest' (Just Ksh) "#!/bin/sh" == Sh
prop_determineShell9 = determineShellTest "#!/bin/env -S dash -x" == Dash
prop_determineShell10 = determineShellTest "#!/bin/env --split-string= dash -x" == Dash
prop_determineShell11 = determineShellTest "#!/bin/busybox sh" == BusyboxSh -- busybox sh is a specific shell, not posix sh
prop_determineShell12 = determineShellTest "#!/bin/busybox ash" == BusyboxSh

determineShellTest = determineShellTest' Nothing
determineShellTest' fallbackShell = determineShell fallbackShell . fromJust . prRoot . pScript
determineShell fallbackShell t = fromMaybe Bash $
    shellForExecutable shellString `mplus` fallbackShell
  where
    shellString = getCandidate t
    getCandidate :: Token -> String
    getCandidate t@T_Script {} = fromShebang t
    getCandidate (T_Annotation _ annotations s) =
        headOrDefault (fromShebang s) [s | ShellOverride s <- annotations]
    fromShebang (T_Script _ (T_Literal _ s) _) = executableFromShebang s

-- Given a root node, make a map from Id to parent Token.
-- This is used to populate parentMap in Parameters
getParentTree :: Token -> Map.Map Id Token
getParentTree t =
    snd $ execState (doStackAnalysis pre post t) ([], Map.empty)
  where
    pre t = modify (first ((:) t))
    post t = do
        (x, map) <- get
        case x of
          _:rest -> case rest of []    -> put (rest, map)
                                 (x:_) -> put (rest, Map.insert (getId t) x map)

-- Given a root node, make a map from Id to Token
getTokenMap :: Token -> Map.Map Id Token
getTokenMap t =
    execState (doAnalysis f t) Map.empty
  where
    f t = modify (Map.insert (getId t) t)


-- Is this token in a quoting free context? (i.e. would variable expansion split)
-- True:  Assignments, [[ .. ]], here docs, already in double quotes
-- False: Regular words
isStrictlyQuoteFree = isQuoteFreeNode True

-- Like above, but also allow some cases where splitting may be desired.
-- True:  Like above + for loops
-- False: Like above
isQuoteFree = isQuoteFreeNode False


isQuoteFreeNode strict shell tree t =
    isQuoteFreeElement t ||
        (fromMaybe False $ msum $ map isQuoteFreeContext $ NE.tail $ getPath tree t)
  where
    -- Is this node self-quoting in itself?
    isQuoteFreeElement t =
        case t of
            T_Assignment id _ _ _ _ -> assignmentIsQuoting id
            T_FdRedirect {}         -> True
            _                       -> False

    -- Are any subnodes inherently self-quoting?
    isQuoteFreeContext t =
        case t of
            TC_Nullary _ DoubleBracket _    -> return True
            TC_Unary _ DoubleBracket _ _    -> return True
            TC_Binary _ DoubleBracket _ _ _ -> return True
            TA_Sequence {}                  -> return True
            T_Arithmetic {}                 -> return True
            T_Assignment id _ _ _ _         -> return $ assignmentIsQuoting id
            T_Redirecting {}                -> return False
            T_DoubleQuoted _ _              -> return True
            T_DollarDoubleQuoted _ _        -> return True
            T_CaseExpression {}             -> return True
            T_HereDoc {}                    -> return True
            T_DollarBraced {}               -> return True
            -- When non-strict, pragmatically assume it's desirable to split here
            T_ForIn {}                      -> return (not strict)
            T_SelectIn {}                   -> return (not strict)
            _                               -> Nothing

    -- Check whether this assignment is self-quoting due to being a recognized
    -- assignment passed to a Declaration Utility. This will soon be required
    -- by POSIX: https://austingroupbugs.net/view.php?id=351
    assignmentIsQuoting id = shellParsesParamsAsAssignments || not (isAssignmentParamToCommand id)
    shellParsesParamsAsAssignments = shell /= Sh

    -- Is this assignment a parameter to a command like export/typeset/etc?
    isAssignmentParamToCommand id =
        case Map.lookup id tree of
            Just (T_SimpleCommand _ _ (_:args)) -> id `elem` (map getId args)
            _ -> False

-- Check if a token is a parameter to a certain command by name:
-- Example: isParamTo (parentMap params) "sed" t
isParamTo :: Map.Map Id Token -> String -> Token -> Bool
isParamTo tree cmd =
    go
  where
    go x = case Map.lookup (getId x) tree of
                Nothing     -> False
                Just parent -> check parent
    check t =
        case t of
            T_SingleQuoted _ _ -> go t
            T_DoubleQuoted _ _ -> go t
            T_NormalWord _ _   -> go t
            T_SimpleCommand {} -> isCommand t cmd
            T_Redirecting {}   -> isCommand t cmd
            _                  -> False

-- Get the parent command (T_Redirecting) of a Token, if any.
getClosestCommand :: Map.Map Id Token -> Token -> Maybe Token
getClosestCommand tree t =
    findFirst findCommand $ NE.toList $ getPath tree t
  where
    findCommand t =
        case t of
            T_Redirecting {} -> return True
            T_Script {}      -> return False
            _                -> Nothing

-- Like above, if koala_man knew Haskell when starting this project.
getClosestCommandM t = do
    params <- ask
    return $ getClosestCommand (parentMap params) t

-- Is the token used as a command name (the first word in a T_SimpleCommand)?
usedAsCommandName tree token = go (getId token) (NE.tail $ getPath tree token)
  where
    go currentId (T_NormalWord id [word]:rest)
        | currentId == getId word = go id rest
    go currentId (T_DoubleQuoted id [word]:rest)
        | currentId == getId word = go id rest
    go currentId (t@(T_SimpleCommand _ _ (word:_)):_) =
        getId word == currentId || getId (getCommandTokenOrThis t) == currentId
    go _ _ = False

-- Version of the above taking the map from the current context
-- Todo: give this the name "getPath"
getPathM t = do
    params <- ask
    return $ getPath (parentMap params) t

isParentOf tree parent child =
    any (\t -> parentId == getId t) (getPath tree child)
  where
    parentId = getId parent

parents params = getPath (parentMap params)

-- Find the first match in a list where the predicate is Just True.
-- Stops if it's Just False and ignores Nothing.
findFirst :: (a -> Maybe Bool) -> [a] -> Maybe a
findFirst p = foldr go Nothing
  where
    go x acc =
      case p x of
        Just True  -> return x
        Just False -> Nothing
        Nothing    -> acc

-- Check whether a word is entirely output from a single command
tokenIsJustCommandOutput t = case t of
    T_NormalWord id [T_DollarExpansion _ cmds] -> check cmds
    T_NormalWord id [T_DoubleQuoted _ [T_DollarExpansion _ cmds]] -> check cmds
    T_NormalWord id [T_Backticked _ cmds] -> check cmds
    T_NormalWord id [T_DoubleQuoted _ [T_Backticked _ cmds]] -> check cmds
    _ -> False
  where
    check [x] = not $ isOnlyRedirection x
    check _   = False

-- TODO: Replace this with a proper Control Flow Graph
getVariableFlow params t =
    reverse $ execState (doStackAnalysis startScope endScope t) []
  where
    startScope t =
        let scopeType = leadType params t
        in do
            when (scopeType /= NoneScope) $ modify (StackScope scopeType:)
            when (assignFirst t) $ setWritten t

    endScope t =
        let scopeType = leadType params t
        in do
            setRead t
            unless (assignFirst t) $ setWritten t
            when (scopeType /= NoneScope) $ modify (StackScopeEnd:)

    assignFirst T_ForIn {}    = True
    assignFirst T_SelectIn {} = True
    assignFirst (T_BatsTest {}) = True
    assignFirst _             = False

    setRead t =
        let read    = getReferencedVariables (parentMap params) t
        in mapM_ (\v -> modify (Reference v:)) read

    setWritten t =
        let written = getModifiedVariables t
        in mapM_ (\v -> modify (Assignment v:)) written


leadType params t =
    case t of
        T_DollarExpansion _ _  -> SubshellScope "$(..) expansion"
        T_Backticked _ _  -> SubshellScope "`..` expansion"
        T_Backgrounded _ _  -> SubshellScope "backgrounding &"
        T_Subshell _ _  -> SubshellScope "(..) group"
        T_BatsTest {} -> SubshellScope "@bats test"
        T_CoProcBody _ _  -> SubshellScope "coproc"
        T_Redirecting {}  ->
            if causesSubshell == Just True
            then SubshellScope "pipeline"
            else NoneScope
        _ -> NoneScope
  where
    parentPipeline = do
        parent <- Map.lookup (getId t) (parentMap params)
        case parent of
            T_Pipeline {} -> return parent
            _             -> Nothing

    causesSubshell = do
        (T_Pipeline _ _ list) <- parentPipeline
        return $ case list of
            _:_:_ -> not (hasLastpipe params) || getId (last list) /= getId t
            _ -> False

getModifiedVariables t =
    case t of
        T_SimpleCommand _ vars [] ->
            [(x, x, name, dataTypeFrom DataString w) | x@(T_Assignment id _ name _ w) <- vars]
        T_SimpleCommand {} ->
            getModifiedVariableCommand t

        TA_Unary _ op v@(TA_Variable _ name _) | "--" `isInfixOf` op || "++" `isInfixOf` op ->
            [(t, v, name, DataString SourceInteger)]
        TA_Assignment _ op (TA_Variable _ name _) rhs -> do
            guard $ op `elem` ["=", "*=", "/=", "%=", "+=", "-=", "<<=", ">>=", "&=", "^=", "|="]
            return (t, t, name, DataString SourceInteger)

        T_BatsTest {} -> [
            (t, t, "lines", DataArray SourceExternal),
            (t, t, "status", DataString SourceInteger),
            (t, t, "output", DataString SourceExternal),
            (t, t, "stderr", DataString SourceExternal),
            (t, t, "stderr_lines", DataArray SourceExternal)
            ]

        -- Count [[ -v foo ]] as an "assignment".
        -- This is to prevent [ -v foo ] being unassigned or unused.
        TC_Unary id _ "-v" token -> maybeToList $ do
            str <- getVariableForTestDashV token
            return (t, token, str, DataString SourceChecked)

        TC_Unary _ _ "-n" token -> markAsChecked t token
        TC_Unary _ _ "-z" token -> markAsChecked t token
        TC_Nullary _ _ token -> markAsChecked t token

        T_DollarBraced _ _ l -> maybeToList $ do
            let string = concat $ oversimplify l
            let modifier = getBracedModifier string
            guard $ any (`isPrefixOf` modifier) ["=", ":="]
            return (t, t, getBracedReference string, DataString $ SourceFrom [l])

        T_FdRedirect _ ('{':var) op -> -- {foo}>&2 modifies foo
            [(t, t, takeWhile (/= '}') var, DataString SourceInteger) | not $ isClosingFileOp op]

        T_CoProc _ Nothing _ ->
            [(t, t, "COPROC", DataArray SourceInteger)]

        T_CoProc _ (Just token) _ -> do
            name <- maybeToList $ getLiteralString token
            [(t, t, name, DataArray SourceInteger)]

        --Points to 'for' rather than variable
        T_ForIn id str [] _ -> [(t, t, str, DataString SourceExternal)]
        T_ForIn id str words _ -> [(t, t, str, DataString $ SourceFrom words)]
        T_SelectIn id str words _ -> [(t, t, str, DataString $ SourceFrom words)]
        _ -> []
  where
    markAsChecked place token = mapMaybe (f place) $ getWordParts token
    f place t = case t of
            T_DollarBraced _ _ l ->
                let str = getBracedReference $ concat $ oversimplify l in do
                    guard $ isVariableName str
                    return (place, t, str, DataString SourceChecked)
            _ -> Nothing


-- Consider 'export/declare -x' a reference, since it makes the var available
getReferencedVariableCommand base@(T_SimpleCommand _ _ (T_NormalWord _ (T_Literal _ x:_):rest)) =
    case x of
        "declare" -> forDeclare
        "typeset" -> forDeclare

        "export" -> if "f" `elem` flags
            then []
            else concatMap getReference rest
        "local" -> if "x" `elem` flags
            then concatMap getReference rest
            else []
        "trap" ->
            case rest of
                head:_ -> map (\x -> (base, head, x)) $ getVariablesFromLiteralToken head
                _ -> []
        "alias" -> [(base, token, name) | token <- rest, name <- getVariablesFromLiteralToken token]
        _ -> []
  where
    forDeclare =
            if
                any (`elem` flags) ["x", "p"] &&
                    (not $ any (`elem` flags) ["f", "F"])
            then concatMap getReference rest
            else []

    getReference t@(T_Assignment _ _ name _ value) = [(t, t, name)]
    getReference t@(T_NormalWord _ [T_Literal _ name]) | not ("-" `isPrefixOf` name) = [(t, t, name)]
    getReference _ = []
    flags = map snd $ getAllFlags base

getReferencedVariableCommand _ = []

-- The function returns a tuple consisting of four items describing an assignment.
-- Given e.g. declare foo=bar
-- (
--   BaseCommand :: Token,     -- The command/structure assigning the variable, i.e. declare foo=bar
--   AssignmentToken :: Token, -- The specific part that assigns this variable, i.e. foo=bar
--   VariableName :: String,   -- The variable name, i.e. foo
--   VariableValue :: DataType -- A description of the value being assigned, i.e. "Literal string with value foo"
-- )
getModifiedVariableCommand base@(T_SimpleCommand id cmdPrefix (T_NormalWord _ (T_Literal _ x:_):rest)) =
   filter (\(_,_,s,_) -> not ("-" `isPrefixOf` s)) $
    case x of
        "builtin" ->
            getModifiedVariableCommand $ T_SimpleCommand id cmdPrefix rest
        "read" ->
            let fallback = catMaybes $ takeWhile isJust (reverse $ map getLiteral rest)
            in fromMaybe fallback $ do
                parsed <- getGnuOpts flagsForRead rest
                case lookup "a" parsed of
                    Just (_, var) -> (:[]) <$> getLiteralArray var
                    Nothing -> return $ catMaybes $
                        map (getLiteral . snd . snd) $ filter (null . fst) parsed

        "getopts" ->
            case rest of
                opts:var:_ -> maybeToList $ getLiteral var
                _          -> []

        "let" -> concatMap letParamToLiteral rest

        "export" ->
            if "f" `elem` flags then [] else concatMap getModifierParamString rest

        "declare" -> forDeclare
        "typeset" -> forDeclare

        "local" -> concatMap getModifierParamString rest
        "readonly" ->
            if any (`elem` flags) ["f", "p"]
            then []
            else concatMap getModifierParamString rest
        "set" -> maybeToList $ do
            params <- getSetParams rest
            return (base, base, "@", DataString $ SourceFrom params)

        "printf" -> maybeToList $ getPrintfVariable rest
        "wait" ->   maybeToList $ getWaitVariable rest

        "mapfile" -> maybeToList $ getMapfileArray base rest
        "readarray" -> maybeToList $ getMapfileArray base rest

        "DEFINE_boolean" -> maybeToList $ getFlagVariable rest
        "DEFINE_float" -> maybeToList $ getFlagVariable rest
        "DEFINE_integer" -> maybeToList $ getFlagVariable rest
        "DEFINE_string" -> maybeToList $ getFlagVariable rest

        _ -> []
  where
    flags = map snd $ getAllFlags base
    stripEquals s = drop 1 $ dropWhile (/= '=') s
    stripEqualsFrom (T_NormalWord id1 (T_Literal id2 s:rs)) =
        T_NormalWord id1 (T_Literal id2 (stripEquals s):rs)
    stripEqualsFrom (T_NormalWord id1 [T_DoubleQuoted id2 [T_Literal id3 s]]) =
        T_NormalWord id1 [T_DoubleQuoted id2 [T_Literal id3 (stripEquals s)]]
    stripEqualsFrom t = t

    forDeclare = if any (`elem` flags) ["F", "f", "p"] then [] else declaredVars

    declaredVars = concatMap (getModifierParam defaultType) rest
      where
        defaultType = if any (`elem` flags) ["a", "A"] then DataArray else DataString

    getLiteralOfDataType t d = do
        s <- getLiteralString t
        when ("-" `isPrefixOf` s) $ fail "argument"
        return (base, t, s, d)

    getLiteral t = getLiteralOfDataType t (DataString SourceExternal)

    getLiteralArray t = getLiteralOfDataType t (DataArray SourceExternal)

    getModifierParamString = getModifierParam DataString

    getModifierParam def t@(T_Assignment _ _ name _ value) =
        [(base, t, name, dataTypeFrom def value)]
    getModifierParam def t@T_NormalWord {} = maybeToList $ do
        name <- getLiteralString t
        guard $ isVariableName name
        return (base, t, name, def SourceDeclaration)
    getModifierParam _ _ = []

    letParamToLiteral token =
          if null var
            then []
            else [(base, token, var, DataString $ SourceFrom [stripEqualsFrom token])]
        where var = takeWhile isVariableChar $ dropWhile (`elem` "+-") $ concat $ oversimplify token

    getSetParams (t:_:rest) | getLiteralString t == Just "-o" = getSetParams rest
    getSetParams (t:rest) =
        let s = getLiteralString t in
            case s of
                Just "--"    -> return rest
                Just ('-':_) -> getSetParams rest
                _            -> return (t:fromMaybe [] (getSetParams rest))
    getSetParams [] = Nothing

    getPrintfVariable list = getFlagAssignedVariable "v" (SourceFrom list) $ getBsdOpts "v:" list
    getWaitVariable   list = getFlagAssignedVariable "p" SourceInteger     $ return $ getGenericOpts list

    getFlagAssignedVariable str dataSource maybeFlags = do
        flags <- maybeFlags
        (_, (flag, value)) <- find ((== str) . fst) flags
        variableName <- getLiteralStringExt (const $ return "!") value
        let (baseName, index) = span (/= '[') variableName
        return (base, value, baseName, (if null index then DataString else DataArray) dataSource)

    -- mapfile has some curious syntax allowing flags plus 0..n variable names
    -- where only the first non-option one is used if any.
    getMapfileArray base rest = parseArgs `mplus` fallback
      where
        parseArgs :: Maybe (Token, Token, String, DataType)
        parseArgs = do
            args <- getGnuOpts "d:n:O:s:u:C:c:t" rest
            case [y | ("",(_,y)) <- args] of
                [] ->
                    return (base, base, "MAPFILE", DataArray SourceExternal)
                first:_ -> do
                    name <- getLiteralString first
                    guard $ isVariableName name
                    return (base, first, name, DataArray SourceExternal)
        -- If arg parsing fails (due to bad or new flags), get the last variable name
        fallback :: Maybe (Token, Token, String, DataType)
        fallback = do
            (name, token) <- listToMaybe . mapMaybe f $ reverse rest
            return (base, token, name, DataArray SourceExternal)
        f arg = do
            name <- getLiteralString arg
            guard $ isVariableName name
            return (name, arg)

    -- get the FLAGS_ variable created by a shflags DEFINE_ call
    getFlagVariable (n:v:_) = do
        name <- getLiteralString n
        return (base, n, "FLAGS_" ++ name, DataString $ SourceExternal)
    getFlagVariable _ = Nothing

getModifiedVariableCommand _ = []

-- Given a NormalWord like foo or foo[$bar], get foo.
-- Primarily used to get references for [[ -v foo[bar] ]]
getVariableForTestDashV :: Token -> Maybe String
getVariableForTestDashV t = do
    str <- takeWhile ('[' /=) <$> getLiteralStringExt toStr t
    guard $ isVariableName str
    return str
  where
    -- foo[bar] gets parsed with [bar] as a glob, so undo that
    toStr (T_Glob _ s) = return s
    -- Turn foo[$x] into foo[\0] so that we can get the constant array name
    -- in a non-constant expression (while filtering out foo$x[$y])
    toStr _ = return "\0"

getReferencedVariables parents t =
    case t of
        T_DollarBraced id _ l -> let str = concat $ oversimplify l in
            (t, t, getBracedReference str) :
                map (\x -> (l, l, x)) (
                    getIndexReferences str
                    ++ getOffsetReferences (getBracedModifier str))
        TA_Variable id name _ ->
            if isArithmeticAssignment t
            then []
            else [(t, t, name)]
        T_Assignment id mode str _ word ->
            [(t, t, str) | mode == Append] ++ specialReferences str t word

        TC_Unary id _ "-v" token -> getIfReference t token
        TC_Unary id _ "-R" token -> getIfReference t token
        TC_Binary id DoubleBracket op lhs rhs ->
            if isDereferencingBinaryOp op
            then concatMap (getIfReference t) [lhs, rhs]
            else []

        T_BatsTest {} -> [ -- pretend @test references vars to avoid warnings
            (t, t, "lines"),
            (t, t, "status"),
            (t, t, "output")
            ]

        T_FdRedirect _ ('{':var) op -> -- {foo}>&- references and closes foo
            [(t, t, takeWhile (/= '}') var) | isClosingFileOp op]
        x -> getReferencedVariableCommand x
  where
    -- Try to reduce false positives for unused vars only referenced from evaluated vars
    specialReferences name base word =
        if name `elem` [
            "PS1", "PS2", "PS3", "PS4",
            "PROMPT_COMMAND"
          ]
        then
            map (\x -> (base, base, x)) $
                getVariablesFromLiteralToken word
        else []

    literalizer t = case t of
        T_Glob _ s -> return s    -- Also when parsed as globs
        _          -> []

    getIfReference context token = maybeToList $ do
            str <- getVariableForTestDashV token
            return (context, token, getBracedReference str)

    isArithmeticAssignment t = case getPath parents t of
        this NE.:| TA_Assignment _ "=" lhs _ :_ -> lhs == t
        _                                  -> False

isDereferencingBinaryOp = (`elem` ["-eq", "-ne", "-lt", "-le", "-gt", "-ge"])

dataTypeFrom defaultType v = (case v of T_Array {} -> DataArray; _ -> defaultType) $ SourceFrom [v]


--- Command specific checks

-- Compare a command to a string: t `isCommand` "sed" (also matches /usr/bin/sed)
isCommand token str = isCommandMatch token (\cmd -> cmd  == str || ('/' : str) `isSuffixOf` cmd)

-- Compare a command to a literal. Like above, but checks full path.
isUnqualifiedCommand token str = isCommandMatch token (== str)

isCommandMatch token matcher = maybe False
    matcher (getCommandName token)

-- Does this regex look like it was intended as a glob?
-- True:  *foo*
-- False: .*foo.*
isConfusedGlobRegex :: String -> Bool
isConfusedGlobRegex ('*':_) = True
isConfusedGlobRegex [x,'*'] | x `notElem` "\\." = True
isConfusedGlobRegex _       = False

getVariablesFromLiteralToken token =
    getVariablesFromLiteral (getLiteralStringDef " " token)

-- Try to get referenced variables from a literal string like "$foo"
-- Ignores tons of cases like arithmetic evaluation and array indices.
prop_getVariablesFromLiteral1 =
    getVariablesFromLiteral "$foo${bar//a/b}$BAZ" == ["foo", "bar", "BAZ"]
getVariablesFromLiteral string =
    map head $ matchAllSubgroups variableRegex string
  where
    variableRegex = mkRegex "\\$\\{?([A-Za-z0-9_]+)"


-- Run a command if the shell is in the given list
whenShell l c = do
    params <- ask
    when (shellType params `elem` l ) c


filterByAnnotation asSpec params =
    filter (not . shouldIgnore)
  where
    token = asScript asSpec
    shouldIgnore note =
        any (shouldIgnoreFor (getCode note)) $
            getPath parents (T_Bang $ tcId note)
    shouldIgnoreFor _ T_Include {} = not $ asCheckSourced asSpec
    shouldIgnoreFor code t = isAnnotationIgnoringCode code t
    parents = parentMap params
    getCode = cCode . tcComment

shouldIgnoreCode params code t =
    any (isAnnotationIgnoringCode code) $
        getPath (parentMap params) t

-- Is this a ${#anything}, to get string length or array count?
isCountingReference (T_DollarBraced id _ token) =
    case concat $ oversimplify token of
        '#':_ -> True
        _     -> False
isCountingReference _ = False

-- FIXME: doesn't handle ${a:+$var} vs ${a:+"$var"}
isQuotedAlternativeReference t =
    case t of
        T_DollarBraced _ _ l ->
            getBracedModifier (concat $ oversimplify l) `matches` re
        _ -> False
  where
    re = mkRegex "(^|\\]):?\\+"

supportsArrays Bash = True
supportsArrays Ksh = True
supportsArrays _ = False

isTrueAssignmentSource c =
    case c of
        DataString SourceChecked -> False
        DataString SourceDeclaration -> False
        DataArray SourceChecked -> False
        DataArray SourceDeclaration -> False
        _ -> True

modifiesVariable params token name =
    or $ map check flow
  where
    flow = getVariableFlow params token
    check t =
        case t of
            Assignment (_, _, n, source) -> isTrueAssignmentSource source && n == name
            _ -> False

isTestCommand t =
    case t of
        T_Condition {} -> True
        T_SimpleCommand {} -> t `isCommand` "test"
        T_Redirecting _ _ t -> isTestCommand t
        T_Annotation _ _ t -> isTestCommand t
        T_Pipeline _ _ [t] -> isTestCommand t
        _ -> False

return []
runTests =  $( [| $(forAllProperties) (quickCheckWithResult (stdArgs { maxSuccess = 1 }) ) |])
