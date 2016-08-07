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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module ShellCheck.AnalyzerLib where
import ShellCheck.AST
import ShellCheck.ASTLib
import ShellCheck.Data
import ShellCheck.Interface
import ShellCheck.Parser
import ShellCheck.Regex

import Control.Arrow (first)
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as Map

import Test.QuickCheck.All (forAllProperties)
import Test.QuickCheck.Test (quickCheckWithResult, stdArgs, maxSuccess)

type Analysis = ReaderT Parameters (Writer [TokenComment]) ()


data Parameters = Parameters {
    variableFlow :: [StackData],
    parentMap :: Map.Map Id Token,
    shellType :: Shell,
    shellTypeSpecified :: Bool
    }

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

data DataSource = SourceFrom [Token] | SourceExternal | SourceDeclaration | SourceInteger
  deriving (Show)

data VariableState = Dead Token String | Alive deriving (Show)

defaultSpec root = AnalysisSpec {
    asScript = root,
    asShellType = Nothing,
    asExecutionMode = Executed
}

pScript s =
  let
    pSpec = ParseSpec {
        psFilename = "script",
        psScript = s
    }
  in prRoot . runIdentity $ parseScript (mockedSystemInterface []) pSpec

makeComment :: Severity -> Id -> Code -> String -> TokenComment
makeComment severity id code note =
    TokenComment id $ Comment severity code note

addComment note = tell [note]

warn :: MonadWriter [TokenComment] m => Id -> Code -> String -> m ()
warn  id code str = addComment $ makeComment WarningC id code str
err   id code str = addComment $ makeComment ErrorC id code str
info  id code str = addComment $ makeComment InfoC id code str
style id code str = addComment $ makeComment StyleC id code str

makeParameters spec =
    let params = Parameters {
        shellType = fromMaybe (determineShell root) $ asShellType spec,
        shellTypeSpecified = isJust $ asShellType spec,
        parentMap = getParentTree root,
        variableFlow =
            getVariableFlow (shellType params) (parentMap params) root
    } in params
  where root = asScript spec

prop_determineShell0 = determineShell (fromJust $ pScript "#!/bin/sh") == Sh
prop_determineShell1 = determineShell (fromJust $ pScript "#!/usr/bin/env ksh") == Ksh
prop_determineShell2 = determineShell (fromJust $ pScript "") == Bash
prop_determineShell3 = determineShell (fromJust $ pScript "#!/bin/sh -e") == Sh
prop_determineShell4 = determineShell (fromJust $ pScript
    "#!/bin/ksh\n#shellcheck shell=sh\nfoo") == Sh
prop_determineShell5 = determineShell (fromJust $ pScript
    "#shellcheck shell=sh\nfoo") == Sh
prop_determineShell6 = determineShell (fromJust $ pScript "#! /bin/sh") == Sh
determineShell t = fromMaybe Bash $ do
    shellString <- foldl mplus Nothing $ getCandidates t
    shellForExecutable shellString
  where
    forAnnotation t =
        case t of
            (ShellOverride s) -> return s
            _ -> fail ""
    getCandidates :: Token -> [Maybe String]
    getCandidates t@(T_Script {}) = [Just $ fromShebang t]
    getCandidates (T_Annotation _ annotations s) =
        map forAnnotation annotations ++
           [Just $ fromShebang s]
    fromShebang (T_Script _ s t) = shellFor s

    shellFor s | "/env " `isInfixOf` s = head (drop 1 (words s)++[""])
    shellFor s | ' ' `elem` s = shellFor $ takeWhile (/= ' ') s
    shellFor s = reverse . takeWhile (/= '/') . reverse $ s


--- Context seeking

getParentTree t =
    snd . snd $ runState (doStackAnalysis pre post t) ([], Map.empty)
  where
    pre t = modify (first ((:) t))
    post t = do
        (_:rest, map) <- get
        case rest of [] -> put (rest, map)
                     (x:_) -> put (rest, Map.insert (getId t) x map)

getTokenMap t =
    execState (doAnalysis f t) Map.empty
  where
    f t = modify (Map.insert (getId t) t)


-- Is this node self quoting for a regular element?
isQuoteFree = isQuoteFreeNode False

-- Is this node striclty self quoting, for array expansions
isStrictlyQuoteFree = isQuoteFreeNode True


isQuoteFreeNode strict tree t =
    (isQuoteFreeElement t == Just True) ||
        head (mapMaybe isQuoteFreeContext (drop 1 $ getPath tree t) ++ [False])
  where
    -- Is this node self-quoting in itself?
    isQuoteFreeElement t =
        case t of
            T_Assignment {} -> return True
            T_FdRedirect {} -> return True
            _ -> Nothing

    -- Are any subnodes inherently self-quoting?
    isQuoteFreeContext t =
        case t of
            TC_Noary _ DoubleBracket _ -> return True
            TC_Unary _ DoubleBracket _ _ -> return True
            TC_Binary _ DoubleBracket _ _ _ -> return True
            TA_Sequence {} -> return True
            T_Arithmetic {} -> return True
            T_Assignment {} -> return True
            T_Redirecting {} -> return $
                if strict then False else
                    -- Not true, just a hack to prevent warning about non-expansion refs
                    any (isCommand t) ["local", "declare", "typeset", "export", "trap", "readonly"]
            T_DoubleQuoted _ _ -> return True
            T_DollarDoubleQuoted _ _ -> return True
            T_CaseExpression {} -> return True
            T_HereDoc {} -> return True
            T_DollarBraced {} -> return True
            -- When non-strict, pragmatically assume it's desirable to split here
            T_ForIn {} -> return (not strict)
            T_SelectIn {} -> return (not strict)
            _ -> Nothing

isParamTo tree cmd =
    go
  where
    go x = case Map.lookup (getId x) tree of
                Nothing -> False
                Just parent -> check parent
    check t =
        case t of
            T_SingleQuoted _ _ -> go t
            T_DoubleQuoted _ _ -> go t
            T_NormalWord _ _ -> go t
            T_SimpleCommand {} -> isCommand t cmd
            T_Redirecting {} -> isCommand t cmd
            _ -> False

getClosestCommand tree t =
    msum . map getCommand $ getPath tree t
  where
    getCommand t@(T_Redirecting {}) = return t
    getCommand _ = Nothing

usedAsCommandName tree token = go (getId token) (tail $ getPath tree token)
  where
    go currentId (T_NormalWord id [word]:rest)
        | currentId == getId word = go id rest
    go currentId (T_DoubleQuoted id [word]:rest)
        | currentId == getId word = go id rest
    go currentId (T_SimpleCommand _ _ (word:_):_)
        | currentId == getId word = True
    go _ _ = False

-- A list of the element and all its parents
getPath tree t = t :
    case Map.lookup (getId t) tree of
        Nothing -> []
        Just parent -> getPath tree parent

isParentOf tree parent child =
    elem (getId parent) . map getId $ getPath tree child

parents params = getPath (parentMap params)

pathTo t = do
    parents <- reader parentMap
    return $ getPath parents t

-- Check whether a word is entirely output from a single command
tokenIsJustCommandOutput t = case t of
    T_NormalWord id [T_DollarExpansion _ cmds] -> check cmds
    T_NormalWord id [T_DoubleQuoted _ [T_DollarExpansion _ cmds]] -> check cmds
    T_NormalWord id [T_Backticked _ cmds] -> check cmds
    T_NormalWord id [T_DoubleQuoted _ [T_Backticked _ cmds]] -> check cmds
    _ -> False
  where
    check [x] = not $ isOnlyRedirection x
    check _ = False

-- TODO: Replace this with a proper Control Flow Graph
getVariableFlow shell parents t =
    let (_, stack) = runState (doStackAnalysis startScope endScope t) []
    in reverse stack
  where
    startScope t =
        let scopeType = leadType shell parents t
        in do
            when (scopeType /= NoneScope) $ modify (StackScope scopeType:)
            when (assignFirst t) $ setWritten t

    endScope t =
        let scopeType = leadType shell parents t
        in do
            setRead t
            unless (assignFirst t) $ setWritten t
            when (scopeType /= NoneScope) $ modify (StackScopeEnd:)

    assignFirst (T_ForIn {}) = True
    assignFirst (T_SelectIn {}) = True
    assignFirst _ = False

    setRead t =
        let read    = getReferencedVariables parents t
        in mapM_ (\v -> modify (Reference v:)) read

    setWritten t =
        let written = getModifiedVariables t
        in mapM_ (\v -> modify (Assignment v:)) written


leadType shell parents t =
    case t of
        T_DollarExpansion _ _  -> SubshellScope "$(..) expansion"
        T_Backticked _ _  -> SubshellScope "`..` expansion"
        T_Backgrounded _ _  -> SubshellScope "backgrounding &"
        T_Subshell _ _  -> SubshellScope "(..) group"
        T_CoProcBody _ _  -> SubshellScope "coproc"
        T_Redirecting {}  ->
            if fromMaybe False causesSubshell
            then SubshellScope "pipeline"
            else NoneScope
        _ -> NoneScope
  where
    parentPipeline = do
        parent <- Map.lookup (getId t) parents
        case parent of
            T_Pipeline {} -> return parent
            _ -> Nothing

    causesSubshell = do
        (T_Pipeline _ _ list) <- parentPipeline
        if length list <= 1
            then return False
            else if lastCreatesSubshell
                then return True
                else return . not $ (getId . head $ reverse list) == getId t

    lastCreatesSubshell =
        case shell of
            Bash -> True
            Dash -> True
            Sh -> True
            Ksh -> False

getModifiedVariables t =
    case t of
        T_SimpleCommand _ vars [] ->
            concatMap (\x -> case x of
                                T_Assignment id _ name _ w  ->
                                    [(x, x, name, dataTypeFrom DataString w)]
                                _ -> []
                      ) vars
        c@(T_SimpleCommand {}) ->
            getModifiedVariableCommand c

        TA_Unary _ "++|" var -> maybeToList $ do
            name <- getLiteralString var
            return (t, t, name, DataString $ SourceFrom [t])
        TA_Unary _ "|++" var -> maybeToList $ do
            name <- getLiteralString var
            return (t, t, name, DataString $ SourceFrom [t])
        TA_Assignment _ op lhs rhs -> maybeToList $ do
            guard $ op `elem` ["=", "*=", "/=", "%=", "+=", "-=", "<<=", ">>=", "&=", "^=", "|="]
            name <- getLiteralString lhs
            return (t, t, name, DataString $ SourceFrom [rhs])

        t@(T_FdRedirect _ ('{':var) op) -> -- {foo}>&2 modifies foo
            [(t, t, takeWhile (/= '}') var, DataString SourceInteger) | not $ isClosingFileOp op]

        t@(T_CoProc _ name _) ->
            [(t, t, fromMaybe "COPROC" name, DataArray SourceInteger)]

        --Points to 'for' rather than variable
        T_ForIn id str words _ -> [(t, t, str, DataString $ SourceFrom words)]
        T_SelectIn id str words _ -> [(t, t, str, DataString $ SourceFrom words)]
        _ -> []

isClosingFileOp op =
    case op of
        T_IoFile _ (T_GREATAND _) (T_NormalWord _ [T_Literal _ "-"]) -> True
        T_IoFile _ (T_LESSAND  _) (T_NormalWord _ [T_Literal _ "-"]) -> True
        _ -> False


-- Consider 'export/declare -x' a reference, since it makes the var available
getReferencedVariableCommand base@(T_SimpleCommand _ _ (T_NormalWord _ (T_Literal _ x:_):rest)) =
    case x of
        "export" -> if "f" `elem` flags
            then []
            else concatMap getReference rest
        "declare" -> if any (`elem` flags) ["x", "p"]
            then concatMap getReference rest
            else []
        "readonly" -> concatMap getReference rest
        "trap" ->
            case rest of
                head:_ -> map (\x -> (head, head, x)) $ getVariablesFromLiteralToken head
                _ -> []
        _ -> []
  where
    getReference t@(T_Assignment _ _ name _ value) = [(t, t, name)]
    getReference t@(T_NormalWord _ [T_Literal _ name]) | not ("-" `isPrefixOf` name) = [(t, t, name)]
    getReference _ = []
    flags = map snd $ getAllFlags base

getReferencedVariableCommand _ = []

getModifiedVariableCommand base@(T_SimpleCommand _ _ (T_NormalWord _ (T_Literal _ x:_):rest)) =
   filter (\(_,_,s,_) -> not ("-" `isPrefixOf` s)) $
    case x of
        "read" ->
            let params = map getLiteral rest in
                catMaybes . takeWhile isJust . reverse $ params
        "getopts" ->
            case rest of
                opts:var:_ -> maybeToList $ getLiteral var
                _ -> []

        "let" -> concatMap letParamToLiteral rest

        "export" ->
            if "f" `elem` flags then [] else concatMap getModifierParamString rest

        "declare" -> if any (`elem` flags) ["F", "f", "p"] then [] else declaredVars
        "typeset" -> declaredVars

        "local" -> concatMap getModifierParamString rest
        "readonly" -> concatMap getModifierParamString rest
        "set" -> maybeToList $ do
            params <- getSetParams rest
            return (base, base, "@", DataString $ SourceFrom params)

        "printf" -> maybeToList $ getPrintfVariable rest

        "mapfile" -> maybeToList $ getMapfileArray base rest
        "readarray" -> maybeToList $ getMapfileArray base rest

        _ -> []
  where
    flags = map snd $ getAllFlags base
    stripEquals s = let rest = dropWhile (/= '=') s in
        if rest == "" then "" else tail rest
    stripEqualsFrom (T_NormalWord id1 (T_Literal id2 s:rs)) =
        T_NormalWord id1 (T_Literal id2 (stripEquals s):rs)
    stripEqualsFrom (T_NormalWord id1 [T_DoubleQuoted id2 [T_Literal id3 s]]) =
        T_NormalWord id1 [T_DoubleQuoted id2 [T_Literal id3 (stripEquals s)]]
    stripEqualsFrom t = t

    declaredVars = concatMap (getModifierParam defaultType) rest
      where
        defaultType = if any (`elem` flags) ["a", "A"] then DataArray else DataString

    getLiteral t = do
        s <- getLiteralString t
        when ("-" `isPrefixOf` s) $ fail "argument"
        return (base, t, s, DataString SourceExternal)

    getModifierParamString = getModifierParam DataString

    getModifierParam def t@(T_Assignment _ _ name _ value) =
        [(base, t, name, dataTypeFrom def value)]
    getModifierParam def t@(T_NormalWord {}) = maybeToList $ do
        name <- getLiteralString t
        guard $ isVariableName name
        return (base, t, name, def SourceDeclaration)
    getModifierParam _ _ = []

    letParamToLiteral token =
          if var == ""
            then []
            else [(base, token, var, DataString $ SourceFrom [stripEqualsFrom token])]
        where var = takeWhile isVariableChar $ dropWhile (`elem` "+-") $ concat $ oversimplify token

    getSetParams (t:_:rest) | getLiteralString t == Just "-o" = getSetParams rest
    getSetParams (t:rest) =
        let s = getLiteralString t in
            case s of
                Just "--" -> return rest
                Just ('-':_) -> getSetParams rest
                _ -> return (t:fromMaybe [] (getSetParams rest))
    getSetParams [] = Nothing

    getPrintfVariable list = f $ map (\x -> (x, getLiteralString x)) list
      where
        f ((_, Just "-v") : (t, Just var) : _) = return (base, t, var, DataString $ SourceFrom list)
        f (_:rest) = f rest
        f [] = fail "not found"

    -- mapfile has some curious syntax allowing flags plus 0..n variable names
    -- where only the first non-option one is used if any. Here we cheat and
    -- just get the last one, if it's a variable name.
    getMapfileArray base arguments = do
        lastArg <- listToMaybe (reverse arguments)
        name <- getLiteralString lastArg
        guard $ isVariableName name
        return (base, lastArg, name, DataArray SourceExternal)

getModifiedVariableCommand _ = []

getIndexReferences s = fromMaybe [] $ do
    match <- matchRegex re s
    index <- match !!! 0
    return $ matchAllStrings variableNameRegex index
  where
    re = mkRegex "(\\[.*\\])"

getReferencedVariables parents t =
    case t of
        T_DollarBraced id l -> let str = bracedString t in
            (t, t, getBracedReference str) :
                map (\x -> (l, l, x)) (getIndexReferences str)
        TA_Expansion id _ ->
            if isArithmeticAssignment t
            then []
            else getIfReference t t
        T_Assignment id mode str _ word ->
            [(t, t, str) | mode == Append] ++ specialReferences str t word

        TC_Unary id _ "-v" token -> getIfReference t token
        TC_Unary id _ "-R" token -> getIfReference t token
        TC_Binary id DoubleBracket op lhs rhs ->
            if isDereferencing op
            then concatMap (getIfReference t) [lhs, rhs]
            else []

        t@(T_FdRedirect _ ('{':var) op) -> -- {foo}>&- references and closes foo
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

    literalizer (TA_Index {}) = return ""  -- x[0] becomes a reference of x
    literalizer _ = Nothing

    getIfReference context token = maybeToList $ do
            str <- getLiteralStringExt literalizer token
            guard . not $ null str
            when (isDigit $ head str) $ fail "is a number"
            return (context, token, getBracedReference str)

    isDereferencing = (`elem` ["-eq", "-ne", "-lt", "-le", "-gt", "-ge"])

    isArithmeticAssignment t = case getPath parents t of
        this: TA_Assignment _ "=" lhs _ :_ -> lhs == t
        _ -> False

dataTypeFrom defaultType v = (case v of T_Array {} -> DataArray; _ -> defaultType) $ SourceFrom [v]


--- Command specific checks

isCommand token str = isCommandMatch token (\cmd -> cmd  == str || ('/' : str) `isSuffixOf` cmd)
isUnqualifiedCommand token str = isCommandMatch token (== str)

isCommandMatch token matcher = fromMaybe False $ do
    cmd <- getCommandName token
    return $ matcher cmd

isConfusedGlobRegex ('*':_) = True
isConfusedGlobRegex [x,'*'] | x /= '\\' = True
isConfusedGlobRegex _ = False

isVariableStartChar x = x == '_' || isAsciiLower x || isAsciiUpper x
isVariableChar x = isVariableStartChar x || isDigit x
variableNameRegex = mkRegex "[_a-zA-Z][_a-zA-Z0-9]*"

prop_isVariableName1 = isVariableName "_fo123"
prop_isVariableName2 = not $ isVariableName "4"
prop_isVariableName3 = not $ isVariableName "test: "
isVariableName (x:r) = isVariableStartChar x && all isVariableChar r
isVariableName _ = False

getVariablesFromLiteralToken token =
    getVariablesFromLiteral (fromJust $ getLiteralStringExt (const $ return " ") token)

-- Try to get referenced variables from a literal string like "$foo"
-- Ignores tons of cases like arithmetic evaluation and array indices.
prop_getVariablesFromLiteral1 =
    getVariablesFromLiteral "$foo${bar//a/b}$BAZ" == ["foo", "bar", "BAZ"]
getVariablesFromLiteral string =
    map (!! 0) $ matchAllSubgroups variableRegex string
  where
    variableRegex = mkRegex "\\$\\{?([A-Za-z0-9_]+)"

prop_getBracedReference1 = getBracedReference "foo" == "foo"
prop_getBracedReference2 = getBracedReference "#foo" == "foo"
prop_getBracedReference3 = getBracedReference "#" == "#"
prop_getBracedReference4 = getBracedReference "##" == "#"
prop_getBracedReference5 = getBracedReference "#!" == "!"
prop_getBracedReference6 = getBracedReference "!#" == "#"
prop_getBracedReference7 = getBracedReference "!foo#?" == "foo"
prop_getBracedReference8 = getBracedReference "foo-bar" == "foo"
prop_getBracedReference9 = getBracedReference "foo:-bar" == "foo"
prop_getBracedReference10= getBracedReference "foo: -1" == "foo"
prop_getBracedReference11= getBracedReference "!os*" == ""
prop_getBracedReference12= getBracedReference "!os?bar**" == ""
prop_getBracedReference13= getBracedReference "foo[bar]" == "foo"
getBracedReference s = fromMaybe s $
    nameExpansion s `mplus` takeName noPrefix `mplus` getSpecial noPrefix `mplus` getSpecial s
  where
    noPrefix = dropPrefix s
    dropPrefix (c:rest) = if c `elem` "!#" then rest else c:rest
    dropPrefix "" = ""
    takeName s = do
        let name = takeWhile isVariableChar s
        guard . not $ null name
        return name
    getSpecial (c:_) =
        if c `elem` "*@#?-$!" then return [c] else fail "not special"
    getSpecial _ = fail "empty"

    nameExpansion ('!':rest) = do -- e.g. ${!foo*bar*}
        let suffix = dropWhile isVariableChar rest
        guard $ suffix /= rest -- e.g. ${!@}
        first <- suffix !!! 0
        guard $ first `elem` "*?"
        return ""
    nameExpansion _ = Nothing

prop_getBracedModifier1 = getBracedModifier "foo:bar:baz" == ":bar:baz"
prop_getBracedModifier2 = getBracedModifier "!var:-foo" == ":-foo"
prop_getBracedModifier3 = getBracedModifier "foo[bar]" == "[bar]"
getBracedModifier s = fromMaybe "" . listToMaybe $ do
    let var = getBracedReference s
    a <- dropModifier s
    dropPrefix var a
  where
    dropPrefix [] t = return t
    dropPrefix (a:b) (c:d) | a == c = dropPrefix b d
    dropPrefix _ _ = []

    dropModifier (c:rest) | c `elem` "#!" = [rest, c:rest]
    dropModifier x = [x]

-- Useful generic functions
potentially :: Monad m => Maybe (m ()) -> m ()
potentially = fromMaybe (return ())

headOrDefault _ (a:_) = a
headOrDefault def _ = def

(!!!) list i =
    case drop i list of
        [] -> Nothing
        (r:_) -> Just r



filterByAnnotation token =
    filter (not . shouldIgnore)
  where
    idFor (TokenComment id _) = id
    shouldIgnore note =
        any (shouldIgnoreFor (getCode note)) $
            getPath parents (T_Bang $ idFor note)
    shouldIgnoreFor num (T_Annotation _ anns _) =
        any hasNum anns
      where
        hasNum (DisableComment ts) = num == ts
        hasNum _ = False
    shouldIgnoreFor _ (T_Include {}) = True -- Ignore included files
    shouldIgnoreFor _ _ = False
    parents = getParentTree token
    getCode (TokenComment _ (Comment _ c _)) = c


return []
runTests =  $( [| $(forAllProperties) (quickCheckWithResult (stdArgs { maxSuccess = 1 }) ) |])
