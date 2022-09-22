{-
    Copyright 2022 Vidar Holen

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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

{-
    Data Flow Analysis on a Control Flow Graph.

    This module implements a pretty standard iterative Data Flow Analysis.
    For an overview of the process, see Wikipedia.

    Since shell scripts rely heavily on global variables, this DFA includes
    tracking the value of globals across calls. Each function invocation is
    treated as a separate DFA problem, and a caching mechanism (hopefully)
    avoids any exponential explosions.

    To do efficient DFA join operations (or merges, as the code calls them),
    some of the data structures have an integer version attached. On update,
    the version is changed. If two states have the same version number,
    a merge is skipped on the grounds that they are identical. It is easy
    to unintentionally forget to update/invalidate the version number,
    and bugs will ensure.

    For performance reasons, the entire code runs in plain ST, with a manual
    context object Ctx being passed around. It relies heavily on mutable
    STRefs. However, this turned out to be literally thousands of times faster
    than my several attempts using RWST, so it can't be helped.
-}

module ShellCheck.CFGAnalysis (
    analyzeControlFlow
    ,CFGParameters (..)
    ,CFGAnalysis (..)
    ,ProgramState (..)
    ,VariableState (..)
    ,VariableValue (..)
    ,VariableProperties
    ,SpaceStatus (..)
    ,NumericalStatus (..)
    ,getIncomingState
    ,getOutgoingState
    ,doesPostDominate
    ,ShellCheck.CFGAnalysis.runTests -- STRIP
    ) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
import Data.Array.Unboxed
import Data.Char
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.DFS
import Data.List hiding (map)
import Data.Maybe
import Data.STRef
import Debug.Trace -- STRIP
import GHC.Generics (Generic)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified ShellCheck.Data as Data
import ShellCheck.AST
import ShellCheck.CFG
import ShellCheck.Prelude

import Test.QuickCheck


-- The number of iterations for DFA to stabilize
iterationCount = 1000000
-- There have been multiple bugs where bad caching caused oscillations.
-- As a precaution, disable caching if there's this many iterations left.
fallbackThreshold = 10000
-- The number of cache entries to keep per node
cacheEntries = 10

logVerbose log = do
    -- traceShowM log
    return ()
logInfo log = do
    -- traceShowM log
    return ()

-- The result of the data flow analysis
data CFGAnalysis = CFGAnalysis {
    graph :: CFGraph,
    tokenToRange :: M.Map Id (Node, Node),
    tokenToNodes :: M.Map Id (S.Set Node),
    postDominators :: Array Node [Node],
    nodeToData :: M.Map Node (ProgramState, ProgramState)
} deriving (Show)

-- The program state we expose externally
data ProgramState = ProgramState {
    -- internalState :: InternalState, -- For debugging
    variablesInScope :: M.Map String VariableState,
    exitCodes :: S.Set Id,
    stateIsReachable :: Bool
} deriving (Show, Eq, Generic, NFData)

internalToExternal :: InternalState -> ProgramState
internalToExternal s =
    ProgramState {
        -- Censor the literal value to avoid introducing dependencies on it. It's just for debugging.
        variablesInScope = M.map censor flatVars,
        -- internalState = s, -- For debugging
        exitCodes = fromMaybe S.empty $ sExitCodes s,
        stateIsReachable = fromMaybe True $ sIsReachable s
    }
  where
    censor s = s {
        variableValue = (variableValue s) {
            literalValue = Nothing
        }
    }
    flatVars = M.unionsWith (\_ last -> last) $ map mapStorage [sGlobalValues s, sLocalValues s, sPrefixValues s]

-- Conveniently get the state before a token id
getIncomingState :: CFGAnalysis -> Id -> Maybe ProgramState
getIncomingState analysis id = do
    (start,end) <- M.lookup id $ tokenToRange analysis
    fst <$> M.lookup start (nodeToData analysis)

-- Conveniently get the state after a token id
getOutgoingState :: CFGAnalysis -> Id -> Maybe ProgramState
getOutgoingState analysis id = do
    (start,end) <- M.lookup id $ tokenToRange analysis
    snd <$> M.lookup end (nodeToData analysis)

-- Conveniently determine whether one node postdominates another,
-- i.e. whether 'target' always unconditionally runs after 'base'.
doesPostDominate :: CFGAnalysis -> Id -> Id -> Bool
doesPostDominate analysis target base = fromMaybe False $ do
    (_, baseEnd) <- M.lookup base $ tokenToRange analysis
    (targetStart, _) <- M.lookup target $ tokenToRange analysis
    return $ targetStart `elem` (postDominators analysis ! baseEnd)

getDataForNode analysis node = M.lookup node $ nodeToData analysis

-- The current state of data flow at a point in the program, potentially as a diff
data InternalState = InternalState {
    sVersion :: Integer,
    sGlobalValues :: VersionedMap String VariableState,
    sLocalValues :: VersionedMap String VariableState,
    sPrefixValues :: VersionedMap String VariableState,
    sFunctionTargets :: VersionedMap String FunctionValue,
    sExitCodes :: Maybe (S.Set Id),
    sIsReachable :: Maybe Bool
} deriving (Show, Generic, NFData)

newInternalState = InternalState {
    sVersion = 0,
    sGlobalValues = vmEmpty,
    sLocalValues = vmEmpty,
    sPrefixValues = vmEmpty,
    sFunctionTargets = vmEmpty,
    sExitCodes = Nothing,
    sIsReachable = Nothing
}

unreachableState = modified newInternalState {
    sIsReachable = Just False
}

-- The default state we assume we get from the environment
createEnvironmentState :: InternalState
createEnvironmentState = do
    foldl' (flip ($)) newInternalState $ concat [
        addVars Data.internalVariables unknownVariableState,
        addVars Data.variablesWithoutSpaces spacelessVariableState,
        addVars Data.specialIntegerVariables integerVariableState
        ]
  where
    addVars names val = map (\name -> insertGlobal name val) names
    spacelessVariableState = unknownVariableState {
        variableValue = VariableValue {
            literalValue = Nothing,
            spaceStatus = SpaceStatusClean,
            numericalStatus = NumericalStatusUnknown
        }
    }
    integerVariableState = unknownVariableState {
        variableValue = unknownIntegerValue
    }


modified s = s { sVersion = -1 }

insertGlobal :: String -> VariableState -> InternalState -> InternalState
insertGlobal name value state = modified state {
    sGlobalValues = vmInsert name value $ sGlobalValues state
}

insertLocal :: String -> VariableState -> InternalState -> InternalState
insertLocal name value state = modified state {
    sLocalValues = vmInsert name value $ sLocalValues state
}

insertPrefix :: String -> VariableState -> InternalState -> InternalState
insertPrefix name value state = modified state {
    sPrefixValues = vmInsert name value $ sPrefixValues state
}

insertFunction :: String -> FunctionValue -> InternalState -> InternalState
insertFunction name value state = modified state {
    sFunctionTargets = vmInsert name value $ sFunctionTargets state
}

addProperties :: S.Set CFVariableProp -> VariableState -> VariableState
addProperties props state = state {
    variableProperties = S.map (S.union props) $ variableProperties state
}

removeProperties :: S.Set CFVariableProp -> VariableState -> VariableState
removeProperties props state = state {
    variableProperties = S.map (\s -> S.difference s props) $ variableProperties state
}

setExitCode id = setExitCodes (S.singleton id)
setExitCodes set state = modified state {
    sExitCodes = Just $ set
}

-- Dependencies on values, e.g. "if there is a global variable named 'foo' without spaces"
-- This is used to see if the DFA of a function would result in the same state, so anything
-- that affects DFA must be tracked.
data StateDependency =
    -- Complete variable state
    DepState Scope String VariableState
    -- Only variable properties (we need properties but not values for x=1)
    | DepProperties Scope String VariableProperties
    -- Function definition
    | DepFunction String (S.Set FunctionDefinition)
    -- Whether invoking the node would result in recursion (i.e., is the function on the stack?)
    | DepIsRecursive Node Bool
    -- The set of commands that could have provided the exit code $?
    | DepExitCodes (S.Set Id)
    deriving (Show, Eq, Ord, Generic, NFData)

-- A function definition, or lack thereof
data FunctionDefinition = FunctionUnknown | FunctionDefinition String Node Node
    deriving (Show, Eq, Ord, Generic, NFData)

-- The Set of places a command name can point (it's a Set to handle conditionally defined functions)
type FunctionValue = S.Set FunctionDefinition

-- Create an InternalState that fulfills the given dependencies
depsToState :: S.Set StateDependency -> InternalState
depsToState set = foldl insert newInternalState $ S.toList set
  where
    insert :: InternalState -> StateDependency -> InternalState
    insert state dep =
        case dep of
            DepFunction name val -> insertFunction name val state
            DepState scope name val -> insertIn True scope name val state
            -- State includes properties and more, so don't overwrite a state with properties
            DepProperties scope name props -> insertIn False scope name unknownVariableState { variableProperties = props } state
            DepIsRecursive _ _ -> state
            DepExitCodes s -> setExitCodes s state

    insertIn overwrite scope name val state =
        let
            (mapToCheck, inserter) =
                case scope of
                    PrefixScope -> (sPrefixValues, insertPrefix)
                    LocalScope -> (sLocalValues, insertLocal)
                    GlobalScope -> (sGlobalValues, insertGlobal)
                    DefaultScope -> error $ pleaseReport "Unresolved scope in dependency"

            alreadyExists = isJust $ vmLookup name $ mapToCheck state
        in
            if overwrite || not alreadyExists
            then inserter name val state
            else state

unknownFunctionValue = S.singleton FunctionUnknown

-- The information about the value of a single variable
data VariableValue = VariableValue {
    literalValue :: Maybe String, -- TODO: For debugging. Remove me.
    spaceStatus :: SpaceStatus,
    numericalStatus :: NumericalStatus
}
    deriving (Show, Eq, Ord, Generic, NFData)

data VariableState = VariableState {
    variableValue :: VariableValue,
    variableProperties :: VariableProperties
}
    deriving (Show, Eq, Ord, Generic, NFData)

-- Whether or not the value needs quoting (has spaces/globs), or we don't know
data SpaceStatus = SpaceStatusEmpty | SpaceStatusClean | SpaceStatusDirty deriving (Show, Eq, Ord, Generic, NFData)
--
-- Whether or not the value needs quoting (has spaces/globs), or we don't know
data NumericalStatus = NumericalStatusUnknown | NumericalStatusEmpty | NumericalStatusMaybe | NumericalStatusDefinitely deriving (Show, Eq, Ord, Generic, NFData)

-- The set of possible sets of properties for this variable
type VariableProperties = S.Set (S.Set CFVariableProp)

defaultProperties = S.singleton S.empty

unknownVariableState = VariableState {
    variableValue = unknownVariableValue,
    variableProperties = defaultProperties
}

unknownVariableValue = VariableValue {
    literalValue = Nothing,
    spaceStatus = SpaceStatusDirty,
    numericalStatus = NumericalStatusUnknown
}

emptyVariableValue = unknownVariableValue {
    literalValue = Just "",
    spaceStatus = SpaceStatusEmpty,
    numericalStatus = NumericalStatusEmpty
}

unsetVariableState = VariableState {
    variableValue = emptyVariableValue,
    variableProperties = defaultProperties
}

mergeVariableState a b = VariableState {
    variableValue = mergeVariableValue (variableValue a) (variableValue b),
    variableProperties = S.union (variableProperties a) (variableProperties b)
}

mergeVariableValue a b = VariableValue {
    literalValue = if literalValue a == literalValue b then literalValue a else Nothing,
    spaceStatus = mergeSpaceStatus (spaceStatus a) (spaceStatus b),
    numericalStatus = mergeNumericalStatus (numericalStatus a) (numericalStatus b)
}

mergeSpaceStatus a b =
    case (a,b) of
        (SpaceStatusEmpty, y) -> y
        (x, SpaceStatusEmpty) -> x
        (SpaceStatusClean, SpaceStatusClean) -> SpaceStatusClean
        _ -> SpaceStatusDirty

mergeNumericalStatus a b =
    case (a,b) of
        (NumericalStatusDefinitely, NumericalStatusDefinitely) -> NumericalStatusDefinitely
        (NumericalStatusDefinitely, _) -> NumericalStatusMaybe
        (_, NumericalStatusDefinitely) -> NumericalStatusMaybe
        (NumericalStatusMaybe, _) -> NumericalStatusMaybe
        (_, NumericalStatusMaybe) -> NumericalStatusMaybe
        (NumericalStatusEmpty, NumericalStatusEmpty) -> NumericalStatusEmpty
        _ -> NumericalStatusUnknown

-- A VersionedMap is a Map that keeps an additional integer version to quickly determine if it has changed.
-- * Version -1 means it's unknown (possibly and presumably changed)
-- * Version 0 means it's empty
-- * Version N means it's equal to any other map with Version N (this is required but not enforced)
data VersionedMap k v = VersionedMap {
    mapVersion :: Integer,
    mapStorage :: M.Map k v
}
    deriving (Generic, NFData)

-- This makes states more readable but inhibits copy-paste
instance (Show k, Show v) => Show (VersionedMap k v) where
    show m = (if mapVersion m >= 0 then "V" ++ show (mapVersion m) else "U") ++ " " ++ show (mapStorage m)

instance Eq InternalState where
    (==) a b = stateIsQuickEqual a b || stateIsSlowEqual a b

instance (Eq k, Eq v) => Eq (VersionedMap k v) where
    (==) a b = vmIsQuickEqual a b || mapStorage a == mapStorage b

instance (Ord k, Ord v) => Ord (VersionedMap k v) where
    compare a b =
        if vmIsQuickEqual a b
        then EQ
        else mapStorage a `compare` mapStorage b


-- A context with STRefs manually passed around to function.
-- This is done because it was dramatically much faster than any RWS type stack
data Ctx s = Ctx {
    -- The current node
    cNode :: STRef s Node,
    -- The current input state
    cInput :: STRef s InternalState,
    -- The current output state
    cOutput :: STRef s InternalState,

    -- The current functions/subshells stack
    cStack :: [StackEntry s],
    -- The input graph
    cGraph :: CFGraph,
    -- An incrementing counter to version maps
    cCounter :: STRef s Integer,
    -- A cache of input state dependencies to output effects
    cCache :: STRef s (M.Map Node [(S.Set StateDependency, InternalState)]),
    -- Whether the cache is enabled (see fallbackThreshold)
    cEnableCache :: STRef s Bool,
    -- The states resulting from data flows per invocation path
    cInvocations :: STRef s (M.Map [Node] (S.Set StateDependency, M.Map Node (InternalState, InternalState)))
}

-- Whenever a function (or subshell) is invoked, a value like this is pushed onto the stack
data StackEntry s = StackEntry {
    -- The entry point of this stack entry for the purpose of detecting recursion
    entryPoint :: Node,
    -- Whether this is a function call (as opposed to a subshell)
    isFunctionCall :: Bool,
    -- The node where this entry point was invoked
    callSite :: Node,
    -- A mutable set of dependencies we fetched from here or higher in the stack
    dependencies :: STRef s (S.Set StateDependency),
    -- The original input state for this stack entry
    stackState :: InternalState
}
    deriving (Eq, Generic, NFData)


-- Overwrite a base state with the contents of a diff state
-- This is unrelated to join/merge.
patchState :: InternalState -> InternalState -> InternalState
patchState base diff =
    case () of
        _ | sVersion diff == 0 -> base
        _ | sVersion base == 0 -> diff
        _ | stateIsQuickEqual base diff -> diff
        _ ->
            InternalState {
                sVersion = -1,
                sGlobalValues = vmPatch (sGlobalValues base) (sGlobalValues diff),
                sLocalValues = vmPatch (sLocalValues base) (sLocalValues diff),
                sPrefixValues = vmPatch (sPrefixValues base) (sPrefixValues diff),
                sFunctionTargets = vmPatch (sFunctionTargets base) (sFunctionTargets diff),
                sExitCodes = sExitCodes diff `mplus` sExitCodes base,
                sIsReachable = sIsReachable diff `mplus` sIsReachable base
            }

patchOutputM ctx diff = do
    let cOut = cOutput ctx
    oldState <- readSTRef cOut
    let newState = patchState oldState diff
    writeSTRef cOut newState

-- Merge (aka Join) two states. This is monadic because it requires looking up
-- values from the current context. For example:
--
--   f() {
--     foo || x=2
--     HERE         # This merge requires looking up the value of $x in the parent frame
--   }
--   x=1
--   f
mergeState :: forall s. Ctx s -> InternalState -> InternalState -> ST s InternalState
mergeState ctx a b = do
    -- Kludge: we want `readVariable` & friends not to read from an intermediate state,
    --         so temporarily set a blank input.
    let cin = cInput ctx
    old <- readSTRef cin
    writeSTRef cin newInternalState
    x <- merge a b
    writeSTRef cin old
    return x

  where

    merge a b =
        case () of
            _ | sIsReachable a == Just True && sIsReachable b == Just False
                    || sIsReachable a == Just False && sIsReachable b == Just True ->
                error $ pleaseReport "Unexpected merge of reachable and unreachable state"
            _ | sIsReachable a == Just False && sIsReachable b == Just False ->
                return unreachableState
            _ | sVersion a >= 0 && sVersion b >= 0 && sVersion a == sVersion b -> return a
            _ -> do
                globals <- mergeMaps ctx mergeVariableState readGlobal (sGlobalValues a) (sGlobalValues b)
                locals <- mergeMaps ctx mergeVariableState readVariable (sLocalValues a) (sLocalValues b)
                prefix <- mergeMaps ctx mergeVariableState readVariable (sPrefixValues a) (sPrefixValues b)
                funcs <- mergeMaps ctx S.union readFunction (sFunctionTargets a) (sFunctionTargets b)
                exitCodes <- mergeMaybes ctx S.union readExitCodes (sExitCodes a) (sExitCodes b)
                return $ InternalState {
                    sVersion = -1,
                    sGlobalValues = globals,
                    sLocalValues = locals,
                    sPrefixValues = prefix,
                    sFunctionTargets = funcs,
                    sExitCodes = exitCodes,
                    sIsReachable = liftM2 (&&) (sIsReachable a) (sIsReachable b)
                }

-- Merge a number of states, or return a default if there are no states
-- (it can't fold from newInternalState because this would be equivalent of adding a new input edge).
mergeStates :: forall s. Ctx s -> InternalState -> [InternalState] -> ST s InternalState
mergeStates ctx def list =
    case list of
        [] -> return def
        (first:rest) -> foldM (mergeState ctx) first rest

-- Merge two maps, key by key. If both maps have a key, the 'merger' is used.
-- If only one has the key, the 'reader' is used to fetch a second, and the two are merged as above.
mergeMaps :: (Ord k) => forall s.
    Ctx s ->
    (v -> v -> v) ->
    (Ctx s -> k -> ST s v) ->
    (VersionedMap k v) ->
    (VersionedMap k v) ->
    ST s (VersionedMap k v)
mergeMaps ctx merger reader a b =
    if vmIsQuickEqual a b
    then return a
    else do
        new <- M.fromDistinctAscList <$> reverse <$> f [] (M.toAscList $ mapStorage a) (M.toAscList $ mapStorage b)
        vmFromMap ctx new
  where
    f l [] [] = return l
    f l [] b = f l b []
    f l ((k,v):rest1) [] = do
        other <- reader ctx k
        f ((k, merger v other):l) rest1 []
    f l l1@((k1, v1):rest1) l2@((k2, v2):rest2) =
        case k1 `compare` k2 of
            EQ ->
                f ((k1, merger v1 v2):l) rest1 rest2
            LT -> do
                nv2 <- reader ctx k1
                f ((k1, merger v1 nv2):l) rest1 l2
            GT -> do
                nv1 <- reader ctx k2
                f ((k2, merger nv1 v2):l) l1 rest2

-- Merge two Maybes, like mergeMaps for a single element
mergeMaybes ctx merger reader a b =
    case (a, b) of
        (Nothing, Nothing) -> return Nothing
        (Just v1, Nothing) -> single v1
        (Nothing, Just v2) -> single v2
        (Just v1, Just v2) -> return $ Just $ merger v1 v2
  where
    single val = do
        result <- merger val <$> reader ctx
        return $ Just result

vmFromMap ctx map = return $ VersionedMap {
    mapVersion = -1,
    mapStorage = map
}

-- Give a VersionedMap a version if it does not already have one.
versionMap ctx map =
    if mapVersion map >= 0
    then return map
    else do
        v <- nextVersion ctx
        return map {
            mapVersion = v
        }

-- Give an InternalState a version if it does not already have one.
versionState ctx state =
    if sVersion state >= 0
    then return state
    else do
        self <- nextVersion ctx
        ssGlobalValues <- versionMap ctx $ sGlobalValues state
        ssLocalValues <- versionMap ctx $ sLocalValues state
        ssFunctionTargets <- versionMap ctx $ sFunctionTargets state
        return state {
            sVersion = self,
            sGlobalValues = ssGlobalValues,
            sLocalValues = ssLocalValues,
            sFunctionTargets = ssFunctionTargets
        }

-- Like 'not null' but for 2+ elements
is2plus :: [a] -> Bool
is2plus l = case l of
    _:_:_ -> True
    _ -> False

-- Use versions to see if two states are trivially identical
stateIsQuickEqual a b =
    let
        va = sVersion a
        vb = sVersion b
    in
        va >= 0 && vb >= 0 && va == vb

-- A manual slow path 'Eq' (it's not derived because it's part of the custom Eq instance)
stateIsSlowEqual a b =
    check sGlobalValues
    && check sLocalValues
    && check sPrefixValues
    && check sFunctionTargets
    && check sIsReachable
  where
    check f = f a == f b

-- Check if two VersionedMaps are trivially equal
vmIsQuickEqual :: VersionedMap k v -> VersionedMap k v -> Bool
vmIsQuickEqual a b =
    let
        va = mapVersion a
        vb = mapVersion b
    in
        va >= 0 && vb >= 0 && va == vb

-- A new, empty VersionedMap
vmEmpty = VersionedMap {
    mapVersion = 0,
    mapStorage = M.empty
}

-- Map.null for VersionedMaps
vmNull :: VersionedMap k v -> Bool
vmNull m = mapVersion m == 0 || (M.null $ mapStorage m)

-- Map.lookup for VersionedMaps
vmLookup name map = M.lookup name $ mapStorage map

-- Map.insert for VersionedMaps
vmInsert key val map = VersionedMap {
    mapVersion = -1,
    mapStorage = M.insert key val $ mapStorage map
}

-- Overwrite all keys in the first map with values from the second
vmPatch :: (Ord k) => VersionedMap k v -> VersionedMap k v -> VersionedMap k v
vmPatch base diff =
    case () of
        _ | mapVersion base == 0 -> diff
        _ | mapVersion diff == 0 -> base
        _ | vmIsQuickEqual base diff -> diff
        _ -> VersionedMap {
            mapVersion = -1,
            mapStorage = M.unionWith (flip const) (mapStorage base) (mapStorage diff)
        }

-- Set a variable. This includes properties. Applies it to the appropriate scope.
writeVariable :: forall s. Ctx s -> String -> VariableState -> ST s ()
writeVariable ctx name val = do
    typ <- readVariableScope ctx name
    case typ of
        GlobalScope -> writeGlobal ctx name val
        LocalScope -> writeLocal ctx name val
        -- Prefixed variables actually become local variables in the invoked function
        PrefixScope -> writeLocal ctx name val

writeGlobal ctx name val = do
    modifySTRef (cOutput ctx) $ insertGlobal name val

writeLocal ctx name val = do
    modifySTRef (cOutput ctx) $ insertLocal name val

writePrefix ctx name val = do
    modifySTRef (cOutput ctx) $ insertPrefix name val

updateVariableValue ctx name val = do
    (props, scope) <- readVariablePropertiesWithScope ctx name
    let f = case scope of
                GlobalScope -> writeGlobal
                LocalScope -> writeLocal
                PrefixScope -> writeLocal -- Updates become local
    f ctx name $ VariableState { variableValue = val, variableProperties = props }

updateGlobalValue ctx name val = do
    props <- readGlobalProperties ctx name
    writeGlobal ctx name VariableState { variableValue = val, variableProperties = props }

updateLocalValue ctx name val = do
    props <- readLocalProperties ctx name
    writeLocal ctx name VariableState { variableValue = val, variableProperties = props }

updatePrefixValue ctx name val = do
    -- Prefix variables don't inherit properties
    writePrefix ctx name VariableState { variableValue = val, variableProperties = defaultProperties }


-- Look up a variable value, and also return its scope
readVariableWithScope :: forall s. Ctx s -> String -> ST s (VariableState, Scope)
readVariableWithScope ctx name = lookupStack get dep def ctx name
  where
    def = (unknownVariableState, GlobalScope)
    get = getVariableWithScope
    dep k (val, scope) = DepState scope k val

-- Look up the variable's properties. This can be done independently to avoid incurring a dependency on the value.
readVariablePropertiesWithScope :: forall s. Ctx s -> String -> ST s (VariableProperties, Scope)
readVariablePropertiesWithScope ctx name = lookupStack get dep def ctx name
  where
    def = (defaultProperties, GlobalScope)
    get s k = do
        (val, scope) <- getVariableWithScope s k
        return (variableProperties val, scope)
    dep k (val, scope) = DepProperties scope k val

readVariableScope ctx name = snd <$> readVariablePropertiesWithScope ctx name

getVariableWithScope :: InternalState -> String -> Maybe (VariableState, Scope)
getVariableWithScope s name =
    case (vmLookup name $ sPrefixValues s, vmLookup name $ sLocalValues s, vmLookup name $ sGlobalValues s) of
        (Just var, _, _) -> return (var, PrefixScope)
        (_, Just var, _) -> return (var, LocalScope)
        (_, _, Just var) -> return (var, GlobalScope)
        _ -> Nothing

undefineFunction ctx name =
    writeFunction ctx name $ FunctionUnknown

undefineVariable ctx name =
    writeVariable ctx name $ unsetVariableState

readVariable ctx name = fst <$> readVariableWithScope ctx name
readVariableProperties ctx name = fst <$> readVariablePropertiesWithScope ctx name

readGlobal ctx name = lookupStack get dep def ctx name
  where
    def = unknownVariableState -- could come from the environment
    get s name = vmLookup name $ sGlobalValues s
    dep k v = DepState GlobalScope k v


readGlobalProperties ctx name = lookupStack get dep def ctx name
  where
    def = defaultProperties
    get s name = variableProperties <$> (vmLookup name $ sGlobalValues s)
    -- This dependency will fail to match if it's shadowed by a local variable,
    -- such as in  x=1; f() { local -i x; declare -ag x; } because we'll look at
    -- x and find it to be local and not global. FIXME?
    dep k v = DepProperties GlobalScope k v

readLocal ctx name = lookupStackUntilFunction get dep def ctx name
  where
    def = unsetVariableState -- can't come from the environment
    get s name = vmLookup name $ sLocalValues s
    dep k v = DepState LocalScope k v

-- We only want to look up the local properties of the current function,
-- though preferably even if we're in a subshell.  FIXME?
readLocalProperties ctx name = fst <$> lookupStackUntilFunction get dep def ctx name
  where
    def = (defaultProperties, LocalScope)
    with tag f = do
        val <- variableProperties <$> f
        return (val, tag)

    get s name = (with LocalScope $ vmLookup name $ sLocalValues s) `mplus` (with PrefixScope $ vmLookup name $ sPrefixValues s)
    dep k (val, scope) = DepProperties scope k val

readFunction ctx name = lookupStack get dep def ctx name
  where
    def = unknownFunctionValue
    get s name = vmLookup name $ sFunctionTargets s
    dep k v = DepFunction k v

writeFunction ctx name val = do
    modifySTRef (cOutput ctx) $ insertFunction name $ S.singleton val

readExitCodes ctx = lookupStack get dep def ctx ()
  where
    get s () = sExitCodes s
    def = S.empty
    dep () v = DepExitCodes v

-- Look up each state on the stack until a value is found (or the default is used),
-- then add this value as a StateDependency.
lookupStack' :: forall s k v.
    -- Whether to stop at function boundaries
    Bool
    -- A function that maybe finds a value from a state
    -> (InternalState -> k -> Maybe v)
    -- A function that creates a dependency on what was found
    -> (k -> v -> StateDependency)
    -- A default value, if the value can't be found anywhere
    -> v
    -- Context
    -> Ctx s
    -- The key to look up
    -> k
    -- Returning the result
    -> ST s v
lookupStack' functionOnly get dep def ctx key = do
    top <- readSTRef $ cInput ctx
    case get top key of
        Just v -> return v
        Nothing -> f (cStack ctx)
  where
    f [] = return def
    f (s:_) | functionOnly && isFunctionCall s = return def
    f (s:rest) = do
        -- Go up the stack until we find the value, and add
        -- a dependency on each state (including where it was found)
        res <- fromMaybe (f rest) (return <$> get (stackState s) key)
        modifySTRef (dependencies s) $ S.insert $ dep key res
        return res

lookupStack = lookupStack' False
lookupStackUntilFunction = lookupStack' True

-- Like lookupStack but without adding dependencies
peekStack get def ctx key = do
    top <- readSTRef $ cInput ctx
    case get top key of
        Just v -> return v
        Nothing -> f (cStack ctx)
  where
    f [] = return def
    f (s:rest) =
        case get (stackState s) key of
            Just v -> return v
            Nothing -> f rest

-- Check if the current context fulfills a StateDependency if entering `entry`
fulfillsDependency ctx entry dep =
    case dep of
        DepState scope name val -> (== (val, scope)) <$> peek scope ctx name
        DepProperties scope name props -> do
            (state, s) <- peek scope ctx name
            return $ scope == s && variableProperties state == props
        DepFunction name val -> (== val) <$> peekFunc ctx name
        -- Hack. Since we haven't pushed the soon-to-be invoked function on the stack,
        -- it won't be found by the normal check.
        DepIsRecursive node val | node == entry -> return True
        DepIsRecursive node val -> return $ val == any (\f -> entryPoint f == node) (cStack ctx)
        DepExitCodes val -> (== val) <$> peekStack (\s k -> sExitCodes s) S.empty ctx ()
  --      _ -> error $ "Unknown dep " ++ show dep
  where
    peek scope = peekStack getVariableWithScope $ if scope == GlobalScope then (unknownVariableState, GlobalScope) else (unsetVariableState, LocalScope)
    peekFunc = peekStack (\state name -> vmLookup name $ sFunctionTargets state) unknownFunctionValue

-- Check if the current context fulfills all StateDependencies
fulfillsDependencies ctx entry deps =
    f $ S.toList deps
  where
    f [] = return True
    f (dep:rest) = do
        res <- fulfillsDependency ctx entry dep
        if res
            then f rest
            else return False

-- Create a brand new Ctx given a Control Flow Graph (CFG)
newCtx g = do
    c <- newSTRef 1
    input <- newSTRef undefined
    output <- newSTRef undefined
    node <- newSTRef undefined
    cache <- newSTRef M.empty
    enableCache <- newSTRef True
    invocations <- newSTRef M.empty
    return $ Ctx {
        cCounter = c,
        cInput = input,
        cOutput = output,
        cNode = node,
        cCache = cache,
        cEnableCache = enableCache,
        cStack = [],
        cInvocations = invocations,
        cGraph = g
    }

-- The next incrementing version for VersionedMaps
nextVersion ctx = do
    let ctr = cCounter ctx
    n <- readSTRef ctr
    writeSTRef ctr $! n+1
    return n

-- Create a new StackEntry
newStackEntry ctx point isCall = do
    deps <- newSTRef S.empty
    state <- readSTRef $ cOutput ctx
    callsite <- readSTRef $ cNode ctx
    return $ StackEntry {
        entryPoint = point,
        isFunctionCall = isCall,
        callSite = callsite,
        dependencies = deps,
        stackState = state
    }

-- Call a function with a new stack entry on the stack
withNewStackFrame ctx node isCall f = do
    newEntry <- newStackEntry ctx node isCall
    newInput <- newSTRef newInternalState
    newOutput <- newSTRef newInternalState
    newNode <- newSTRef node
    let newCtx = ctx {
        cInput = newInput,
        cOutput = newOutput,
        cNode = newNode,
        cStack = newEntry : cStack ctx
    }
    x <- f newCtx

    {-
    deps <- readSTRef $ dependencies newEntry
    selfcheck <- fulfillsDependencies newCtx deps
    unless selfcheck $ error $ pleaseReport $ "Unmet stack dependencies on " ++ show (node, deps)
    -}

    return (x, newEntry)

-- Check if invoking this function would be a recursive loop
-- (i.e. we already have the function on the stack)
wouldBeRecursive ctx node = f (cStack ctx)
  where
    f [] = return False
    f (s:rest) = do
        res <-
            if entryPoint s == node
            then return True
            else f rest
        modifySTRef (dependencies s) $ S.insert $ DepIsRecursive node res
        return res

-- The main DFA 'transfer' function, applying the effects of a node to the output state
transfer ctx label =
  --traceShow ("Transferring", label) $
    case label of
        CFStructuralNode -> return ()
        CFEntryPoint _ -> return ()
        CFImpliedExit -> return ()
        CFResolvedExit {} -> return ()

        CFExecuteCommand cmd -> transferCommand ctx cmd
        CFExecuteSubshell reason entry exit -> transferSubshell ctx reason entry exit
        CFApplyEffects effects -> mapM_ (\(IdTagged _ f) -> transferEffect ctx f) effects
        CFSetExitCode id -> transferExitCode ctx id

        CFUnresolvedExit -> patchOutputM ctx unreachableState
        CFUnreachable -> patchOutputM ctx unreachableState

        -- TODO
        CFSetBackgroundPid _ -> return ()
        CFDropPrefixAssignments {} ->
            modifySTRef (cOutput ctx) $ \c -> modified c { sPrefixValues = vmEmpty }
--        _ -> error $ "Unknown " ++ show label


-- Transfer the effects of a subshell invocation. This is similar to a function call
-- to allow easily discarding the effects (otherwise the InternalState would have
-- to represent subshell depth, while this way it can simply use the function stack).
transferSubshell ctx reason entry exit = do
    let cout = cOutput ctx
    initial <- readSTRef cout
    runCached ctx entry (f entry exit)
    res <- readSTRef cout
    -- Clear subshell changes. TODO: track this to warn about modifications.
    writeSTRef cout $ initial {
        sExitCodes = sExitCodes res
    }
  where
    f entry exit ctx = do
        (states, frame) <- withNewStackFrame ctx entry False (flip dataflow $ entry)
        let (_, res) = fromMaybe (error $ pleaseReport "Subshell has no exit") $ M.lookup exit states
        deps <- readSTRef $ dependencies frame
        registerFlowResult ctx entry states deps
        return (deps, res)

-- Transfer the effects of executing a command, i.e. the merged union of all possible function definitions.
transferCommand ctx Nothing = return ()
transferCommand ctx (Just name) = do
    targets <- readFunction ctx name
    logVerbose ("Transferring ",name,targets)
    transferMultiple ctx $ map (flip transferFunctionValue) $ S.toList targets

-- Transfer a set of function definitions and merge the output states.
transferMultiple ctx funcs = do
    logVerbose ("Transferring set of ", length funcs)
    original <- readSTRef out
    branches <- mapM (apply ctx original) funcs
    merged <- mergeStates ctx original branches
    let patched = patchState original merged
    writeSTRef out patched
  where
    out = cOutput ctx
    apply ctx original f = do
        writeSTRef out original
        f ctx
        readSTRef out

-- Transfer the effects of a single function definition.
transferFunctionValue ctx funcVal =
    case funcVal of
        FunctionUnknown -> return ()
        FunctionDefinition name entry exit -> do
            isRecursive <- wouldBeRecursive ctx entry
            if isRecursive
                then return () -- TODO: Find a better strategy for recursion
                else runCached ctx entry (f name entry exit)
  where
    f name entry exit ctx = do
        (states, frame) <- withNewStackFrame ctx entry True (flip dataflow $ entry)
        deps <- readSTRef $ dependencies frame
        let res =
                case M.lookup exit states of
                    Just (input, output) -> do
                        -- Discard local variables. TODO: track&retain variables declared local in previous scopes?
                        modified output { sLocalValues = vmEmpty }
                    Nothing -> do
                        -- e.g. f() { exit; }
                        unreachableState
        registerFlowResult ctx entry states deps
        return (deps, res)

transferExitCode ctx id = do
    modifySTRef (cOutput ctx) $ setExitCode id

-- Register/save the result of a dataflow of a function.
-- At the end, all the different values from different flows are merged together.
registerFlowResult ctx entry states deps = do
    -- This function is called in the context of a CFExecuteCommand and not its invoked function,
    -- so manually add the current node to the stack.
    current <- readSTRef $ cNode ctx
    let parents = map callSite $ cStack ctx
    -- A unique path to this flow context. The specific value doesn't matter, as long as it's
    -- unique per invocation of the function. This is required so that 'x=1; f; x=2; f' won't
    -- overwrite each other.
    let path = entry : current : parents
    modifySTRef (cInvocations ctx) $ M.insert path (deps, states)


-- Look up a node in the cache and see if the dependencies of any entries are matched.
-- In that case, reuse the previous result instead of doing a new data flow.
runCached :: forall s. Ctx s -> Node -> (Ctx s -> ST s (S.Set StateDependency, InternalState)) -> ST s ()
runCached ctx node f = do
    cache <- getCache ctx node
    case cache of
        Just v -> do
            logInfo ("Running cached", node)
            -- do { (deps, diff) <- f ctx; unless (v == diff) $ traceShowM ("Cache FAILED to match actual result", node, deps, diff); }
            patchOutputM ctx v

        Nothing -> do
            logInfo ("Cache failed", node)
            (deps, diff) <- f ctx
            modifySTRef (cCache ctx) (M.insertWith (\_ old -> (deps, diff):(take cacheEntries old)) node [(deps,diff)])
            logVerbose ("Recomputed cache for", node, deps)
            -- do { f <- fulfillsDependencies ctx node deps; unless (f) $ traceShowM ("New dependencies FAILED to match", node, deps); }
            patchOutputM ctx diff

-- Get a cached version whose dependencies are currently fulfilled, if any.
getCache :: forall s. Ctx s -> Node -> ST s (Maybe InternalState)
getCache ctx node = do
    cache <- readSTRef $ cCache ctx
    enable <- readSTRef $ cEnableCache ctx
    logVerbose ("Cache for", node, "length", length $ M.findWithDefault [] node cache, M.lookup node cache)
    if enable
        then f $ M.findWithDefault [] node cache
        else return Nothing
  where
    f [] = return Nothing
    f ((deps, value):rest) = do
        match <- fulfillsDependencies ctx node deps
        if match
            then return $ Just value
            else f rest

-- Transfer a single CFEffect to the output state.
transferEffect ctx effect =
    case effect of
        CFReadVariable name ->
            case name of
                "?" -> void $ readExitCodes ctx
                _ -> void $ readVariable ctx name
        CFWriteVariable name value -> do
            val <- cfValueToVariableValue ctx value
            updateVariableValue ctx name val
        CFWriteGlobal name value -> do
            val <- cfValueToVariableValue ctx value
            updateGlobalValue ctx name val
        CFWriteLocal name value -> do
            val <- cfValueToVariableValue ctx value
            updateLocalValue ctx name val
        CFWritePrefix name value -> do
            val <- cfValueToVariableValue ctx value
            updatePrefixValue ctx name val

        CFSetProps scope name props ->
            case scope of
                DefaultScope -> do
                    state <- readVariable ctx name
                    writeVariable ctx name $ addProperties props state
                GlobalScope -> do
                    state <- readGlobal ctx name
                    writeGlobal ctx name $ addProperties props state
                LocalScope -> do
                    out <- readSTRef (cOutput ctx)
                    state <- readLocal ctx name
                    writeLocal ctx name $ addProperties props state
                PrefixScope -> do
                    -- Prefix values become local
                    state <- readLocal ctx name
                    writeLocal ctx name $ addProperties props state

        CFUnsetProps scope name props ->
            case scope of
                DefaultScope -> do
                    state <- readVariable ctx name
                    writeVariable ctx name $ removeProperties props state
                GlobalScope -> do
                    state <- readGlobal ctx name
                    writeGlobal ctx name $ removeProperties props state
                LocalScope -> do
                    out <- readSTRef (cOutput ctx)
                    state <- readLocal ctx name
                    writeLocal ctx name $ removeProperties props state
                PrefixScope -> do
                    -- Prefix values become local
                    state <- readLocal ctx name
                    writeLocal ctx name $ removeProperties props state


        CFUndefineVariable name -> undefineVariable ctx name
        CFUndefineFunction name -> undefineFunction ctx name
        CFUndefine name -> do
            -- This should really just unset one or the other
            undefineVariable ctx name
            undefineFunction ctx name
        CFDefineFunction name id entry exit ->
            writeFunction ctx name $ FunctionDefinition name entry exit

        -- TODO
        CFUndefineNameref name -> undefineVariable ctx name
        CFHintArray name -> return ()
        CFHintDefined name -> return ()
--        _ -> error $ "Unknown effect " ++ show effect


-- Transfer the CFG's idea of a value into our VariableState
cfValueToVariableValue ctx val =
    case val of
        CFValueArray -> return unknownVariableValue -- TODO: Track array status
        CFValueComputed _ parts -> foldM f emptyVariableValue parts
        CFValueInteger -> return unknownIntegerValue
        CFValueString -> return unknownVariableValue
        CFValueUninitialized -> return emptyVariableValue
--        _ -> error $ "Unknown value: " ++ show val
  where
    f val part = do
        next <- computeValue ctx part
        return $ val `appendVariableValue` next

-- A value can be computed from 0 or more parts, such as x="literal$y$z"
computeValue ctx part =
    case part of
        CFStringLiteral str -> return $ literalToVariableValue str
        CFStringInteger -> return unknownIntegerValue
        CFStringUnknown -> return unknownVariableValue
        CFStringVariable name -> variableStateToValue <$> readVariable ctx name
   where
    variableStateToValue state =
        case () of
            _ | all (CFVPInteger `S.member`) $ variableProperties state -> unknownIntegerValue
            _ -> variableValue state

-- Append two VariableValues as if with z="$x$y"
appendVariableValue :: VariableValue -> VariableValue -> VariableValue
appendVariableValue a b =
    unknownVariableValue {
        literalValue = liftM2 (++) (literalValue a) (literalValue b),
        spaceStatus = appendSpaceStatus (spaceStatus a) (spaceStatus b),
        numericalStatus = appendNumericalStatus (numericalStatus a) (numericalStatus b)
    }

appendSpaceStatus a b =
    case (a,b) of
        (SpaceStatusEmpty, _) -> b
        (_, SpaceStatusEmpty) -> a
        (SpaceStatusClean, SpaceStatusClean) -> a
        _ ->SpaceStatusDirty

appendNumericalStatus a b =
    case (a,b) of
        (NumericalStatusEmpty, x) -> x
        (x, NumericalStatusEmpty) -> x
        (NumericalStatusDefinitely, NumericalStatusDefinitely) -> NumericalStatusDefinitely
        (NumericalStatusUnknown, _) -> NumericalStatusUnknown
        (_, NumericalStatusUnknown) -> NumericalStatusUnknown
        _ -> NumericalStatusMaybe

unknownIntegerValue = unknownVariableValue {
    literalValue = Nothing,
    spaceStatus = SpaceStatusClean,
    numericalStatus = NumericalStatusDefinitely
}

literalToVariableValue str = unknownVariableValue {
    literalValue = Just str,
    spaceStatus = literalToSpaceStatus str,
    numericalStatus = literalToNumericalStatus str
}

withoutChanges ctx f = do
    let inp = cInput ctx
    let out = cOutput ctx
    prevInput <- readSTRef inp
    prevOutput <- readSTRef out
    res <- f
    writeSTRef inp prevInput
    writeSTRef out prevOutput
    return res

-- Get the SpaceStatus for a literal string, i.e. if it needs quoting
literalToSpaceStatus str =
    case str of
        "" -> SpaceStatusEmpty
        _ | all (`notElem` " \t\n*?[") str -> SpaceStatusClean
        _ -> SpaceStatusDirty

-- Get the NumericalStatus for a literal string, i.e. whether it's an integer
literalToNumericalStatus str =
    case str of
        "" -> NumericalStatusEmpty
        '-':rest -> if isNumeric rest then NumericalStatusDefinitely else NumericalStatusUnknown
        rest -> if isNumeric rest then NumericalStatusDefinitely else NumericalStatusUnknown
  where
    isNumeric = all isDigit

type StateMap = M.Map Node (InternalState, InternalState)

-- Classic, iterative Data Flow Analysis. See Wikipedia for a description of the process.
dataflow :: forall s. Ctx s -> Node -> ST s StateMap
dataflow ctx entry = do
    pending <- newSTRef $ S.singleton entry
    states <- newSTRef $ M.empty
    -- Should probably be done via a stack frame instead
    withoutChanges ctx $
        f iterationCount pending states
    readSTRef states
  where
    graph = cGraph ctx
    f 0 _ _ = error $ pleaseReport "DFA did not reach fix point"
    f n pending states = do
        ps <- readSTRef pending

        when (n == fallbackThreshold) $ do
            -- This should never happen, but has historically been due to caching bugs.
            -- Try disabling the cache and continuing.
            logInfo "DFA is not stabilizing! Disabling cache."
            writeSTRef (cEnableCache ctx) False

        if S.null ps
            then return ()
            else do
                let (next, rest) = S.deleteFindMin ps
                nexts <- process states next
                writeSTRef pending $ foldl (flip S.insert) rest nexts
                f (n-1) pending states

    process states node = do
        stateMap <- readSTRef states
        let inputs = filter (\c -> sIsReachable c /= Just False) $ mapMaybe (\c -> fmap snd $ M.lookup c stateMap) incoming
        input <-
            case incoming of
                [] -> return newInternalState
                _ ->
                    case inputs of
                        [] -> return unreachableState
                        (x:rest) -> foldM (mergeState ctx) x rest
        writeSTRef (cInput ctx) $ input
        writeSTRef (cOutput ctx) $ input
        writeSTRef (cNode ctx) $ node
        transfer ctx label
        newOutput <- readSTRef $ cOutput ctx
        result <-
            if is2plus outgoing
            then
                -- Version the state because we split and will probably merge later
                versionState ctx newOutput
            else return newOutput
        writeSTRef states $ M.insert node (input, result) stateMap
        case M.lookup node stateMap of
            Nothing -> return outgoing
            Just (oldInput, oldOutput) ->
                if oldOutput == result
                then return []
                else return outgoing
      where
        (incomingL, _, label, outgoingL) = context graph $ node
        incoming = map snd $ filter isRegular $ incomingL
        outgoing = map snd outgoingL
        isRegular = ((== CFEFlow) . fst)

runRoot ctx env entry exit = do
    writeSTRef (cInput ctx) $ env
    writeSTRef (cOutput ctx) $ env
    writeSTRef (cNode ctx) $ entry
    (states, frame) <- withNewStackFrame ctx entry False $ \c -> dataflow c entry
    deps <- readSTRef $ dependencies frame
    registerFlowResult ctx entry states deps
    -- Return the final state, used to invoke functions that were declared but not invoked
    return $ snd $ fromMaybe (error $ pleaseReport "Missing exit state") $ M.lookup exit states


analyzeControlFlow :: CFGParameters -> Token -> CFGAnalysis
analyzeControlFlow params t =
    let
        cfg = buildGraph params t
        (entry, exit) = M.findWithDefault (error $ pleaseReport "Missing root") (getId t) (cfIdToRange cfg)
    in
        runST $ f cfg entry exit
  where
    f cfg entry exit = do
        let env = createEnvironmentState
        ctx <- newCtx $ cfGraph cfg
        -- Do a dataflow analysis starting on the root node
        exitState <- runRoot ctx env entry exit

        -- All nodes we've touched
        invocations <- readSTRef $ cInvocations ctx
        let invokedNodes = M.fromDistinctAscList $ map (\c -> (c, ())) $ S.toList $ M.keysSet $ groupByNode $ M.map snd invocations

        -- Invoke all functions that were declared but not invoked
        -- This is so that we still get warnings for dead code
        -- (it's probably not actually dead, just used by a script that sources ours)
        let declaredFunctions = getFunctionTargets exitState
        let uninvoked = M.difference declaredFunctions invokedNodes

        let stragglerInput =
                (env `patchState` exitState) {
                    -- We don't want `die() { exit $?; }; echo "Sourced"` to assume $? is always echo
                    sExitCodes = Nothing
                }

        analyzeStragglers ctx stragglerInput uninvoked

        -- Now round up all the states from all data flows
        -- (FIXME: this excludes functions that were defined in straggling functions)
        invocations <- readSTRef $ cInvocations ctx
        invokedStates <- flattenByNode ctx $ groupByNode $ M.map addDeps invocations

        -- Fill in the map with unreachable states for anything we didn't get to
        let baseStates = M.fromDistinctAscList $ map (\c -> (c, (unreachableState, unreachableState))) $ uncurry enumFromTo $ nodeRange $ cfGraph cfg
        let allStates = M.unionWith (flip const) baseStates invokedStates

        -- Convert to external states
        let nodeToData = M.map (\(a,b) -> (internalToExternal a, internalToExternal b)) allStates

        return $ nodeToData `deepseq` CFGAnalysis {
            graph = cfGraph cfg,
            tokenToRange = cfIdToRange cfg,
            tokenToNodes = cfIdToNodes cfg,
            nodeToData = nodeToData,
            postDominators = cfPostDominators cfg
        }


    -- Include the dependencies in the state of each function, e.g. if it depends on `x=foo` then add that.
    addDeps :: (S.Set StateDependency, M.Map Node (InternalState, InternalState)) -> M.Map Node (InternalState, InternalState)
    addDeps (deps, m) = let base = depsToState deps in M.map (\(a,b) -> (base `patchState` a, base `patchState` b)) m

    -- Collect all the states that each node has resulted in.
    groupByNode :: forall k v. M.Map k (M.Map Node v) -> M.Map Node [v]
    groupByNode pathMap = M.fromListWith (++) $ map (\(k,v) -> (k,[v])) $ concatMap M.toList $ M.elems pathMap

    -- Merge all the pre/post states for each node. This would have been a foldM if Map had one.
    flattenByNode ctx m = M.fromDistinctAscList <$> (mapM (mergePair ctx) $ M.toList m)

    mergeAllStates ctx pairs =
        let
            (pres, posts) = unzip pairs
        in do
            pre <- mergeStates ctx (error $ pleaseReport "Null node states") pres
            post <- mergeStates ctx (error $ pleaseReport "Null node states") posts
            return (pre, post)

    mergePair ctx (node, list) = do
        merged <- mergeAllStates ctx list
        return (node, merged)

    -- Get the all the functions defined in an InternalState
    getFunctionTargets :: InternalState -> M.Map Node FunctionDefinition
    getFunctionTargets state =
        let
            declaredFuncs = S.unions $ mapStorage $ sFunctionTargets state
            getFunc d =
                case d of
                    FunctionDefinition _ entry _ -> Just (entry, d)
                    _ -> Nothing
            funcs = mapMaybe getFunc $ S.toList declaredFuncs
        in
            M.fromList funcs


analyzeStragglers ctx state stragglers = do
    mapM_ f $ M.elems stragglers
  where
    f def@(FunctionDefinition name entry exit) = do
        writeSTRef (cInput ctx) state
        writeSTRef (cOutput ctx) state
        writeSTRef (cNode ctx) entry
        transferFunctionValue ctx def



return []
runTests = $quickCheckAll
