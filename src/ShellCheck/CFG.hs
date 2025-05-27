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
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

-- Constructs a Control Flow Graph from an AST
module ShellCheck.CFG (
    CFNode (..),
    CFEdge (..),
    CFEffect (..),
    CFStringPart (..),
    CFVariableProp (..),
    CFGResult (..),
    CFValue (..),
    CFGraph,
    CFGParameters (..),
    IdTagged (..),
    Scope (..),
    buildGraph
    , ShellCheck.CFG.runTests -- STRIP
    )
  where

import GHC.Generics (Generic)
import ShellCheck.AST
import ShellCheck.ASTLib
import ShellCheck.Data
import ShellCheck.Interface
import ShellCheck.Prelude
import ShellCheck.Regex
import Control.DeepSeq
import Control.Monad
import Control.Monad.Identity
import Data.Array.Unboxed
import Data.Array.ST
import Data.List hiding (map)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.RWS.Lazy
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.DFS
import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.Query.Dominators
import Data.Graph.Inductive.PatriciaTree as G
import Debug.Trace -- STRIP

import Test.QuickCheck.All (forAllProperties)
import Test.QuickCheck.Test (quickCheckWithResult, stdArgs, maxSuccess)


-- Our basic Graph type
type CFGraph = G.Gr CFNode CFEdge

-- Node labels in a Control Flow Graph
data CFNode =
    -- A no-op node for structural purposes
    CFStructuralNode
    -- A no-op for graph inspection purposes
    | CFEntryPoint String
    -- Drop current prefix assignments
    | CFDropPrefixAssignments
    -- A node with a certain effect on program state
    | CFApplyEffects [IdTagged CFEffect]
    -- The execution of a command or function by literal string if possible
    | CFExecuteCommand (Maybe String)
    -- Execute a subshell. These are represented by disjoint graphs just like
    -- functions, but they don't require any form of name resolution
    | CFExecuteSubshell String Node Node
    -- Assignment of $?
    | CFSetExitCode Id
    -- The virtual 'exit' at the natural end of a subshell
    | CFImpliedExit
    -- An exit statement resolvable at CFG build time
    | CFResolvedExit
    -- An exit statement only resolvable at DFA time
    | CFUnresolvedExit
    -- An unreachable node, serving as the unconnected end point of a range
    | CFUnreachable
    -- Assignment of $!
    | CFSetBackgroundPid Id
  deriving (Eq, Ord, Show, Generic, NFData)

-- Edge labels in a Control Flow Graph
data CFEdge =
    CFEErrExit
    -- Regular control flow edge
    | CFEFlow
    -- An edge that a human might think exists (e.g. from a backgrounded process to its parent)
    | CFEFalseFlow
    -- An edge followed on exit
    | CFEExit
  deriving (Eq, Ord, Show, Generic, NFData)

-- Actions we track
data CFEffect =
    CFSetProps (Maybe Scope) String (S.Set CFVariableProp)
    | CFUnsetProps (Maybe Scope) String (S.Set CFVariableProp)
    | CFReadVariable String
    | CFWriteVariable String CFValue
    | CFWriteGlobal String CFValue
    | CFWriteLocal String CFValue
    | CFWritePrefix String CFValue
    | CFDefineFunction String Id Node Node
    | CFUndefine String
    | CFUndefineVariable String
    | CFUndefineFunction String
    | CFUndefineNameref String
    -- Usage implies that this is an array (e.g. it's expanded with index)
    | CFHintArray String
    -- Operation implies that the variable will be defined (e.g. [ -z "$var" ])
    | CFHintDefined String
  deriving (Eq, Ord, Show, Generic, NFData)

data IdTagged a = IdTagged Id a
  deriving (Eq, Ord, Show, Generic, NFData)

-- Where a variable's value comes from
data CFValue =
    -- The special 'uninitialized' value
    CFValueUninitialized
    -- An arbitrary array value
    | CFValueArray
    -- An arbitrary string value
    | CFValueString
    -- An arbitrary integer
    | CFValueInteger
    -- Token 'Id' concatenates and assigns the given parts
    | CFValueComputed Id [CFStringPart]
  deriving (Eq, Ord, Show, Generic, NFData)

-- Simplified computed strings
data CFStringPart =
    -- A known literal string value, like 'foo'
    CFStringLiteral String
    -- The contents of a variable, like $foo (may not be a string)
    | CFStringVariable String
    -- An value that is unknown but an integer
    | CFStringInteger
    -- An unknown string value, for things we can't handle
    | CFStringUnknown
  deriving (Eq, Ord, Show, Generic, NFData)

-- The properties of a variable
data CFVariableProp = CFVPExport | CFVPArray | CFVPAssociative | CFVPInteger
  deriving (Eq, Ord, Show, Generic, NFData)

-- Options when generating CFG
data CFGParameters = CFGParameters {
    -- Whether the last element in a pipeline runs in the current shell
    cfLastpipe :: Bool,
    -- Whether all elements in a pipeline count towards the exit status
    cfPipefail :: Bool
}

data CFGResult = CFGResult {
    -- The graph itself
    cfGraph :: CFGraph,
    -- Map from Id to nominal start&end node (i.e. assuming normal execution without exits)
    cfIdToRange :: M.Map Id (Node, Node),
    -- A set of all nodes belonging to an Id, recursively
    cfIdToNodes :: M.Map Id (S.Set Node),
    -- An array (from,to) saying whether 'from' postdominates 'to'
    cfPostDominators :: Array Node [Node]
}
  deriving (Show)

buildGraph :: CFGParameters -> Token -> CFGResult
buildGraph params root =
    let
        (nextNode, base) = execRWS (buildRoot root) (newCFContext params) 0
        (nodes, edges, mapping, association) =
--            renumberTopologically $
                removeUnnecessaryStructuralNodes
                    base

        idToRange = M.fromList mapping
        isRealEdge (from, to, edge) = case edge of CFEFlow -> True; CFEExit -> True; _ -> False
        onlyRealEdges = filter isRealEdge edges
        (_, mainExit) = fromJust $ M.lookup (getId root) idToRange

        result = CFGResult {
            cfGraph = mkGraph nodes edges,
            cfIdToRange = idToRange,
            cfIdToNodes = M.fromListWith S.union $ map (\(id, n) -> (id, S.singleton n)) association,
            cfPostDominators = findPostDominators mainExit $ mkGraph nodes onlyRealEdges
        }
    in
        result

remapGraph :: M.Map Node Node -> CFW -> CFW
remapGraph remap (nodes, edges, mapping, assoc) =
    (
        map (remapNode remap) nodes,
        map (remapEdge remap) edges,
        map (\(id, (a,b)) -> (id, (remapHelper remap a, remapHelper remap b))) mapping,
        map (\(id, n) -> (id, remapHelper remap n)) assoc
    )

prop_testRenumbering =
    let
        s = CFStructuralNode
        before = (
            [(1,s), (3,s), (4, s), (8,s)],
            [(1,3,CFEFlow), (3,4, CFEFlow), (4,8,CFEFlow)],
            [(Id 0, (3,4))],
            [(Id 1, 3), (Id 2, 4)]
            )
        after = (
            [(0,s), (1,s), (2,s), (3,s)],
            [(0,1,CFEFlow), (1,2, CFEFlow), (2,3,CFEFlow)],
            [(Id 0, (1,2))],
            [(Id 1, 1), (Id 2, 2)]
            )
    in after == renumberGraph before

-- Renumber the graph for prettiness, so there are no gaps in node numbers
renumberGraph :: CFW -> CFW
renumberGraph g@(nodes, edges, mapping, assoc) =
    let renumbering = M.fromList (flip zip [0..] $ sort $ map fst nodes)
    in remapGraph renumbering g

prop_testRenumberTopologically =
    let
        s = CFStructuralNode
        before = (
            [(4,s), (2,s), (3, s)],
            [(4,2,CFEFlow), (2,3, CFEFlow)],
            [(Id 0, (4,2))],
            []
            )
        after = (
            [(0,s), (1,s), (2,s)],
            [(0,1,CFEFlow), (1,2, CFEFlow)],
            [(Id 0, (0,1))],
            []
            )
    in after == renumberTopologically before

-- Renumber the graph in topological order
renumberTopologically g@(nodes, edges, mapping, assoc) =
    let renumbering = M.fromList (flip zip [0..] $ topsort (mkGraph nodes edges :: CFGraph))
    in remapGraph renumbering g

prop_testRemoveStructural =
    let
        s = CFStructuralNode
        before = (
            [(1,s), (2,s), (3, s), (4,s)],
            [(1,2,CFEFlow), (2,3, CFEFlow), (3,4,CFEFlow)],
            [(Id 0, (2,3))],
            [(Id 0, 3)]
            )
        after = (
            [(1,s), (2,s), (4,s)],
            [(1,2,CFEFlow), (2,4,CFEFlow)],
            [(Id 0, (2,2))],
            [(Id 0, 2)]
            )
    in after == removeUnnecessaryStructuralNodes before

-- Collapse structural nodes that just form long chains like x->x->x.
-- This way we can generate them with abandon, without making DFA slower.
--
-- Note in particular that we can't remove a structural node x in
-- foo -> x -> bar , because then the pre/post-condition for tokens
-- previously pointing to x would be wrong.
removeUnnecessaryStructuralNodes (nodes, edges, mapping, association) =
    remapGraph recursiveRemapping
        (
            filter (\(n, _) -> n `M.notMember` recursiveRemapping) nodes,
            filter (`S.notMember` edgesToCollapse) edges,
            mapping,
            association
        )
  where
    regularEdges = filter isRegularEdge edges
    inDegree = counter $ map (\(from,to,_) -> from) regularEdges
    outDegree = counter $ map (\(from,to,_) -> to) regularEdges
    structuralNodes = S.fromList [node | (node, CFStructuralNode) <- nodes]
    candidateNodes = S.filter isLinear structuralNodes
    edgesToCollapse = S.fromList $ filter filterEdges regularEdges

    remapping :: M.Map Node Node
    remapping = M.fromList $ map orderEdge $ S.toList edgesToCollapse
    recursiveRemapping = M.mapWithKey (\c _ -> recursiveLookup remapping c) remapping

    filterEdges (a,b,_) =
        a `S.member` candidateNodes && b `S.member` candidateNodes

    orderEdge (a,b,_) = if a < b then (b,a) else (a,b)
    counter = M.fromListWith (+) . map (\key -> (key, 1))
    isRegularEdge (_, _, CFEFlow) = True
    isRegularEdge _ = False

    recursiveLookup :: M.Map Node Node -> Node -> Node
    recursiveLookup map node =
        case M.lookup node map of
            Nothing -> node
            Just x -> recursiveLookup map x

    isLinear node =
        M.findWithDefault 0 node inDegree == 1
        && M.findWithDefault 0 node outDegree == 1


remapNode :: M.Map Node Node -> LNode CFNode -> LNode CFNode
remapNode m (node, label) =
    (remapHelper m node, newLabel)
  where
    newLabel = case label of
        CFApplyEffects effects -> CFApplyEffects (map (remapEffect m) effects)
        CFExecuteSubshell s a b -> CFExecuteSubshell s (remapHelper m a) (remapHelper m b)
        _ -> label

remapEffect map old@(IdTagged id effect) =
    case effect of
        CFDefineFunction name id start end -> IdTagged id $ CFDefineFunction name id (remapHelper map start) (remapHelper map end)
        _ -> old

remapEdge :: M.Map Node Node -> LEdge CFEdge -> LEdge CFEdge
remapEdge map (from, to, label) = (remapHelper map from, remapHelper map to, label)
remapHelper map n = M.findWithDefault n n map

data Range = Range Node Node
  deriving (Eq, Show)

data CFContext = CFContext {
    cfIsCondition :: Bool,
    cfIsFunction :: Bool,
    cfLoopStack :: [(Node, Node)],
    cfTokenStack :: [Id],
    cfExitTarget :: Maybe Node,
    cfReturnTarget :: Maybe Node,
    cfParameters :: CFGParameters
}
newCFContext params = CFContext {
    cfIsCondition = False,
    cfIsFunction = False,
    cfLoopStack = [],
    cfTokenStack = [],
    cfExitTarget = Nothing,
    cfReturnTarget = Nothing,
    cfParameters = params
}

-- The monad we generate a graph in
type CFM a = RWS CFContext CFW Int a
type CFW = ([LNode CFNode], [LEdge CFEdge], [(Id, (Node, Node))], [(Id, Node)])

newNode :: CFNode -> CFM Node
newNode label = do
    n <- get
    stack <- asks cfTokenStack
    put (n+1)
    tell ([(n, label)], [], [], map (\c -> (c, n)) stack)
    return n

newNodeRange :: CFNode -> CFM Range
-- newNodeRange label = nodeToRange <$> newNode label
newNodeRange label = nodeToRange <$> newNode label

-- Build a disjoint piece of the graph and return a CFExecuteSubshell. The Id is used purely for debug naming.
subshell :: Id -> String -> CFM Range -> CFM Range
subshell id reason p = do
    start <- newNode $ CFEntryPoint $ "Subshell " ++ show id ++ ": " ++ reason
    end <- newNode CFStructuralNode
    middle <- local (\c -> c { cfExitTarget = Just end, cfReturnTarget = Just end}) p
    linkRanges [nodeToRange start, middle, nodeToRange end]
    newNodeRange $ CFExecuteSubshell reason start end


withFunctionScope p = do
    end <- newNode CFStructuralNode
    body <- local (\c -> c { cfReturnTarget = Just end, cfIsFunction = True }) p
    linkRanges [body, nodeToRange end]

-- Anything that happens recursively in f will be attributed to this id
under :: Id -> CFM a -> CFM a
under id f = local (\c -> c { cfTokenStack = id:(cfTokenStack c) }) f

nodeToRange :: Node -> Range
nodeToRange n = Range n n

link :: Node -> Node -> CFEdge -> CFM ()
link from to label = do
    tell ([], [(from, to, label)], [], [])

registerNode :: Id -> Range -> CFM ()
registerNode id (Range start end) = tell ([], [], [(id, (start, end))], [])

linkRange :: Range -> Range -> CFM Range
linkRange = linkRangeAs CFEFlow

linkRangeAs :: CFEdge -> Range -> Range -> CFM Range
linkRangeAs label (Range start mid1) (Range mid2 end) = do
    link mid1 mid2 label
    return (Range start end)

-- Like linkRange but without actually linking
spanRange :: Range -> Range -> Range
spanRange (Range start mid1) (Range mid2 end) = Range start end

linkRanges :: [Range] -> CFM Range
linkRanges [] = error "Empty range"
linkRanges (first:rest) = foldM linkRange first rest

sequentially :: [Token] -> CFM Range
sequentially list = do
    first <- newStructuralNode
    rest <- mapM build list
    linkRanges (first:rest)

withContext :: (CFContext -> CFContext) -> CFM a -> CFM a
withContext = local

withReturn :: Range -> CFM a -> CFM a
withReturn _ p = p

asCondition :: CFM Range -> CFM Range
asCondition = withContext (\c -> c { cfIsCondition = True })

newStructuralNode = newNodeRange CFStructuralNode

buildRoot :: Token -> CFM Range
buildRoot t = under (getId t) $ do
    entry <- newNodeRange $ CFEntryPoint "MAIN"
    impliedExit <- newNode CFImpliedExit
    end <- newNode CFStructuralNode
    start <- local (\c -> c { cfExitTarget = Just end, cfReturnTarget = Just impliedExit}) $ build t
    range <- linkRanges [entry, start, nodeToRange impliedExit, nodeToRange end]
    registerNode (getId t) range
    return range

applySingle e = CFApplyEffects [e]

-- Build the CFG.
build :: Token -> CFM Range
build t = do
    range <- under (getId t) $ build' t
    registerNode (getId t) range
    return range
  where
    build' t = case t of
        T_Annotation _ _ list -> build list
        T_Script _ _ list -> do
            sequentially list

        TA_Assignment id op var@(TA_Variable _ name indices) rhs -> do
            -- value first: (( var[x=1] = (x=2) )) runs x=1 last
            value <- build rhs
            subscript <- sequentially indices
            read <-
                if op == "="
                then none
                -- This is += or something
                else newNodeRange $ applySingle $ IdTagged id $ CFReadVariable name

            write <- newNodeRange $ applySingle $ IdTagged id $ CFWriteVariable name $
                        if null indices
                        then CFValueInteger
                        else CFValueArray

            linkRanges [value, subscript, read, write]

        TA_Assignment id op lhs rhs -> do
            -- This is likely an invalid assignment like (( 1 = 2 )), but it
            -- could be e.g. x=y; (( $x = 3 )); echo $y, so expand both sides
            -- without updating anything
            sequentially [lhs, rhs]

        TA_Binary _ _ a b -> sequentially [a,b]
        TA_Expansion _ list -> sequentially list
        TA_Sequence _ list -> sequentially list
        TA_Parenthesis _ t -> build t

        TA_Trinary _ cond a b -> do
            condition <- build cond
            ifthen <- build a
            elsethen <- build b
            end <- newStructuralNode
            linkRanges [condition, ifthen, end]
            linkRanges [condition, elsethen, end]

        TA_Variable id name indices -> do
            subscript <- sequentially indices
            hint <-
                if null indices
                then none
                else nodeToRange <$> newNode (applySingle $ IdTagged id $ CFHintArray name)
            read <- nodeToRange <$> newNode (applySingle $ IdTagged id $ CFReadVariable name)
            linkRanges [subscript, hint, read]

        TA_Unary id op (TA_Variable _ name indices) | "--" `isInfixOf` op || "++" `isInfixOf` op -> do
            subscript <- sequentially indices
            read <- newNodeRange $ applySingle $ IdTagged id $ CFReadVariable name
            write <- newNodeRange $ applySingle $ IdTagged id $ CFWriteVariable name $
                        if null indices
                        then CFValueInteger
                        else CFValueArray
            linkRanges [subscript, read, write]
        TA_Unary _ _ arg -> build arg

        TC_And _ SingleBracket _ lhs rhs -> do
            sequentially [lhs, rhs]

        TC_And _ DoubleBracket _ lhs rhs -> do
            left <- build lhs
            right <- build rhs
            end <- newStructuralNode
            -- complete
            linkRanges [left, right, end]
            -- short circuit
            linkRange left end

        -- TODO: Handle integer ops
        TC_Binary _ mode str lhs rhs -> do
            left <- build lhs
            right <- build rhs
            linkRange left right

        TC_Empty {} -> newStructuralNode

        TC_Group _ _ t -> build t

        -- TODO: Mark as checked
        TC_Nullary _ _ arg -> build arg

        TC_Or _ SingleBracket _ lhs rhs -> sequentially [lhs, rhs]

        TC_Or _ DoubleBracket _ lhs rhs -> do
            left <- build lhs
            right <- build rhs
            end <- newStructuralNode
            -- complete
            linkRanges [left, right, end]
            -- short circuit
            linkRange left end

        -- TODO: Handle -v, -z, -n
        TC_Unary _ _ op arg -> do
            build arg

        T_Arithmetic id root -> do
            exe <- build root
            status <- newNodeRange (CFSetExitCode id)
            linkRange exe status

        T_AndIf _ lhs rhs -> do
            left <- build lhs
            right <- build rhs
            end <- newStructuralNode
            linkRange left right
            linkRange right end
            linkRange left end

        T_Array _ list -> sequentially list

        T_Assignment {} -> buildAssignment Nothing t

        T_Backgrounded id body -> do
            start <- newStructuralNode
            fork <- subshell id "backgrounding '&'" $ build body
            pid <- newNodeRange $ CFSetBackgroundPid id
            status <- newNodeRange $ CFSetExitCode id

            linkRange start fork
            -- Add a join from the fork to warn about variable changes
            linkRangeAs CFEFalseFlow fork pid
            linkRanges [start, pid, status]

        T_Backticked id body ->
            subshell id "`..` expansion" $ sequentially body

        T_Banged id cmd -> do
            main <- build cmd
            status <- newNodeRange (CFSetExitCode id)
            linkRange main status

        T_BatsTest id _ body -> do
            -- These are technically set by the 'run' command, but we'll just define them
            -- up front to avoid figuring out which commands named "run" belong to Bats.
            status <- newNodeRange $ applySingle $ IdTagged id $ CFWriteVariable "status" CFValueInteger
            output <- newNodeRange $ applySingle $ IdTagged id $ CFWriteVariable "output" CFValueString
            main <- build body
            linkRanges [status, output, main]

        T_BraceExpansion _ list -> sequentially list

        T_BraceGroup id body ->
            sequentially body

        T_CaseExpression id t [] -> build t

        T_CaseExpression id t list@(hd:tl) -> do
            start <- newStructuralNode
            token <- build t
            branches <- mapM buildBranch (hd NE.:| tl)
            end <- newStructuralNode

            let neighbors = zip (NE.toList branches) $ NE.tail branches
            let (_, firstCond, _) = NE.head branches
            let (_, lastCond, lastBody) = NE.last branches

            linkRange start token
            linkRange token firstCond
            mapM_ (uncurry $ linkBranch end) neighbors
            linkRange lastBody end

            unless (any hasCatchAll list) $
                -- There's no *) branch, so assume we can fall through
                void $ linkRange token end

            return $ spanRange start end

          where
            -- for a | b | c, evaluate each in turn and allow short circuiting
            buildCond list = do
                start <- newStructuralNode
                conds <- mapM build list
                end <- newStructuralNode
                linkRanges (start:conds)
                mapM_ (`linkRange` end) conds
                return $ spanRange start end

            buildBranch (typ, cond, body) = do
                c <- buildCond cond
                b <- sequentially body
                linkRange c b
                return (typ, c, b)

            linkBranch end (typ, cond, body) (_, nextCond, nextBody) = do
                -- Failure case
                linkRange cond nextCond
                -- After body
                case typ of
                    CaseBreak -> linkRange body end
                    CaseFallThrough -> linkRange body nextBody
                    CaseContinue -> linkRange body nextCond

            -- Find a *) if any

            hasCatchAll (_,cond,_) = any isCatchAll cond
            isCatchAll c = fromMaybe False $ do
                pg <- wordToExactPseudoGlob c
                return $ pg `pseudoGlobIsSuperSetof` [PGMany]

        T_Condition id _ op -> do
            cond <- build op
            status <- newNodeRange $ CFSetExitCode id
            linkRange cond status

        T_CoProc id maybeNameToken t -> do
            -- If unspecified, "COPROC". If not a constant string, Nothing.
            let maybeName = case maybeNameToken of
                    Just x -> getLiteralString x
                    Nothing -> Just "COPROC"

            let parentNode = case maybeName of
                    Just str -> applySingle $ IdTagged id $ CFWriteVariable str CFValueArray
                    Nothing -> CFStructuralNode

            start <- newStructuralNode
            parent <- newNodeRange parentNode
            child <- subshell id "coproc" $ build t
            end <- newNodeRange $ CFSetExitCode id

            linkRange start parent
            linkRange start child
            linkRange parent end
            linkRangeAs CFEFalseFlow child end

            return $ spanRange start end
        T_CoProcBody _ t -> build t

        T_DollarArithmetic _ arith -> build arith
        T_DollarDoubleQuoted _ list -> sequentially list
        T_DollarSingleQuoted _ _ -> none
        T_DollarBracket _ t -> build t

        T_DollarBraced id _ t -> do
            let str = concat $ oversimplify t
            let modifier = getBracedModifier str
            let reference = getBracedReference str
            let indices = getIndexReferences str
            let offsets = getOffsetReferences str
            vals <- build t
            others <- mapM (\x -> nodeToRange <$> newNode (applySingle $ IdTagged id $ CFReadVariable x)) (indices ++ offsets)
            deps <- linkRanges (vals:others)
            read <- nodeToRange <$> newNode (applySingle $ IdTagged id $ CFReadVariable reference)
            totalRead <- linkRange deps read

            if any (`isPrefixOf` modifier) ["=", ":="]
              then do
                optionalAssign <- newNodeRange (applySingle $ IdTagged id $ CFWriteVariable reference CFValueString)
                result <- newStructuralNode
                linkRange optionalAssign result
                linkRange totalRead result
              else return totalRead

        T_DoubleQuoted _ list -> sequentially list

        T_DollarExpansion id body ->
            subshell id "$(..) expansion" $ sequentially body

        T_Extglob _ _ list -> sequentially list

        T_FdRedirect id ('{':identifier) op -> do
            let name = takeWhile (/= '}') identifier
            expression <- build op
            rw <- newNodeRange $
                if isClosingFileOp op
                then applySingle $ IdTagged id $ CFReadVariable name
                else applySingle $ IdTagged id $ CFWriteVariable name CFValueInteger

            linkRange expression rw


        T_FdRedirect _ name t -> do
            build t

        T_ForArithmetic _ initT condT incT bodyT -> do
            init <- build initT
            cond <- build condT
            body <- sequentially bodyT
            inc <- build incT
            end <- newStructuralNode

            -- Forward edges
            linkRanges [init, cond, body, inc]
            linkRange cond end
            -- Backward edge
            linkRange inc cond
            return $ spanRange init end

        T_ForIn id name words body -> forInHelper id name words body

        -- For functions we generate an unlinked subgraph, and mention that in its definition node
        T_Function id _ _ name body -> do
            range <- local (\c -> c { cfExitTarget = Nothing }) $ do
                entry <- newNodeRange $ CFEntryPoint $ "function " ++ name
                f <- withFunctionScope $ build body
                linkRange entry f
            let (Range entry exit) = range
            definition <- newNodeRange (applySingle $ IdTagged id $ CFDefineFunction name id entry exit)
            exe <- newNodeRange (CFSetExitCode id)
            linkRange definition exe

        T_Glob {} -> none

        T_HereString _ t -> build t
        T_HereDoc _ _ _ _ list -> sequentially list

        T_IfExpression id ifs elses -> do
            start <- newStructuralNode
            branches <- doBranches start ifs elses []
            end <- newStructuralNode
            mapM_ (`linkRange` end) branches
            return $ spanRange start end
          where
            doBranches start ((conds, thens):rest) elses result = do
                cond <- asCondition $ sequentially conds
                action <- sequentially thens
                linkRange start cond
                linkRange cond action
                doBranches cond rest elses (action:result)
            doBranches start [] elses result = do
                rest <-
                    if null elses
                    then newNodeRange (CFSetExitCode id)
                    else sequentially elses
                linkRange start rest
                return (rest:result)

        T_Include _ t -> build t

        T_IndexedElement _ indicesT valueT -> do
            indices <- sequentially indicesT
            value <- build valueT
            linkRange indices value

        T_IoDuplicate _ op _ -> build op

        T_IoFile _ op t -> do
            exp <- build t
            doesntDoMuch <- build op
            linkRange exp doesntDoMuch

        T_Literal {} -> none

        T_NormalWord _ list -> sequentially list

        T_OrIf _ lhs rhs -> do
            left <- build lhs
            right <- build rhs
            end <- newStructuralNode
            linkRange left right
            linkRange right end
            linkRange left end

        T_Pipeline _ _ [cmd] -> build cmd
        T_Pipeline id _ cmds -> do
            start <- newStructuralNode
            hasLastpipe <- reader $ cfLastpipe . cfParameters
            (leading, last) <- buildPipe hasLastpipe cmds
            -- Ideally we'd let this exit code be that of the last command in the pipeline but ok
            end <- newNodeRange $ CFSetExitCode id

            mapM_ (linkRange start) leading
            mapM_ (\c -> linkRangeAs CFEFalseFlow c end) leading
            linkRanges $ [start] ++ last ++ [end]
          where
            buildPipe True [x] = do
                last <- build x
                return ([], [last])
            buildPipe lp (first:rest) = do
                this <- subshell id "pipeline" $ build first
                (leading, last) <- buildPipe lp rest
                return (this:leading, last)
            buildPipe _ [] = return ([], [])

        T_ProcSub id op cmds -> do
            start <- newStructuralNode
            body <- subshell id (op ++ "() process substitution") $ sequentially cmds
            end <- newStructuralNode

            linkRange start body
            linkRangeAs CFEFalseFlow body end
            linkRange start end

        T_Redirecting _ redirs cmd -> do
            -- For simple commands, this is the other way around in bash
            -- We do it in this order for comound commands like { x=name; } > "$x"
            redir <- sequentially redirs
            body <- build cmd
            linkRange redir body

        T_SelectIn id name words body -> forInHelper id name words body

        T_SimpleCommand id vars [] -> do
            -- Vars can also be empty, as in the command "> foo"
            assignments <- sequentially vars
            status <- newNodeRange (CFSetExitCode id)
            linkRange assignments status

        T_SimpleCommand id vars (cmd:args) ->
            handleCommand t vars (cmd NE.:| args) $ getUnquotedLiteral cmd

        T_SingleQuoted _ _ -> none

        T_SourceCommand _ originalCommand inlinedSource -> do
            cmd <- build originalCommand
            end <- newStructuralNode
            inline <- withReturn end $ build inlinedSource
            linkRange cmd inline
            linkRange inline end
            return $ spanRange cmd inline

        T_Subshell id body -> do
            main <- subshell id "explicit (..) subshell" $ sequentially body
            status <- newNodeRange (CFSetExitCode id)
            linkRange main status

        T_UntilExpression id cond body -> whileHelper id cond body
        T_WhileExpression id cond body -> whileHelper id cond body

        T_CLOBBER _ -> none
        T_GREATAND _ -> none
        T_LESSAND _ -> none
        T_LESSGREAT _ -> none
        T_DGREAT _ -> none
        T_Greater _ -> none
        T_Less _ -> none
        T_ParamSubSpecialChar _ _ -> none

        x -> do
            error ("Unimplemented: " ++ show x) -- STRIP
            none

--  Still in `where` clause
    forInHelper id name words body = do
        entry <- newStructuralNode
        expansion <- sequentially words
        assignmentChoice <- newStructuralNode
        assignments <-
            if null words || any willSplit words
            then (:[]) <$> (newNodeRange $ applySingle $ IdTagged id $ CFWriteVariable name CFValueString)
            else mapM (\t -> newNodeRange $ applySingle $ IdTagged id $ CFWriteVariable name $ CFValueComputed (getId t) $ tokenToParts t) words
        body <- sequentially body
        exit <- newStructuralNode
        -- Forward edges
        linkRanges [entry, expansion, assignmentChoice]
        mapM_ (\t -> linkRanges [assignmentChoice, t, body]) assignments
        linkRange body exit
        linkRange expansion exit
        -- Backward edge
        linkRange body assignmentChoice
        return $ spanRange entry exit

    whileHelper id cond body = do
        condRange <- asCondition $ sequentially cond
        bodyRange <- sequentially body
        end <- newNodeRange (CFSetExitCode id)

        linkRange condRange bodyRange
        linkRange bodyRange condRange
        linkRange condRange end


handleCommand cmd vars args literalCmd = do
    -- TODO: Handle assignments in declaring commands

    case literalCmd of
        Just "exit" -> regularExpansion vars (NE.toList args) $ handleExit
        Just "return" -> regularExpansion vars (NE.toList args) $ handleReturn
        Just "unset" -> regularExpansionWithStatus vars args $ handleUnset args

        Just "declare" -> handleDeclare args
        Just "local" -> handleDeclare args
        Just "typeset" -> handleDeclare args

        Just "printf" -> regularExpansionWithStatus vars args $ handlePrintf args
        Just "wait" -> regularExpansionWithStatus vars args $ handleWait args

        Just "mapfile" -> regularExpansionWithStatus vars args $ handleMapfile args
        Just "readarray" -> regularExpansionWithStatus vars args $ handleMapfile args

        Just "read" -> regularExpansionWithStatus vars args $ handleRead args

        Just "DEFINE_boolean" -> regularExpansionWithStatus vars args $ handleDEFINE args
        Just "DEFINE_float" ->   regularExpansionWithStatus vars args $ handleDEFINE args
        Just "DEFINE_integer" -> regularExpansionWithStatus vars args $ handleDEFINE args
        Just "DEFINE_string" ->  regularExpansionWithStatus vars args $ handleDEFINE args

        -- This will mostly behave like 'command' but ok
        Just "builtin" ->
            case args of
                _ NE.:| [] -> regular
                (_ NE.:| newcmd:newargs) ->
                    handleCommand newcmd vars (newcmd NE.:| newargs) $ getLiteralString newcmd
        Just "command" ->
            case args of
                _ NE.:| [] -> regular
                (_ NE.:| newcmd:newargs) ->
                    handleOthers (getId newcmd) vars (newcmd NE.:| newargs) $ getLiteralString newcmd
        _ -> regular

  where
    regular = handleOthers (getId cmd) vars args literalCmd
    handleExit = do
        exitNode <- reader cfExitTarget
        case exitNode of
            Just target -> do
                exit <- newNode CFResolvedExit
                link exit target CFEExit
                unreachable <- newNode CFUnreachable
                return $ Range exit unreachable
            Nothing -> do
                exit <- newNode CFUnresolvedExit
                unreachable <- newNode CFUnreachable
                return $ Range exit unreachable

    handleReturn = do
        returnTarget <- reader cfReturnTarget
        case returnTarget of
            Nothing -> error $ pleaseReport "missing return target"
            Just target -> do
                ret <- newNode CFStructuralNode
                link ret target CFEFlow
                unreachable <- newNode CFUnreachable
                return $ Range ret unreachable

    handleUnset (cmd NE.:| args) = do
        case () of
                _ | "n" `elem` flagNames -> unsetWith CFUndefineNameref
                _ | "v" `elem` flagNames -> unsetWith CFUndefineVariable
                _ | "f" `elem` flagNames -> unsetWith CFUndefineFunction
                _ -> unsetWith CFUndefine
      where
        pairs :: [(String, Token)] -- [(Flag string, token)] e.g. [("-f", t), ("", myfunc)]
        pairs = map (\(str, (flag, val)) -> (str, flag)) $ fromMaybe (map (\c -> ("", (c,c))) args) $ getGnuOpts "vfn" args
        (names, flags) = partition (null . fst) pairs
        flagNames = map fst flags
        literalNames :: [(Token, String)] -- Literal names to unset, e.g. [(myfuncToken, "myfunc")]
        literalNames = mapMaybe (\(_, t) -> (,) t <$> getLiteralString t) names
        -- Apply a constructor like CFUndefineVariable to each literalName, and tag with its id
        unsetWith c = newNodeRange $ CFApplyEffects $ map (\(token, name) -> IdTagged (getId token) $ c name) literalNames


    variableAssignRegex = mkRegex "^([_a-zA-Z][_a-zA-Z0-9]*)="

    handleDeclare (cmd NE.:| args) = do
        isFunc <- asks cfIsFunction
        -- This is a bit of a kludge: we don't have great support for things like
        -- 'declare -i x=$x' so do one round with declare x=$x, followed by declare -i x
        let (evaluated, assignments, added, removed) = mconcat $ map (toEffects isFunc) args
        before <- sequentially $ evaluated
        assignments <- newNodeRange $ CFApplyEffects assignments
        addedProps <- if null added then newStructuralNode else newNodeRange $ CFApplyEffects added
        removedProps <- if null removed then newStructuralNode else newNodeRange $ CFApplyEffects removed
        result <- newNodeRange $ CFSetExitCode (getId cmd)
        linkRanges [before, assignments, addedProps, removedProps, result]
      where
        opts = map fst $ getGenericOpts args
        array = "a" `elem` opts || associative
        associative = "A" `elem` opts
        integer = "i" `elem` opts
        func = "f" `elem` opts || "F" `elem` opts
        global = "g" `elem` opts
        export = "x" `elem` opts
        writer isFunc =
            case () of
                _ | global -> CFWriteGlobal
                _ | isFunc -> CFWriteLocal
                _ -> CFWriteVariable

        scope isFunc =
            case () of
                _ | global -> Just GlobalScope
                _ | isFunc -> Just LocalScope
                _ -> Nothing

        addedProps = S.fromList $ concat $ [
            [ CFVPArray | array ],
            [ CFVPInteger | integer ],
            [ CFVPExport | export ],
            [ CFVPAssociative | associative ]
          ]

        removedProps = S.fromList $ concat $ [
            -- Array property can't be unset
            [ CFVPInteger | 'i' `elem` unsetOptions ],
            [ CFVPExport | 'e' `elem` unsetOptions ]
          ]

        toEffects isFunc (T_Assignment id mode var idx t) =
            let
                pre = idx ++ [t]
                val = [ IdTagged id $ (writer isFunc) var $ CFValueComputed (getId t) $ [ CFStringVariable var | mode == Append ] ++ tokenToParts t ]
                added = [ IdTagged id $ CFSetProps (scope isFunc) var addedProps | not $ S.null addedProps ]
                removed = [ IdTagged id $ CFUnsetProps (scope isFunc) var addedProps | not $ S.null removedProps ]
            in
                (pre, val, added, removed)

        toEffects isFunc t =
            let
                id = getId t
                pre = [t]
                literal = getLiteralStringDef "\0" t
                isKnown = '\0' `notElem` literal
                match = fmap head $ variableAssignRegex `matchRegex` literal
                name = fromMaybe literal match

                asLiteral =
                    IdTagged id $ (writer isFunc) name $
                        CFValueComputed (getId t) [ CFStringLiteral $ drop 1 $ dropWhile (/= '=') $ literal ]
                asUnknown =
                    IdTagged id $ (writer isFunc) name $
                        CFValueString

                added = [ IdTagged id $ CFSetProps (scope isFunc) name addedProps ]
                removed = [ IdTagged id $ CFUnsetProps (scope isFunc) name removedProps ]

            in
                case () of
                    _ | not (isVariableName name) -> (pre, [], [], [])
                    _ | isJust match && isKnown -> (pre, [asLiteral], added, removed)
                    _ | isJust match -> (pre, [asUnknown], added, removed)
                    -- e.g. declare -i x
                    _ -> (pre, [], added, removed)

        -- find "ia" from `define +i +a`
        unsetOptions :: String
        unsetOptions =
            let
                strings = mapMaybe getLiteralString args
                plusses = filter ("+" `isPrefixOf`) strings
            in
                concatMap (drop 1) plusses

    handlePrintf (cmd NE.:| args) =
        newNodeRange $ CFApplyEffects $ maybeToList findVar
      where
        findVar = do
            flags <- getBsdOpts "v:" args
            (flag, arg) <- lookup "v" flags
            name <- getLiteralString arg
            return $ IdTagged (getId arg) $ CFWriteVariable name CFValueString

    handleWait (cmd NE.:| args) =
        newNodeRange $ CFApplyEffects $ maybeToList findVar
      where
        findVar = do
            let flags = getGenericOpts args
            (flag, arg) <- lookup "p" flags
            name <- getLiteralString arg
            return $ IdTagged (getId arg) $ CFWriteVariable name CFValueInteger

    handleMapfile (cmd NE.:| args) =
        newNodeRange $ CFApplyEffects [findVar]
      where
        findVar =
            let (id, name) = fromMaybe (getId cmd, "MAPFILE") $ getFromArg `mplus` getFromFallback
            in IdTagged id $ CFWriteVariable name CFValueArray

        getFromArg = do
            flags <- getGnuOpts flagsForMapfile args
            (_, arg) <- lookup "" flags
            name <- getLiteralString arg
            return (getId arg, name)

        getFromFallback =
            listToMaybe $ mapMaybe getIfVar $ reverse args
        getIfVar c = do
            name <- getLiteralString c
            guard $ isVariableName name
            return (getId c, name)

    handleRead (cmd NE.:| args) = newNodeRange $ CFApplyEffects main
      where
        main = fromMaybe fallback $ do
            flags <- getGnuOpts flagsForRead args
            return $ fromMaybe (withFields flags) $ withArray flags

        withArray :: [(String, (Token, Token))] -> Maybe [IdTagged CFEffect]
        withArray flags = do
            (_, token) <- lookup "a" flags
            return $ fromMaybe [] $ do
                name <- getLiteralString token
                return [ IdTagged (getId token) $ CFWriteVariable name CFValueArray ]

        withFields flags = mapMaybe getAssignment flags

        getAssignment :: (String, (Token, Token)) -> Maybe (IdTagged CFEffect)
        getAssignment f = do
            ("", (t, _)) <- return f
            name <- getLiteralString t
            return $ IdTagged (getId t) $ CFWriteVariable name CFValueString

        fallback =
            let
                names = reverse $ map fromJust $ takeWhile isJust $ map (\c -> sequence (getId c, getLiteralString c)) $ reverse args
                namesOrDefault = if null names then [(getId cmd, "REPLY")] else names
                hasDashA = any (== "a") $ map fst $ getGenericOpts args
                value = if hasDashA then CFValueArray else CFValueString
            in
                map (\(id, name) -> IdTagged id $ CFWriteVariable name value) namesOrDefault

    handleDEFINE (cmd NE.:| args) =
        newNodeRange $ CFApplyEffects $ maybeToList findVar
      where
        findVar = do
            name <- listToMaybe $ drop 1 args
            str <- getLiteralString name
            guard $ isVariableName str
            return $ IdTagged (getId name) $ CFWriteVariable str CFValueString

    handleOthers id vars args cmd =
        regularExpansion vars (NE.toList args) $ do
            exe <- newNodeRange $ CFExecuteCommand cmd
            status <- newNodeRange $ CFSetExitCode id
            linkRange exe status

    regularExpansion vars args p = do
            args <- sequentially args
            assignments <- mapM (buildAssignment (Just PrefixScope)) vars
            exe <- p
            dropAssignments <-
                if null vars
                then
                    return []
                else do
                    drop <- newNodeRange CFDropPrefixAssignments
                    return [drop]

            linkRanges $ [args] ++ assignments ++ [exe] ++ dropAssignments

    regularExpansionWithStatus vars args@(cmd NE.:| _) p = do
        initial <- regularExpansion vars (NE.toList args) p
        status <- newNodeRange $ CFSetExitCode (getId cmd)
        linkRange initial status


none = newStructuralNode

data Scope = GlobalScope | LocalScope | PrefixScope
  deriving (Eq, Ord, Show, Generic, NFData)

buildAssignment scope t = do
    op <- case t of
            T_Assignment id mode var indices value -> do
                expand <- build value
                index <- sequentially indices
                read <- case mode of
                    Append -> newNodeRange (applySingle $ IdTagged id $ CFReadVariable var)
                    Assign -> none
                let valueType = if null indices then f id value else CFValueArray
                let scoper =
                                case scope of
                                    Just PrefixScope -> CFWritePrefix
                                    Just LocalScope -> CFWriteLocal
                                    Just GlobalScope -> CFWriteGlobal
                                    Nothing -> CFWriteVariable
                write <- newNodeRange $ applySingle $ IdTagged id $ scoper var valueType
                linkRanges [expand, index, read, write]
              where
                f :: Id -> Token -> CFValue
                f id t@T_NormalWord {} = CFValueComputed id $ [CFStringVariable var | mode == Append] ++ tokenToParts t
                f id t@(T_Literal _ str) = CFValueComputed id $ [CFStringVariable var | mode == Append] ++ tokenToParts t
                f _ T_Array {} = CFValueArray

    registerNode (getId t) op
    return op


tokenToParts t =
    case t of
        T_NormalWord _ list -> concatMap tokenToParts list
        T_DoubleQuoted _ list -> concatMap tokenToParts list
        T_SingleQuoted _ str -> [ CFStringLiteral str ]
        T_Literal _ str -> [ CFStringLiteral str ]
        T_DollarArithmetic {} -> [ CFStringInteger ]
        T_DollarBracket {} -> [ CFStringInteger ]
        T_DollarBraced _ _ list | isUnmodifiedParameterExpansion t -> [ CFStringVariable (getBracedReference $ concat $ oversimplify list) ]
        -- Check if getLiteralString can handle it, if not it's unknown
        _ -> [maybe CFStringUnknown CFStringLiteral $ getLiteralString t]


-- Like & but well defined when the node already exists
safeUpdate ctx@(_,node,_,_) graph = ctx & (delNode node graph)

-- Change all subshell invocations to instead link directly to their contents.
-- This is used for producing dominator trees.
inlineSubshells :: CFGraph -> CFGraph
inlineSubshells graph = relinkedGraph
  where
    subshells = ufold find [] graph
    find (incoming, node, label, outgoing) acc =
        case label of
            CFExecuteSubshell _ start end -> (node, label, start, end, incoming, outgoing):acc
            _ -> acc

    relinkedGraph = foldl' relink graph subshells
    relink graph (node, label, start, end, incoming, outgoing) =
        let
            -- Link CFExecuteSubshell to the CFEntryPoint
            subshellToStart = (incoming, node, label, [(CFEFlow, start)])
            -- Link the subshell exit to the
            endToNexts = (endIncoming, endNode, endLabel, outgoing)
            (endIncoming, endNode, endLabel, _) = context graph end
        in
            subshellToStart `safeUpdate` (endToNexts `safeUpdate` graph)

findEntryNodes :: CFGraph -> [Node]
findEntryNodes graph = ufold find [] graph
  where
    find (incoming, node, label, _) list =
        case label of
            CFEntryPoint {} | null incoming -> node:list
            _ -> list

findDominators main graph = asSetMap
  where
    inlined = inlineSubshells graph
    entryNodes = main : findEntryNodes graph
    asLists = concatMap (dom inlined) entryNodes
    asSetMap = M.fromList $ map (\(node, list) -> (node, S.fromList list)) asLists

findTerminalNodes :: CFGraph -> [Node]
findTerminalNodes graph = ufold find [] graph
  where
    find (_, node, label, _) list =
        case label of
            CFUnresolvedExit -> node:list
            CFApplyEffects effects -> f effects list
            _ -> list

    f [] list = list
    f (IdTagged _ (CFDefineFunction _ id start end):rest) list = f rest (end:list)
    f (_:rest) list = f rest list

findPostDominators :: Node -> CFGraph -> Array Node [Node]
findPostDominators mainexit graph = asArray
  where
    inlined = inlineSubshells graph
    terminals = findTerminalNodes inlined
    (incoming, _, label, outgoing) = context graph mainexit
    withExitEdges = (incoming ++ map (\c -> (CFEFlow, c)) terminals, mainexit, label, outgoing) `safeUpdate` inlined
    reversed = grev withExitEdges
    postDoms = dom reversed mainexit
    (_, maxNode) = nodeRange graph
    -- Holes in the array cause "Exception: (Array.!): undefined array element" while
    -- inspecting/debugging, so fill the array first and then update.
    initializedArray = listArray (0, maxNode) $ repeat []
    asArray = initializedArray // postDoms

return []
runTests =  $( [| $(forAllProperties) (quickCheckWithResult (stdArgs { maxSuccess = 1 }) ) |])
