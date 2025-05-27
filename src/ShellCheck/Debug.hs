{-

This file contains useful functions for debugging and developing ShellCheck.

To invoke them interactively, run:

    cabal repl

At the ghci prompt, enter:

    :load ShellCheck.Debug

You can now invoke the functions. Here are some examples:

    shellcheckString "echo $1"
    stringToAst "(( x+1 ))"
    stringToCfg "if foo; then bar; else baz; fi"
    writeFile "/tmp/test.dot" $ stringToCfgViz "while foo; do bar; done"

The latter file can be rendered to png with GraphViz:

    dot -Tpng /tmp/test.dot > /tmp/test.png

To run all unit tests in a module:

    ShellCheck.Parser.runTests
    ShellCheck.Analytics.runTests

To run a specific test:

    :load ShellCheck.Analytics
    prop_checkUuoc3

If you make code changes, reload in seconds at any time with:

    :r

===========================================================================

Crash course in printf debugging in Haskell:

    import Debug.Trace

    greet 0 = return ()
    -- Print when a function is invoked
    greet n | trace ("calling greet " ++ show n) False = undefined
    greet n = do
        putStrLn "Enter name"
        name <- getLine
        -- Print at some point in any monadic function
        traceM $ "user entered " ++ name
        putStrLn $ "Hello " ++ name
        -- Print a value before passing it on
        greet $ traceShowId (n - 1)


===========================================================================

If you want to invoke `ghci` directly, such as on `shellcheck.hs`, to
debug all of ShellCheck including I/O, you may see an error like this:

    src/ShellCheck/Data.hs:5:1: error:
        Could not load module ‘Paths_ShellCheck’
    it is a hidden module in the package ‘ShellCheck-0.8.0’

This can easily be circumvented by running `./setgitversion` or manually
editing src/ShellCheck/Data.hs to replace the auto-deduced version number
with a constant string as indicated.

Afterwards, you can run the ShellCheck tool, as if from the shell, with:

    $ ghci shellcheck.hs
    ghci> runMain ["-x", "file.sh"]

-}

module ShellCheck.Debug () where

import ShellCheck.Analyzer
import ShellCheck.AST
import ShellCheck.CFG
import ShellCheck.Checker
import ShellCheck.CFGAnalysis as CF
import ShellCheck.Interface
import ShellCheck.Parser
import ShellCheck.Prelude

import Control.Monad
import Control.Monad.Identity
import Control.Monad.RWS
import Control.Monad.Writer
import Data.Graph.Inductive.Graph as G
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S


-- Run all of ShellCheck (minus output formatters)
shellcheckString :: String -> CheckResult
shellcheckString scriptString =
    runIdentity $ checkScript dummySystemInterface checkSpec
  where
    checkSpec :: CheckSpec
    checkSpec = emptyCheckSpec {
        csScript = scriptString
    }

dummySystemInterface :: SystemInterface Identity
dummySystemInterface = mockedSystemInterface [
    -- A tiny, fake filesystem for sourced files
    ("lib/mylib1.sh", "foo=$(cat $1 | wc -l)"),
    ("lib/mylib2.sh", "bar=42")
    ]

-- Parameters used when generating Control Flow Graphs
cfgParams :: CFGParameters
cfgParams = CFGParameters {
    cfLastpipe = False,
    cfPipefail = False
}

-- An example script to play with
exampleScript :: String
exampleScript = unlines [
    "#!/bin/sh",
    "count=0",
    "for file in *",
    "do",
    "  (( count++ ))",
    "done",
    "echo $count"
    ]

-- Parse the script string into ShellCheck's ParseResult
parseScriptString :: String -> ParseResult
parseScriptString scriptString =
    runIdentity $ parseScript dummySystemInterface parseSpec
  where
    parseSpec :: ParseSpec
    parseSpec = newParseSpec {
        psFilename = "myscript",
        psScript = scriptString
    }


-- Parse the script string into an Abstract Syntax Tree
stringToAst :: String -> Token
stringToAst scriptString =
    case maybeRoot of
        Just root -> root
        Nothing -> error $ "Script failed to parse: " ++ show parserWarnings
  where
    parseResult :: ParseResult
    parseResult = parseScriptString scriptString

    maybeRoot :: Maybe Token
    maybeRoot = prRoot parseResult

    parserWarnings :: [PositionedComment]
    parserWarnings = prComments parseResult


astToCfgResult :: Token -> CFGResult
astToCfgResult = buildGraph cfgParams

astToDfa :: Token -> CFGAnalysis
astToDfa = analyzeControlFlow cfgParams

astToCfg :: Token -> CFGraph
astToCfg = cfGraph . astToCfgResult

stringToCfg :: String -> CFGraph
stringToCfg = astToCfg . stringToAst

stringToDfa :: String -> CFGAnalysis
stringToDfa = astToDfa . stringToAst

cfgToGraphViz :: CFGraph -> String
cfgToGraphViz = cfgToGraphVizWith show

stringToCfgViz :: String -> String
stringToCfgViz = cfgToGraphViz . stringToCfg

stringToDfaViz :: String -> String
stringToDfaViz = dfaToGraphViz . stringToDfa

-- Dump a Control Flow Graph as GraphViz with extended information
stringToDetailedCfgViz :: String -> String
stringToDetailedCfgViz scriptString = cfgToGraphVizWith nodeLabel graph
  where
    ast :: Token
    ast = stringToAst scriptString

    cfgResult :: CFGResult
    cfgResult = astToCfgResult ast

    graph :: CFGraph
    graph = cfGraph cfgResult

    idToToken :: M.Map Id Token
    idToToken = M.fromList $ execWriter $ doAnalysis (\c -> tell [(getId c, c)]) ast

    idToNode :: M.Map Id (Node, Node)
    idToNode = cfIdToRange cfgResult

    nodeToStartIds :: M.Map Node (S.Set Id)
    nodeToStartIds =
        M.fromListWith S.union $
            map (\(id, (start, _)) -> (start, S.singleton id)) $
                M.toList idToNode

    nodeToEndIds :: M.Map Node (S.Set Id)
    nodeToEndIds =
        M.fromListWith S.union $
            map (\(id, (_, end)) -> (end, S.singleton id)) $
                M.toList idToNode

    formatId :: Id -> String
    formatId id = fromMaybe ("Unknown " ++ show id) $ do
        (OuterToken _ token) <- M.lookup id idToToken
        firstWord <- words (show token) !!! 0
        -- Strip off "Inner_"
        (_ : tokenName) <- return $ dropWhile (/= '_') firstWord
        return $ tokenName ++ " " ++ show id

    formatGroup :: S.Set Id -> String
    formatGroup set = intercalate ", " $ map formatId $ S.toList set

    nodeLabel (node, label) = unlines [
        show node ++ ". " ++ show label,
        "Begin: " ++ formatGroup (M.findWithDefault S.empty node nodeToStartIds),
        "End: " ++ formatGroup (M.findWithDefault S.empty node nodeToEndIds)
        ]


-- Dump a Control Flow Graph with Data Flow Analysis as GraphViz
dfaToGraphViz :: CF.CFGAnalysis -> String
dfaToGraphViz analysis = cfgToGraphVizWith label $ CF.graph analysis
  where
    label (node, label) =
        let
            desc = show node ++ ". " ++ show label
        in
            fromMaybe ("No DFA available\n\n" ++ desc) $ do
                (pre, post) <- M.lookup node $ CF.nodeToData analysis
                return $ unlines [
                    "Precondition: " ++ show pre,
                    "",
                    desc,
                    "",
                    "Postcondition: " ++ show post
                    ]


-- Dump an Control Flow Graph to GraphViz with a given node formatter
cfgToGraphVizWith :: (LNode CFNode -> String) -> CFGraph -> String
cfgToGraphVizWith nodeLabel graph = concat [
    "digraph {\n",
    concatMap dumpNode (labNodes graph),
    concatMap dumpLink (labEdges graph),
    tagVizEntries graph,
    "}\n"
    ]
  where
    dumpNode l@(node, label) = show node ++ " [label=" ++ quoteViz (nodeLabel l) ++ "]\n"
    dumpLink (from, to, typ) = show from ++ " -> " ++ show to ++ " [style=" ++ quoteViz (edgeStyle typ)  ++ "]\n"
    edgeStyle CFEFlow = "solid"
    edgeStyle CFEExit = "bold"
    edgeStyle CFEFalseFlow = "dotted"

quoteViz str = "\"" ++ escapeViz str ++ "\""
escapeViz [] = []
escapeViz (c:rest) =
    case c of
        '\"' -> '\\' : '\"' : escapeViz rest
        '\n' -> '\\' : 'l' : escapeViz rest
        '\\' -> '\\' : '\\' : escapeViz rest
        _ -> c : escapeViz rest


-- Dump an Abstract Syntax Tree (or branch thereof) to GraphViz format
astToGraphViz :: Token -> String
astToGraphViz token = concat [
    "digraph {\n",
    formatTree token,
    "}\n"
    ]
  where
    formatTree :: Token -> String
    formatTree t = snd $ execRWS (doStackAnalysis push pop t) () []

    push :: Token -> RWS () String [Int] ()
    push (OuterToken (Id n) inner) = do
        stack <- get
        put (n : stack)
        case stack of
            [] -> return ()
            (top:_) -> tell $ show top ++ " -> " ++ show n ++ "\n"
        tell $ show n ++ " [label=" ++ quoteViz (show n ++ ": " ++ take 32 (show inner)) ++ "]\n"

    pop :: Token -> RWS () String [Int] ()
    pop _ = modify tail


-- For each entry point, set the rank so that they'll align in the graph
tagVizEntries :: CFGraph -> String
tagVizEntries graph = "{ rank=same " ++ rank ++ " }"
  where
    entries = mapMaybe find $ labNodes graph
    find (node, CFEntryPoint name) = return (node, name)
    find _ = Nothing
    rank = unwords $ map (\(c, _) -> show c) entries
