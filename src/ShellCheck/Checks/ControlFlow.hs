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

-- Checks that run on the Control Flow Graph (as opposed to the AST)
-- This is scaffolding for a work in progress.

module ShellCheck.Checks.ControlFlow (checker, optionalChecks, ShellCheck.Checks.ControlFlow.runTests) where

import ShellCheck.AST
import ShellCheck.ASTLib
import ShellCheck.CFG hiding (cfgAnalysis)
import ShellCheck.CFGAnalysis
import ShellCheck.AnalyzerLib
import ShellCheck.Data
import ShellCheck.Interface

import Control.Monad
import Control.Monad.Reader
import Data.Graph.Inductive.Graph
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Maybe

import Test.QuickCheck.All (forAllProperties)
import Test.QuickCheck.Test (quickCheckWithResult, stdArgs, maxSuccess)


optionalChecks :: [CheckDescription]
optionalChecks = []

-- A check that runs on the entire graph
type ControlFlowCheck = Analysis
-- A check invoked once per node, with its (pre,post) data
type ControlFlowNodeCheck = LNode CFNode -> (ProgramState, ProgramState) -> Analysis
-- A check invoked once per effect, with its node's (pre,post) data
type ControlFlowEffectCheck = IdTagged CFEffect -> Node -> (ProgramState, ProgramState) -> Analysis


checker :: AnalysisSpec -> Parameters -> Checker
checker spec params = Checker {
    perScript = const $ sequence_ controlFlowChecks,
    perToken = const $ return ()
}

controlFlowChecks :: [ControlFlowCheck]
controlFlowChecks = [
        runNodeChecks controlFlowNodeChecks
    ]

controlFlowNodeChecks :: [ControlFlowNodeCheck]
controlFlowNodeChecks = [
    runEffectChecks controlFlowEffectChecks
    ]

controlFlowEffectChecks :: [ControlFlowEffectCheck]
controlFlowEffectChecks = [
    ]

runNodeChecks :: [ControlFlowNodeCheck] -> ControlFlowCheck
runNodeChecks perNode = do
    cfg <- asks cfgAnalysis
    runOnAll cfg
  where
    getData datas n@(node, label) = do
        (pre, post) <- M.lookup node datas
        return (n, (pre, post))

    runOn :: (LNode CFNode, (ProgramState, ProgramState)) -> Analysis
    runOn (node, prepost) = mapM_ (\c -> c node prepost) perNode
    runOnAll cfg = mapM_ runOn $ mapMaybe (getData $ nodeToData cfg) $ labNodes (graph cfg)

runEffectChecks :: [ControlFlowEffectCheck] -> ControlFlowNodeCheck
runEffectChecks list = checkNode
  where
    checkNode (node, label) prepost =
        case label of
            CFApplyEffects effects -> mapM_ (\effect -> mapM_ (\c -> c effect node prepost) list) effects
            _ -> return ()


return []
runTests =  $( [| $(forAllProperties) (quickCheckWithResult (stdArgs { maxSuccess = 1 }) ) |])
