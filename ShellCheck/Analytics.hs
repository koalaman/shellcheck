{-
    Copyright 2012-2015 Vidar Holen

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
{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module ShellCheck.Analytics (runAnalytics, ShellCheck.Analytics.runTests) where

import ShellCheck.AST
import ShellCheck.ASTLib
import ShellCheck.AnalyzerLib hiding (producesComments)
import ShellCheck.Data
import ShellCheck.Parser
import ShellCheck.Interface
import ShellCheck.Regex

import Control.Arrow (first)
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Char
import Data.Functor
import Data.Function (on)
import Data.List
import Data.Maybe
import Data.Ord
import Debug.Trace
import qualified Data.Map.Strict as Map
import Test.QuickCheck.All (forAllProperties)
import Test.QuickCheck.Test (quickCheckWithResult, stdArgs, maxSuccess)

-- Checks that are run on the AST root
treeChecks :: [Parameters -> Token -> [TokenComment]]
treeChecks = [
    runNodeAnalysis
        (\p t -> (mapM_ ((\ f -> f t) . (\ f -> f p))
            nodeChecks))
    ,subshellAssignmentCheck
    ,checkSpacefulness
    ,checkQuotesInLiterals
    ,checkShebangParameters
    ,checkFunctionsUsedExternally
    ,checkUnusedAssignments
    ,checkUnpassedInFunctions
    ,checkArrayWithoutIndex
    ,checkShebang
    ,checkUnassignedReferences
    ,checkUncheckedCdPushdPopd
    ,checkArrayAssignmentIndices
    ,checkUseBeforeDefinition
    ]

runAnalytics :: AnalysisSpec -> [TokenComment]
runAnalytics options =
        runList options treeChecks

runList :: AnalysisSpec -> [Parameters -> Token -> [TokenComment]]
    -> [TokenComment]
runList spec list = notes
    where
        root = asScript spec
        params = makeParameters spec
        notes = concatMap (\f -> f params root) list


checkList l t = concatMap (\f -> f t) l


-- Checks that are run on each node in the AST
runNodeAnalysis f p t = execWriter (doAnalysis (f p) t)

nodeChecks :: [Parameters -> Token -> Writer [TokenComment] ()]
nodeChecks = [
    checkUuoc
    ,checkPipePitfalls
    ,checkForInQuoted
    ,checkForInLs
    ,checkShorthandIf
    ,checkDollarStar
    ,checkUnquotedDollarAt
    ,checkStderrRedirect
    ,checkUnquotedN
    ,checkNumberComparisons
    ,checkSingleBracketOperators
    ,checkDoubleBracketOperators
    ,checkLiteralBreakingTest
    ,checkConstantNullary
    ,checkDivBeforeMult
    ,checkArithmeticDeref
    ,checkArithmeticBadOctal
    ,checkComparisonAgainstGlob
    ,checkCommarrays
    ,checkOrNeq
    ,checkEchoWc
    ,checkConstantIfs
    ,checkPipedAssignment
    ,checkAssignAteCommand
    ,checkUuoeVar
    ,checkQuotedCondRegex
    ,checkForInCat
    ,checkFindExec
    ,checkValidCondOps
    ,checkGlobbedRegex
    ,checkTestRedirects
    ,checkIndirectExpansion
    ,checkSudoRedirect
    ,checkPS1Assignments
    ,checkBackticks
    ,checkInexplicablyUnquoted
    ,checkTildeInQuotes
    ,checkLonelyDotDash
    ,checkSpuriousExec
    ,checkSpuriousExpansion
    ,checkDollarBrackets
    ,checkSshHereDoc
    ,checkGlobsAsOptions
    ,checkWhileReadPitfalls
    ,checkArithmeticOpCommand
    ,checkCharRangeGlob
    ,checkUnquotedExpansions
    ,checkSingleQuotedVariables
    ,checkRedirectToSame
    ,checkPrefixAssignmentReference
    ,checkLoopKeywordScope
    ,checkCdAndBack
    ,checkWrongArithmeticAssignment
    ,checkConditionalAndOrs
    ,checkFunctionDeclarations
    ,checkStderrPipe
    ,checkOverridingPath
    ,checkArrayAsString
    ,checkUnsupported
    ,checkMultipleAppends
    ,checkSuspiciousIFS
    ,checkShouldUseGrepQ
    ,checkTestArgumentSplitting
    ,checkConcatenatedDollarAt
    ,checkTildeInPath
    ,checkMaskedReturns
    ,checkReadWithoutR
    ,checkLoopVariableReassignment
    ,checkTrailingBracket
    ,checkReturnAgainstZero
    ,checkRedirectedNowhere
    ,checkUnmatchableCases
    ,checkSubshellAsTest
    ,checkSplittingInArrays
    ,checkRedirectionToNumber
    ,checkGlobAsCommand
    ,checkFlagAsCommand
    ,checkEmptyCondition
    ,checkPipeToNowhere
    ]


wouldHaveBeenGlob s = '*' `elem` s

verify :: (Parameters -> Token -> Writer [TokenComment] ()) -> String -> Bool
verify f s = checkNode f s == Just True

verifyNot :: (Parameters -> Token -> Writer [TokenComment] ()) -> String -> Bool
verifyNot f s = checkNode f s == Just False

verifyTree :: (Parameters -> Token -> [TokenComment]) -> String -> Bool
verifyTree f s = producesComments f s == Just True

verifyNotTree :: (Parameters -> Token -> [TokenComment]) -> String -> Bool
verifyNotTree f s = producesComments f s == Just False

checkCommand str f t@(T_SimpleCommand id _ (cmd:rest)) =
    when (t `isCommand` str) $ f cmd rest
checkCommand _ _ _ = return ()

checkUnqualifiedCommand str f t@(T_SimpleCommand id _ (cmd:rest)) =
    when (t `isUnqualifiedCommand` str) $ f cmd rest
checkUnqualifiedCommand _ _ _ = return ()


checkNode f = producesComments (runNodeAnalysis f)
producesComments :: (Parameters -> Token -> [TokenComment]) -> String -> Maybe Bool
producesComments f s = do
        root <- pScript s
        return . not . null $ runList (defaultSpec root) [f]

-- Copied from https://wiki.haskell.org/Edit_distance
dist :: Eq a => [a] -> [a] -> Int
dist a b
    = last (if lab == 0 then mainDiag
            else if lab > 0 then lowers !! (lab - 1)
                 else{- < 0 -}   uppers !! (-1 - lab))
    where mainDiag = oneDiag a b (head uppers) (-1 : head lowers)
          uppers = eachDiag a b (mainDiag : uppers) -- upper diagonals
          lowers = eachDiag b a (mainDiag : lowers) -- lower diagonals
          eachDiag a [] diags = []
          eachDiag a (bch:bs) (lastDiag:diags) = oneDiag a bs nextDiag lastDiag : eachDiag a bs diags
              where nextDiag = head (tail diags)
          oneDiag a b diagAbove diagBelow = thisdiag
              where doDiag [] b nw n w = []
                    doDiag a [] nw n w = []
                    doDiag (ach:as) (bch:bs) nw n w = me : doDiag as bs me (tail n) (tail w)
                        where me = if ach == bch then nw else 1 + min3 (head w) nw (head n)
                    firstelt = 1 + head diagBelow
                    thisdiag = firstelt : doDiag a b firstelt diagAbove (tail diagBelow)
          lab = length a - length b
          min3 x y z = if x < y then x else min y z

hasFloatingPoint params = shellType params == Ksh

-- Checks whether the current parent path is part of a condition
isCondition [] = False
isCondition [_] = False
isCondition (child:parent:rest) =
    getId child `elem` map getId (getConditionChildren parent) || isCondition (parent:rest)
  where
    getConditionChildren t =
        case t of
            T_AndIf _ left right -> [left]
            T_OrIf id left right -> [left]
            T_IfExpression id conditions elses -> concatMap (take 1 . reverse . fst) conditions
            T_WhileExpression id c l -> take 1 . reverse $ c
            T_UntilExpression id c l -> take 1 . reverse $ c
            _ -> []

prop_checkEchoWc3 = verify checkEchoWc "n=$(echo $foo | wc -c)"
checkEchoWc _ (T_Pipeline id _ [a, b]) =
    when (acmd == ["echo", "${VAR}"]) $
        case bcmd of
            ["wc", "-c"] -> countMsg
            ["wc", "-m"] -> countMsg
            _ -> return ()
  where
    acmd = oversimplify a
    bcmd = oversimplify b
    countMsg = style id 2000 "See if you can use ${#variable} instead."
checkEchoWc _ _ = return ()

prop_checkPipedAssignment1 = verify checkPipedAssignment "A=ls | grep foo"
prop_checkPipedAssignment2 = verifyNot checkPipedAssignment "A=foo cmd | grep foo"
prop_checkPipedAssignment3 = verifyNot checkPipedAssignment "A=foo"
checkPipedAssignment _ (T_Pipeline _ _ (T_Redirecting _ _ (T_SimpleCommand id (_:_) []):_:_)) =
    warn id 2036 "If you wanted to assign the output of the pipeline, use a=$(b | c) ."
checkPipedAssignment _ _ = return ()

prop_checkAssignAteCommand1 = verify checkAssignAteCommand "A=ls -l"
prop_checkAssignAteCommand2 = verify checkAssignAteCommand "A=ls --sort=$foo"
prop_checkAssignAteCommand3 = verify checkAssignAteCommand "A=cat foo | grep bar"
prop_checkAssignAteCommand4 = verifyNot checkAssignAteCommand "A=foo ls -l"
prop_checkAssignAteCommand5 = verify checkAssignAteCommand "PAGER=cat grep bar"
prop_checkAssignAteCommand6 = verifyNot checkAssignAteCommand "PAGER=\"cat\" grep bar"
prop_checkAssignAteCommand7 = verify checkAssignAteCommand "here=pwd"
checkAssignAteCommand _ (T_SimpleCommand id (T_Assignment _ _ _ _ assignmentTerm:[]) list) =
    -- Check if first word is intended as an argument (flag or glob).
    if firstWordIsArg list
    then
        err id 2037 "To assign the output of a command, use var=$(cmd) ."
    else
        -- Check if it's a known, unquoted command name.
        when (isCommonCommand $ getUnquotedLiteral assignmentTerm) $
            warn id 2209 "Use var=$(command) to assign output (or quote to assign string)."
  where
    isCommonCommand (Just s) = s `elem` commonCommands
    isCommonCommand _ = False
    firstWordIsArg list = fromMaybe False $ do
        head <- list !!! 0
        return $ isGlob head || isUnquotedFlag head

checkAssignAteCommand _ _ = return ()

prop_checkArithmeticOpCommand1 = verify checkArithmeticOpCommand "i=i + 1"
prop_checkArithmeticOpCommand2 = verify checkArithmeticOpCommand "foo=bar * 2"
prop_checkArithmeticOpCommand3 = verifyNot checkArithmeticOpCommand "foo + opts"
checkArithmeticOpCommand _ (T_SimpleCommand id [T_Assignment {}] (firstWord:_)) =
    fromMaybe (return ()) $ check <$> getGlobOrLiteralString firstWord
  where
    check op =
        when (op `elem` ["+", "-", "*", "/"]) $
            warn (getId firstWord) 2099 $
                "Use $((..)) for arithmetics, e.g. i=$((i " ++ op ++ " 2))"
checkArithmeticOpCommand _ _ = return ()

prop_checkWrongArit = verify checkWrongArithmeticAssignment "i=i+1"
prop_checkWrongArit2 = verify checkWrongArithmeticAssignment "n=2; i=n*2"
checkWrongArithmeticAssignment params (T_SimpleCommand id (T_Assignment _ _ _ _ val:[]) []) =
  fromMaybe (return ()) $ do
    str <- getNormalString val
    match <- matchRegex regex str
    var <- match !!! 0
    op <- match !!! 1
    Map.lookup var references
    return . warn (getId val) 2100 $
        "Use $((..)) for arithmetics, e.g. i=$((i " ++ op ++ " 2))"
  where
    regex = mkRegex "^([_a-zA-Z][_a-zA-Z0-9]*)([+*-]).+$"
    references = foldl (flip ($)) Map.empty (map insertRef $ variableFlow params)
    insertRef (Assignment (_, _, name, _)) =
        Map.insert name ()
    insertRef _ = Prelude.id

    getNormalString (T_NormalWord _ words) = do
        parts <- foldl (liftM2 (\x y -> x ++ [y])) (Just []) $ map getLiterals words
        return $ concat parts
    getNormalString _ = Nothing

    getLiterals (T_Literal _ s) = return s
    getLiterals (T_Glob _ s) = return s
    getLiterals _ = Nothing
checkWrongArithmeticAssignment _ _ = return ()


prop_checkUuoc1 = verify checkUuoc "cat foo | grep bar"
prop_checkUuoc2 = verifyNot checkUuoc "cat * | grep bar"
prop_checkUuoc3 = verify checkUuoc "cat $var | grep bar"
prop_checkUuoc4 = verifyNot checkUuoc "cat $var"
prop_checkUuoc5 = verifyNot checkUuoc "cat \"$@\""
prop_checkUuoc6 = verifyNot checkUuoc "cat -n | grep bar"
checkUuoc _ (T_Pipeline _ _ (T_Redirecting _ _ cmd:_:_)) =
    checkCommand "cat" (const f) cmd
  where
    f [word] = unless (mayBecomeMultipleArgs word || isOption word) $
        style (getId word) 2002 "Useless cat. Consider 'cmd < file | ..' or 'cmd file | ..' instead."
    f _ = return ()
    isOption word = "-" `isPrefixOf` onlyLiteralString word
checkUuoc _ _ = return ()

prop_checkPipePitfalls3 = verify checkPipePitfalls "ls | grep -v mp3"
prop_checkPipePitfalls4 = verifyNot checkPipePitfalls "find . -print0 | xargs -0 foo"
prop_checkPipePitfalls5 = verifyNot checkPipePitfalls "ls -N | foo"
prop_checkPipePitfalls6 = verify checkPipePitfalls "find . | xargs foo"
prop_checkPipePitfalls7 = verifyNot checkPipePitfalls "find . -printf '%s\\n' | xargs foo"
prop_checkPipePitfalls8 = verify checkPipePitfalls "foo | grep bar | wc -l"
prop_checkPipePitfalls9 = verifyNot checkPipePitfalls "foo | grep -o bar | wc -l"
prop_checkPipePitfalls10 = verifyNot checkPipePitfalls "foo | grep -o bar | wc"
prop_checkPipePitfalls11 = verifyNot checkPipePitfalls "foo | grep bar | wc"
prop_checkPipePitfalls12 = verifyNot checkPipePitfalls "foo | grep -o bar | wc -c"
prop_checkPipePitfalls13 = verifyNot checkPipePitfalls "foo | grep bar | wc -c"
prop_checkPipePitfalls14 = verifyNot checkPipePitfalls "foo | grep -o bar | wc -cmwL"
prop_checkPipePitfalls15 = verifyNot checkPipePitfalls "foo | grep bar | wc -cmwL"
prop_checkPipePitfalls16 = verifyNot checkPipePitfalls "foo | grep -r bar | wc -l"
checkPipePitfalls _ (T_Pipeline id _ commands) = do
    for ["find", "xargs"] $
        \(find:xargs:_) ->
          let args = oversimplify xargs ++ oversimplify find
          in
            unless (any ($ args) [
                hasShortParameter '0',
                hasParameter "null",
                hasParameter "print0",
                hasParameter "printf"
              ]) $ warn (getId find) 2038
                      "Use -print0/-0 or -exec + to allow for non-alphanumeric filenames."

    for' ["ps", "grep"] $
        \x -> info x 2009 "Consider using pgrep instead of grepping ps output."

    for ["grep", "wc"] $
        \(grep:wc:_) ->
            let flagsGrep = fromMaybe [] $ map snd . getAllFlags <$> getCommand grep
                flagsWc = fromMaybe [] $ map snd . getAllFlags <$> getCommand wc
            in
                unless (any (`elem` ["o", "only-matching", "r", "R", "recursive"]) flagsGrep || any (`elem` ["m", "chars", "w", "words", "c", "bytes", "L", "max-line-length"]) flagsWc || null flagsWc) $
                    style (getId grep) 2126 "Consider using grep -c instead of grep|wc -l."

    didLs <- fmap or . sequence $ [
        for' ["ls", "grep"] $
            \x -> warn x 2010 "Don't use ls | grep. Use a glob or a for loop with a condition to allow non-alphanumeric filenames.",
        for' ["ls", "xargs"] $
            \x -> warn x 2011 "Use 'find .. -print0 | xargs -0 ..' or 'find .. -exec .. +' to allow non-alphanumeric filenames."
        ]
    unless didLs $ do
        for ["ls", "?"] $
            \(ls:_) -> unless (hasShortParameter 'N' (oversimplify ls)) $
                info (getId ls) 2012 "Use find instead of ls to better handle non-alphanumeric filenames."
        return ()
  where
    for l f =
        let indices = indexOfSublists l (map (headOrDefault "" . oversimplify) commands)
        in do
            mapM_ (f . (\ n -> take (length l) $ drop n commands)) indices
            return . not . null $ indices
    for' l f = for l (first f)
    first func (x:_) = func (getId x)
    first _ _ = return ()
    hasShortParameter char = any (\x -> "-" `isPrefixOf` x && char `elem` x)
    hasParameter string =
        any (isPrefixOf string . dropWhile (== '-'))
checkPipePitfalls _ _ = return ()

indexOfSublists sub = f 0
  where
    f _ [] = []
    f n a@(r:rest) =
        let others = f (n+1) rest in
            if match sub a
              then n:others
              else others
    match ("?":r1) (_:r2) = match r1 r2
    match (x1:r1) (x2:r2) | x1 == x2 = match r1 r2
    match [] _ = True
    match _ _ = False


prop_checkShebangParameters1 = verifyTree checkShebangParameters "#!/usr/bin/env bash -x\necho cow"
prop_checkShebangParameters2 = verifyNotTree checkShebangParameters "#! /bin/sh  -l "
checkShebangParameters p (T_Annotation _ _ t) = checkShebangParameters p t
checkShebangParameters _ (T_Script id sb _) =
    [makeComment ErrorC id 2096 "On most OS, shebangs can only specify a single parameter." | length (words sb) > 2]

prop_checkShebang1 = verifyNotTree checkShebang "#!/usr/bin/env bash -x\necho cow"
prop_checkShebang2 = verifyNotTree checkShebang "#! /bin/sh  -l "
prop_checkShebang3 = verifyTree checkShebang "ls -l"
prop_checkShebang4 = verifyNotTree checkShebang "#shellcheck shell=sh\nfoo"
prop_checkShebang5 = verifyTree checkShebang "#!/usr/bin/env ash"
prop_checkShebang6 = verifyNotTree checkShebang "#!/usr/bin/env ash\n# shellcheck shell=dash\n"
prop_checkShebang7 = verifyNotTree checkShebang "#!/usr/bin/env ash\n# shellcheck shell=sh\n"
checkShebang params (T_Annotation _ list t) =
    if any isOverride list then [] else checkShebang params t
  where
    isOverride (ShellOverride _) = True
    isOverride _ = False
checkShebang params (T_Script id sb _) = execWriter $
    unless (shellTypeSpecified params) $ do
        when (sb == "") $
            err id 2148 "Tips depend on target shell and yours is unknown. Add a shebang."
        when (executableFromShebang sb == "ash") $
            warn id 2187 "Ash scripts will be checked as Dash. Add '# shellcheck shell=dash' to silence."


prop_checkForInQuoted = verify checkForInQuoted "for f in \"$(ls)\"; do echo foo; done"
prop_checkForInQuoted2 = verifyNot checkForInQuoted "for f in \"$@\"; do echo foo; done"
prop_checkForInQuoted2a = verifyNot checkForInQuoted "for f in *.mp3; do echo foo; done"
prop_checkForInQuoted2b = verify checkForInQuoted "for f in \"*.mp3\"; do echo foo; done"
prop_checkForInQuoted3 = verify checkForInQuoted "for f in 'find /'; do true; done"
prop_checkForInQuoted4 = verify checkForInQuoted "for f in 1,2,3; do true; done"
prop_checkForInQuoted4a = verifyNot checkForInQuoted "for f in foo{1,2,3}; do true; done"
prop_checkForInQuoted5 = verify checkForInQuoted "for f in ls; do true; done"
prop_checkForInQuoted6 = verifyNot checkForInQuoted "for f in \"${!arr}\"; do true; done"
checkForInQuoted _ (T_ForIn _ f [T_NormalWord _ [word@(T_DoubleQuoted id list)]] _) =
    when (any (\x -> willSplit x && not (mayBecomeMultipleArgs x)) list
            || (fmap wouldHaveBeenGlob (getLiteralString word) == Just True)) $
        err id 2066 "Since you double quoted this, it will not word split, and the loop will only run once."
checkForInQuoted _ (T_ForIn _ f [T_NormalWord _ [T_SingleQuoted id _]] _) =
    warn id 2041 "This is a literal string. To run as a command, use $(..) instead of '..' . "
checkForInQuoted _ (T_ForIn _ f [T_NormalWord _ [T_Literal id s]] _) =
    if ',' `elem` s
      then unless ('{' `elem` s) $
            warn id 2042 "Use spaces, not commas, to separate loop elements."
      else warn id 2043 "This loop will only ever run once for a constant value. Did you perhaps mean to loop over dir/*, $var or $(cmd)?"
checkForInQuoted _ _ = return ()

prop_checkForInCat1 = verify checkForInCat "for f in $(cat foo); do stuff; done"
prop_checkForInCat1a= verify checkForInCat "for f in `cat foo`; do stuff; done"
prop_checkForInCat2 = verify checkForInCat "for f in $(cat foo | grep lol); do stuff; done"
prop_checkForInCat2a= verify checkForInCat "for f in `cat foo | grep lol`; do stuff; done"
prop_checkForInCat3 = verifyNot checkForInCat "for f in $(cat foo | grep bar | wc -l); do stuff; done"
checkForInCat _ (T_ForIn _ f [T_NormalWord _ w] _) = mapM_ checkF w
  where
    checkF (T_DollarExpansion id [T_Pipeline _ _ r])
        | all isLineBased r =
            info id 2013 "To read lines rather than words, pipe/redirect to a 'while read' loop."
    checkF (T_Backticked id cmds) = checkF (T_DollarExpansion id cmds)
    checkF _ = return ()
    isLineBased cmd = any (cmd `isCommand`)
                        ["grep", "fgrep", "egrep", "sed", "cat", "awk", "cut", "sort"]
checkForInCat _ _ = return ()

prop_checkForInLs = verify checkForInLs "for f in $(ls *.mp3); do mplayer \"$f\"; done"
prop_checkForInLs2 = verify checkForInLs "for f in `ls *.mp3`; do mplayer \"$f\"; done"
prop_checkForInLs3 = verify checkForInLs "for f in `find / -name '*.mp3'`; do mplayer \"$f\"; done"
checkForInLs _ = try
  where
   try (T_ForIn _ f [T_NormalWord _ [T_DollarExpansion id [x]]] _) =
        check id f x
   try (T_ForIn _ f [T_NormalWord _ [T_Backticked id [x]]] _) =
        check id f x
   try _ = return ()
   check id f x =
    case oversimplify x of
      ("ls":n) ->
        let warntype = if any ("-" `isPrefixOf`) n then warn else err in
          warntype id 2045 "Iterating over ls output is fragile. Use globs."
      ("find":_) -> warn id 2044 "For loops over find output are fragile. Use find -exec or a while read loop."
      _ -> return ()


prop_checkFindExec1 = verify checkFindExec "find / -name '*.php' -exec rm {};"
prop_checkFindExec2 = verify checkFindExec "find / -exec touch {} && ls {} \\;"
prop_checkFindExec3 = verify checkFindExec "find / -execdir cat {} | grep lol +"
prop_checkFindExec4 = verifyNot checkFindExec "find / -name '*.php' -exec foo {} +"
prop_checkFindExec5 = verifyNot checkFindExec "find / -execdir bash -c 'a && b' \\;"
prop_checkFindExec6 = verify checkFindExec "find / -type d -execdir rm *.jpg \\;"
checkFindExec _ cmd@(T_SimpleCommand _ _ t@(h:r)) | cmd `isCommand` "find" = do
    c <- broken r False
    when c $
        let wordId = getId $ last t in
            err wordId 2067 "Missing ';' or + terminating -exec. You can't use |/||/&&, and ';' has to be a separate, quoted argument."

  where
    broken [] v = return v
    broken (w:r) v = do
        when v (mapM_ warnFor $ fromWord w)
        case getLiteralString w of
            Just "-exec" -> broken r True
            Just "-execdir" -> broken r True
            Just "+" -> broken r False
            Just ";" -> broken r False
            _ -> broken r v

    shouldWarn x =
      case x of
        T_DollarExpansion _ _ -> True
        T_Backticked _ _ -> True
        T_Glob _ _ -> True
        T_Extglob {} -> True
        _ -> False

    warnFor x =
        when(shouldWarn x) $
            info (getId x) 2014 "This will expand once before find runs, not per file found."

    fromWord (T_NormalWord _ l) = l
    fromWord _ = []
checkFindExec _ _ = return ()


prop_checkUnquotedExpansions1 = verify checkUnquotedExpansions "rm $(ls)"
prop_checkUnquotedExpansions1a= verify checkUnquotedExpansions "rm `ls`"
prop_checkUnquotedExpansions2 = verify checkUnquotedExpansions "rm foo$(date)"
prop_checkUnquotedExpansions3 = verify checkUnquotedExpansions "[ $(foo) == cow ]"
prop_checkUnquotedExpansions3a= verify checkUnquotedExpansions "[ ! $(foo) ]"
prop_checkUnquotedExpansions4 = verifyNot checkUnquotedExpansions "[[ $(foo) == cow ]]"
prop_checkUnquotedExpansions5 = verifyNot checkUnquotedExpansions "for f in $(cmd); do echo $f; done"
prop_checkUnquotedExpansions6 = verifyNot checkUnquotedExpansions "$(cmd)"
prop_checkUnquotedExpansions7 = verifyNot checkUnquotedExpansions "cat << foo\n$(ls)\nfoo"
prop_checkUnquotedExpansions8 = verifyNot checkUnquotedExpansions "set -- $(seq 1 4)"
prop_checkUnquotedExpansions9 = verifyNot checkUnquotedExpansions "echo foo `# inline comment`"
checkUnquotedExpansions params =
    check
  where
    check t@(T_DollarExpansion _ c) = examine t c
    check t@(T_Backticked _ c) = examine t c
    check t@(T_DollarBraceCommandExpansion _ c) = examine t c
    check _ = return ()
    tree = parentMap params
    examine t contents =
        unless (null contents || shouldBeSplit t || isQuoteFree tree t || usedAsCommandName tree t) $
            warn (getId t) 2046 "Quote this to prevent word splitting."

    shouldBeSplit t =
        getCommandNameFromExpansion t == Just "seq"


prop_checkRedirectToSame = verify checkRedirectToSame "cat foo > foo"
prop_checkRedirectToSame2 = verify checkRedirectToSame "cat lol | sed -e 's/a/b/g' > lol"
prop_checkRedirectToSame3 = verifyNot checkRedirectToSame "cat lol | sed -e 's/a/b/g' > foo.bar && mv foo.bar lol"
prop_checkRedirectToSame4 = verifyNot checkRedirectToSame "foo /dev/null > /dev/null"
prop_checkRedirectToSame5 = verifyNot checkRedirectToSame "foo > bar 2> bar"
prop_checkRedirectToSame6 = verifyNot checkRedirectToSame "echo foo > foo"
prop_checkRedirectToSame7 = verifyNot checkRedirectToSame "sed 's/foo/bar/g' file | sponge file"
checkRedirectToSame params s@(T_Pipeline _ _ list) =
    mapM_ (\l -> (mapM_ (\x -> doAnalysis (checkOccurrences x) l) (getAllRedirs list))) list
  where
    note x = makeComment InfoC x 2094
                "Make sure not to read and write the same file in the same pipeline."
    checkOccurrences t@(T_NormalWord exceptId x) u@(T_NormalWord newId y) =
        when (exceptId /= newId
                && x == y
                && not (isOutput t && isOutput u)
                && not (special t)
                && not (any isHarmlessCommand [t,u])) $ do
            addComment $ note newId
            addComment $ note exceptId
    checkOccurrences _ _ = return ()
    getAllRedirs = concatMap (\t ->
        case t of
            T_Redirecting _ ls _ -> concatMap getRedirs ls
            _ -> [])
    getRedirs (T_FdRedirect _ _ (T_IoFile _ op file)) =
            case op of T_Greater _ -> [file]
                       T_Less _    -> [file]
                       T_DGREAT _  -> [file]
                       _ -> []
    getRedirs _ = []
    special x = "/dev/" `isPrefixOf` concat (oversimplify x)
    isOutput t =
        case drop 1 $ getPath (parentMap params) t of
            T_IoFile _ op _:_ ->
                case op of
                    T_Greater _  -> True
                    T_DGREAT _ -> True
                    _ -> False
            _ -> False
    isHarmlessCommand arg = fromMaybe False $ do
        cmd <- getClosestCommand (parentMap params) arg
        name <- getCommandBasename cmd
        return $ name `elem` ["echo", "printf", "sponge"]

checkRedirectToSame _ _ = return ()


prop_checkShorthandIf  = verify checkShorthandIf "[[ ! -z file ]] && scp file host || rm file"
prop_checkShorthandIf2 = verifyNot checkShorthandIf "[[ ! -z file ]] && { scp file host || echo 'Eek'; }"
prop_checkShorthandIf3 = verifyNot checkShorthandIf "foo && bar || echo baz"
prop_checkShorthandIf4 = verifyNot checkShorthandIf "foo && a=b || a=c"
prop_checkShorthandIf5 = verifyNot checkShorthandIf "foo && rm || printf b"
prop_checkShorthandIf6 = verifyNot checkShorthandIf "if foo && bar || baz; then true; fi"
prop_checkShorthandIf7 = verifyNot checkShorthandIf "while foo && bar || baz; do true; done"
prop_checkShorthandIf8 = verify checkShorthandIf "if true; then foo && bar || baz; fi"
checkShorthandIf params x@(T_AndIf id _ (T_OrIf _ _ (T_Pipeline _ _ t)))
        | not (isOk t || inCondition) =
    info id 2015 "Note that A && B || C is not if-then-else. C may run when A is true."
  where
    isOk [t] = isAssignment t || fromMaybe False (do
        name <- getCommandBasename t
        return $ name `elem` ["echo", "exit", "return", "printf"])
    isOk _ = False
    inCondition = isCondition $ getPath (parentMap params) x
checkShorthandIf _ _ = return ()


prop_checkDollarStar = verify checkDollarStar "for f in $*; do ..; done"
prop_checkDollarStar2 = verifyNot checkDollarStar "a=$*"
prop_checkDollarStar3 = verifyNot checkDollarStar "[[ $* = 'a b' ]]"
checkDollarStar p t@(T_NormalWord _ [b@(T_DollarBraced id _)])
      | bracedString b == "*"  =
    unless (isStrictlyQuoteFree (parentMap p) t) $
        warn id 2048 "Use \"$@\" (with quotes) to prevent whitespace problems."
checkDollarStar _ _ = return ()


prop_checkUnquotedDollarAt = verify checkUnquotedDollarAt "ls $@"
prop_checkUnquotedDollarAt1= verifyNot checkUnquotedDollarAt "ls ${#@}"
prop_checkUnquotedDollarAt2 = verify checkUnquotedDollarAt "ls ${foo[@]}"
prop_checkUnquotedDollarAt3 = verifyNot checkUnquotedDollarAt "ls ${#foo[@]}"
prop_checkUnquotedDollarAt4 = verifyNot checkUnquotedDollarAt "ls \"$@\""
prop_checkUnquotedDollarAt5 = verifyNot checkUnquotedDollarAt "ls ${foo/@/ at }"
prop_checkUnquotedDollarAt6 = verifyNot checkUnquotedDollarAt "a=$@"
prop_checkUnquotedDollarAt7 = verify checkUnquotedDollarAt "for f in ${var[@]}; do true; done"
prop_checkUnquotedDollarAt8 = verifyNot checkUnquotedDollarAt "echo \"${args[@]:+${args[@]}}\""
prop_checkUnquotedDollarAt9 = verifyNot checkUnquotedDollarAt "echo ${args[@]:+\"${args[@]}\"}"
prop_checkUnquotedDollarAt10 = verifyNot checkUnquotedDollarAt "echo ${@+\"$@\"}"
checkUnquotedDollarAt p word@(T_NormalWord _ parts) | not $ isStrictlyQuoteFree (parentMap p) word =
    forM_ (take 1 $ filter isArrayExpansion parts) $ \x ->
        unless (isQuotedAlternativeReference x) $
            err (getId x) 2068
                "Double quote array expansions to avoid re-splitting elements."
checkUnquotedDollarAt _ _ = return ()

prop_checkConcatenatedDollarAt1 = verify checkConcatenatedDollarAt "echo \"foo$@\""
prop_checkConcatenatedDollarAt2 = verify checkConcatenatedDollarAt "echo ${arr[@]}lol"
prop_checkConcatenatedDollarAt3 = verify checkConcatenatedDollarAt "echo $a$@"
prop_checkConcatenatedDollarAt4 = verifyNot checkConcatenatedDollarAt "echo $@"
prop_checkConcatenatedDollarAt5 = verifyNot checkConcatenatedDollarAt "echo \"${arr[@]}\""
checkConcatenatedDollarAt p word@T_NormalWord {}
    | not $ isQuoteFree (parentMap p) word =
        unless (null $ drop 1 parts) $
            mapM_ for array
  where
    parts = getWordParts word
    array = take 1 $ filter isArrayExpansion parts
    for t = err (getId t) 2145 "Argument mixes string and array. Use * or separate argument."
checkConcatenatedDollarAt _ _ = return ()

prop_checkArrayAsString1 = verify checkArrayAsString "a=$@"
prop_checkArrayAsString2 = verify checkArrayAsString "a=\"${arr[@]}\""
prop_checkArrayAsString3 = verify checkArrayAsString "a=*.png"
prop_checkArrayAsString4 = verify checkArrayAsString "a={1..10}"
prop_checkArrayAsString5 = verifyNot checkArrayAsString "a='*.gif'"
prop_checkArrayAsString6 = verifyNot checkArrayAsString "a=$*"
prop_checkArrayAsString7 = verifyNot checkArrayAsString "a=( $@ )"
checkArrayAsString _ (T_Assignment id _ _ _ word) =
    if willConcatInAssignment word
    then
      warn (getId word) 2124
        "Assigning an array to a string! Assign as array, or use * instead of @ to concatenate."
    else
      when (willBecomeMultipleArgs word) $
        warn (getId word) 2125
          "Brace expansions and globs are literal in assignments. Quote it or use an array."
checkArrayAsString _ _ = return ()

prop_checkArrayWithoutIndex1 = verifyTree checkArrayWithoutIndex "foo=(a b); echo $foo"
prop_checkArrayWithoutIndex2 = verifyNotTree checkArrayWithoutIndex "foo='bar baz'; foo=($foo); echo ${foo[0]}"
prop_checkArrayWithoutIndex3 = verifyTree checkArrayWithoutIndex "coproc foo while true; do echo cow; done; echo $foo"
prop_checkArrayWithoutIndex4 = verifyTree checkArrayWithoutIndex "coproc tail -f log; echo $COPROC"
prop_checkArrayWithoutIndex5 = verifyTree checkArrayWithoutIndex "a[0]=foo; echo $a"
prop_checkArrayWithoutIndex6 = verifyTree checkArrayWithoutIndex "echo $PIPESTATUS"
prop_checkArrayWithoutIndex7 = verifyTree checkArrayWithoutIndex "a=(a b); a+=c"
prop_checkArrayWithoutIndex8 = verifyTree checkArrayWithoutIndex "declare -a foo; foo=bar;"
checkArrayWithoutIndex params _ =
    doVariableFlowAnalysis readF writeF defaultMap (variableFlow params)
  where
    defaultMap = Map.fromList $ map (\x -> (x,())) arrayVariables
    readF _ (T_DollarBraced id token) _ = do
        map <- get
        return . maybeToList $ do
            name <- getLiteralString token
            assigned <- Map.lookup name map
            return $ makeComment WarningC id 2128
                    "Expanding an array without an index only gives the first element."
    readF _ _ _ = return []

    writeF _ (T_Assignment id mode name [] _) _ (DataString _) = do
        isArray <- gets (isJust . Map.lookup name)
        return $ if not isArray then [] else
            case mode of
                Assign -> [makeComment WarningC id 2178 "Variable was used as an array but is now assigned a string."]
                Append -> [makeComment WarningC id 2179 "Use array+=(\"item\") to append items to an array."]

    writeF _ t name (DataArray _) = do
        modify (Map.insert name ())
        return []
    writeF _ expr name _ = do
        if isIndexed expr
          then modify (Map.insert name ())
          else modify (Map.delete name)
        return []

    isIndexed expr =
        case expr of
            T_Assignment _ _ _ (_:_) _ -> True
            _ -> False

prop_checkStderrRedirect = verify checkStderrRedirect "test 2>&1 > cow"
prop_checkStderrRedirect2 = verifyNot checkStderrRedirect "test > cow 2>&1"
prop_checkStderrRedirect3 = verifyNot checkStderrRedirect "test 2>&1 > file | grep stderr"
prop_checkStderrRedirect4 = verifyNot checkStderrRedirect "errors=$(test 2>&1 > file)"
prop_checkStderrRedirect5 = verifyNot checkStderrRedirect "read < <(test 2>&1 > file)"
prop_checkStderrRedirect6 = verify checkStderrRedirect "foo | bar 2>&1 > /dev/null"
checkStderrRedirect params redir@(T_Redirecting _ [
    T_FdRedirect id "2" (T_IoDuplicate _ (T_GREATAND _) "1"),
    T_FdRedirect _ _ (T_IoFile _ op _)
    ] _) = case op of
            T_Greater _ -> error
            T_DGREAT _ -> error
            _ -> return ()
  where
    usesOutput t =
        case t of
            (T_Pipeline _ _ list) -> length list > 1 && not (isParentOf (parentMap params) (last list) redir)
            T_ProcSub {} -> True
            T_DollarExpansion {} -> True
            T_Backticked {} -> True
            _ -> False
    isCaptured = any usesOutput $ getPath (parentMap params) redir

    error = unless isCaptured $
        err id 2069 "The order of the 2>&1 and the redirect matters. The 2>&1 has to be last."

checkStderrRedirect _ _ = return ()

lt x = trace ("Tracing " ++ show x) x
ltt t = trace ("Tracing " ++ show t)


prop_checkSingleQuotedVariables  = verify checkSingleQuotedVariables "echo '$foo'"
prop_checkSingleQuotedVariables2 = verify checkSingleQuotedVariables "echo 'lol$1.jpg'"
prop_checkSingleQuotedVariables3 = verifyNot checkSingleQuotedVariables "sed 's/foo$/bar/'"
prop_checkSingleQuotedVariables3a= verify checkSingleQuotedVariables "sed 's/${foo}/bar/'"
prop_checkSingleQuotedVariables3b= verify checkSingleQuotedVariables "sed 's/$(echo cow)/bar/'"
prop_checkSingleQuotedVariables3c= verify checkSingleQuotedVariables "sed 's/$((1+foo))/bar/'"
prop_checkSingleQuotedVariables4 = verifyNot checkSingleQuotedVariables "awk '{print $1}'"
prop_checkSingleQuotedVariables5 = verifyNot checkSingleQuotedVariables "trap 'echo $SECONDS' EXIT"
prop_checkSingleQuotedVariables6 = verifyNot checkSingleQuotedVariables "sed -n '$p'"
prop_checkSingleQuotedVariables6a= verify checkSingleQuotedVariables "sed -n '$pattern'"
prop_checkSingleQuotedVariables7 = verifyNot checkSingleQuotedVariables "PS1='$PWD \\$ '"
prop_checkSingleQuotedVariables8 = verify checkSingleQuotedVariables "find . -exec echo '$1' {} +"
prop_checkSingleQuotedVariables9 = verifyNot checkSingleQuotedVariables "find . -exec awk '{print $1}' {} \\;"
prop_checkSingleQuotedVariables10= verify checkSingleQuotedVariables "echo '`pwd`'"
prop_checkSingleQuotedVariables11= verifyNot checkSingleQuotedVariables "sed '${/lol/d}'"
prop_checkSingleQuotedVariables12= verifyNot checkSingleQuotedVariables "eval 'echo $1'"
prop_checkSingleQuotedVariables13= verifyNot checkSingleQuotedVariables "busybox awk '{print $1}'"
prop_checkSingleQuotedVariables14= verifyNot checkSingleQuotedVariables "[ -v 'bar[$foo]' ]"
checkSingleQuotedVariables params t@(T_SingleQuoted id s) =
    when (s `matches` re) $
        if "sed" == commandName
        then unless (s `matches` sedContra) showMessage
        else unless isProbablyOk showMessage
  where
    parents = parentMap params
    showMessage = info id 2016
        "Expressions don't expand in single quotes, use double quotes for that."
    commandName = fromMaybe "" $ do
        cmd <- getClosestCommand parents t
        name <- getCommandBasename cmd
        return $ if name == "find" then getFindCommand cmd else name

    isProbablyOk =
            any isOkAssignment (take 3 $ getPath parents t)
            || commandName `elem` [
                "trap"
                ,"sh"
                ,"bash"
                ,"ksh"
                ,"zsh"
                ,"ssh"
                ,"eval"
                ,"xprop"
                ,"alias"
                ,"sudo" -- covering "sudo sh" and such
                ,"dpkg-query"
                ,"jq"  -- could also check that user provides --arg
                ]
            || "awk" `isSuffixOf` commandName
            || "perl" `isPrefixOf` commandName

    commonlyQuoted = ["PS1", "PS2", "PS3", "PS4", "PROMPT_COMMAND"]
    isOkAssignment t =
        case t of
            T_Assignment _ _ name _ _ -> name `elem` commonlyQuoted
            TC_Unary _ _ "-v" _ -> True
            _ -> False

    re = mkRegex "\\$[{(0-9a-zA-Z_]|`.*`"
    sedContra = mkRegex "\\$[{dpsaic]($|[^a-zA-Z])"

    getFindCommand (T_SimpleCommand _ _ words) =
        let list = map getLiteralString words
            cmd  = dropWhile (\x -> x /= Just "-exec" && x /= Just "-execdir") list
        in
          case cmd of
            (flag:cmd:rest) -> fromMaybe "find" cmd
            _ -> "find"
    getFindCommand (T_Redirecting _ _ cmd) = getFindCommand cmd
    getFindCommand _ = "find"
checkSingleQuotedVariables _ _ = return ()


prop_checkUnquotedN = verify checkUnquotedN "if [ -n $foo ]; then echo cow; fi"
prop_checkUnquotedN2 = verify checkUnquotedN "[ -n $cow ]"
prop_checkUnquotedN3 = verifyNot checkUnquotedN "[[ -n $foo ]] && echo cow"
prop_checkUnquotedN4 = verify checkUnquotedN "[ -n $cow -o -t 1 ]"
prop_checkUnquotedN5 = verifyNot checkUnquotedN "[ -n \"$@\" ]"
checkUnquotedN _ (TC_Unary _ SingleBracket "-n" (T_NormalWord id [t])) | willSplit t =
       err id 2070 "-n doesn't work with unquoted arguments. Quote or use [[ ]]."
checkUnquotedN _ _ = return ()

prop_checkNumberComparisons1 = verify checkNumberComparisons "[[ $foo < 3 ]]"
prop_checkNumberComparisons2 = verify checkNumberComparisons "[[ 0 >= $(cmd) ]]"
prop_checkNumberComparisons3 = verifyNot checkNumberComparisons "[[ $foo ]] > 3"
prop_checkNumberComparisons4 = verify checkNumberComparisons "[[ $foo > 2.72 ]]"
prop_checkNumberComparisons5 = verify checkNumberComparisons "[[ $foo -le 2.72 ]]"
prop_checkNumberComparisons6 = verify checkNumberComparisons "[[ 3.14 -eq $foo ]]"
prop_checkNumberComparisons7 = verifyNot checkNumberComparisons "[[ 3.14 == $foo ]]"
prop_checkNumberComparisons8 = verify checkNumberComparisons "[ foo <= bar ]"
prop_checkNumberComparisons9 = verify checkNumberComparisons "[ foo \\>= bar ]"
prop_checkNumberComparisons11 = verify checkNumberComparisons "[ $foo -eq 'N' ]"
prop_checkNumberComparisons12 = verify checkNumberComparisons "[ x$foo -gt x${N} ]"
prop_checkNumberComparisons13 = verify checkNumberComparisons "[ $foo > $bar ]"
prop_checkNumberComparisons14 = verifyNot checkNumberComparisons "[[ foo < bar ]]"
prop_checkNumberComparisons15 = verifyNot checkNumberComparisons "[ $foo '>' $bar ]"
checkNumberComparisons params (TC_Binary id typ op lhs rhs) = do
    if isNum lhs || isNum rhs
      then do
        when (isLtGt op) $
          err id 2071 $
            op ++ " is for string comparisons. Use " ++ eqv op ++ " instead."
        when (isLeGe op && hasStringComparison) $
            err id 2071 $ op ++ " is not a valid operator. " ++
              "Use " ++ eqv op ++ " ."
      else do
        when (isLeGe op || isLtGt op) $
            mapM_ checkDecimals [lhs, rhs]

        when (isLeGe op && hasStringComparison) $
            err id 2122 $ op ++ " is not a valid operator. " ++
                "Use '! a " ++ esc ++ invert op ++ " b' instead."

        when (typ == SingleBracket && op `elem` ["<", ">"]) $
            case shellType params of
                Sh -> return ()  -- These are unsupported and will be caught by bashism checks.
                Dash -> err id 2073 $ "Escape \\" ++ op ++ " to prevent it redirecting."
                _ -> err id 2073 $ "Escape \\" ++ op ++ " to prevent it redirecting (or switch to [[ .. ]])."

    when (op `elem` ["-lt", "-gt", "-le", "-ge", "-eq"]) $ do
        mapM_ checkDecimals [lhs, rhs]
        when (typ == SingleBracket) $
            checkStrings [lhs, rhs]

  where
      hasStringComparison = shellType params /= Sh
      isLtGt = flip elem ["<", "\\<", ">", "\\>"]
      isLeGe = flip elem ["<=", "\\<=", ">=", "\\>="]

      checkDecimals hs =
        when (isFraction hs && not (hasFloatingPoint params)) $
            err (getId hs) 2072 decimalError
      decimalError = "Decimals are not supported. " ++
        "Either use integers only, or use bc or awk to compare."

      checkStrings =
        mapM_ stringError . take 1 . filter isNonNum

      isNonNum t = fromMaybe False $ do
        s <- getLiteralStringExt (const $ return "") t
        return . not . all numChar $ s
      numChar x = isDigit x || x `elem` "+-. "

      stringError t = err (getId t) 2170 $
          "Numerical " ++ op ++ " does not dereference in [..]. Expand or use string operator."

      isNum t =
        case oversimplify t of
            [v] -> all isDigit v
            _ -> False
      isFraction t =
        case oversimplify t of
            [v] -> isJust $ matchRegex floatRegex v
            _ -> False

      eqv ('\\':s) = eqv s
      eqv "<" = "-lt"
      eqv ">" = "-gt"
      eqv "<=" = "-le"
      eqv ">=" = "-ge"
      eqv _ = "the numerical equivalent"

      esc = if typ == SingleBracket then "\\" else ""
      seqv "-ge" = "! a " ++ esc ++ "< b"
      seqv "-gt" = esc ++ ">"
      seqv "-le" = "! a " ++ esc ++ "> b"
      seqv "-lt" = esc ++ "<"
      seqv "-eq" = "="
      seqv "-ne" = "!="
      seqv _ = "the string equivalent"

      invert ('\\':s) = invert s
      invert "<=" = ">"
      invert ">=" = "<"

      floatRegex = mkRegex "^[-+]?[0-9]+\\.[0-9]+$"
checkNumberComparisons _ _ = return ()

prop_checkSingleBracketOperators1 = verify checkSingleBracketOperators "[ test =~ foo ]"
checkSingleBracketOperators params (TC_Binary id SingleBracket "=~" lhs rhs) =
    when (shellType params `elem` [Bash, Ksh]) $
        err id 2074 $ "Can't use =~ in [ ]. Use [[..]] instead."
checkSingleBracketOperators _ _ = return ()

prop_checkDoubleBracketOperators1 = verify checkDoubleBracketOperators "[[ 3 \\< 4 ]]"
prop_checkDoubleBracketOperators3 = verifyNot checkDoubleBracketOperators "[[ foo < bar ]]"
checkDoubleBracketOperators _ x@(TC_Binary id typ op lhs rhs)
    | typ == DoubleBracket && op `elem` ["\\<", "\\>"] =
        err id 2075 $ "Escaping " ++ op ++" is required in [..], but invalid in [[..]]"
checkDoubleBracketOperators _ _ = return ()

prop_checkConditionalAndOrs1 = verify checkConditionalAndOrs "[ foo && bar ]"
prop_checkConditionalAndOrs2 = verify checkConditionalAndOrs "[[ foo -o bar ]]"
prop_checkConditionalAndOrs3 = verifyNot checkConditionalAndOrs "[[ foo || bar ]]"
prop_checkConditionalAndOrs4 = verify checkConditionalAndOrs "[ foo -a bar ]"
prop_checkConditionalAndOrs5 = verify checkConditionalAndOrs "[ -z 3 -o a = b ]"
checkConditionalAndOrs _ t =
    case t of
        (TC_And id SingleBracket "&&" _ _) ->
            err id 2107 "Instead of [ a && b ], use [ a ] && [ b ]."
        (TC_And id DoubleBracket "-a" _ _) ->
            err id 2108 "In [[..]], use && instead of -a."
        (TC_Or id SingleBracket "||" _ _) ->
            err id 2109 "Instead of [ a || b ], use [ a ] || [ b ]."
        (TC_Or id DoubleBracket "-o" _ _) ->
            err id 2110 "In [[..]], use || instead of -o."

        (TC_And id SingleBracket "-a" _ _) ->
            warn id 2166 "Prefer [ p ] && [ q ] as [ p -a q ] is not well defined."
        (TC_Or id SingleBracket "-o" _ _) ->
            warn id 2166 "Prefer [ p ] || [ q ] as [ p -o q ] is not well defined."

        _ -> return ()

prop_checkQuotedCondRegex1 = verify checkQuotedCondRegex "[[ $foo =~ \"bar.*\" ]]"
prop_checkQuotedCondRegex2 = verify checkQuotedCondRegex "[[ $foo =~ '(cow|bar)' ]]"
prop_checkQuotedCondRegex3 = verifyNot checkQuotedCondRegex "[[ $foo =~ $foo ]]"
prop_checkQuotedCondRegex4 = verifyNot checkQuotedCondRegex "[[ $foo =~ \"bar\" ]]"
prop_checkQuotedCondRegex5 = verifyNot checkQuotedCondRegex "[[ $foo =~ 'cow bar' ]]"
prop_checkQuotedCondRegex6 = verify checkQuotedCondRegex "[[ $foo =~ 'cow|bar' ]]"
checkQuotedCondRegex _ (TC_Binary _ _ "=~" _ rhs) =
    case rhs of
        T_NormalWord id [T_DoubleQuoted _ _] -> error rhs
        T_NormalWord id [T_SingleQuoted _ _] -> error rhs
        _ -> return ()
  where
    error t =
        unless (isConstantNonRe t) $
            err (getId t) 2076
                "Don't quote rhs of =~, it'll match literally rather than as a regex."
    re = mkRegex "[][*.+()|]"
    hasMetachars s = s `matches` re
    isConstantNonRe t = fromMaybe False $ do
        s <- getLiteralString t
        return . not $ hasMetachars s
checkQuotedCondRegex _ _ = return ()

prop_checkGlobbedRegex1 = verify checkGlobbedRegex "[[ $foo =~ *foo* ]]"
prop_checkGlobbedRegex2 = verify checkGlobbedRegex "[[ $foo =~ f* ]]"
prop_checkGlobbedRegex2a = verify checkGlobbedRegex "[[ $foo =~ \\#* ]]"
prop_checkGlobbedRegex3 = verifyNot checkGlobbedRegex "[[ $foo =~ $foo ]]"
prop_checkGlobbedRegex4 = verifyNot checkGlobbedRegex "[[ $foo =~ ^c.* ]]"
checkGlobbedRegex _ (TC_Binary _ DoubleBracket "=~" _ rhs) =
    let s = concat $ oversimplify rhs in
        when (isConfusedGlobRegex s) $
            warn (getId rhs) 2049 "=~ is for regex. Use == for globs."
checkGlobbedRegex _ _ = return ()


prop_checkConstantIfs1 = verify checkConstantIfs "[[ foo != bar ]]"
prop_checkConstantIfs2a= verify checkConstantIfs "[ n -le 4 ]"
prop_checkConstantIfs2b= verifyNot checkConstantIfs "[[ n -le 4 ]]"
prop_checkConstantIfs3 = verify checkConstantIfs "[[ $n -le 4 && n != 2 ]]"
prop_checkConstantIfs4 = verifyNot checkConstantIfs "[[ $n -le 3 ]]"
prop_checkConstantIfs5 = verifyNot checkConstantIfs "[[ $n -le $n ]]"
prop_checkConstantIfs6 = verifyNot checkConstantIfs "[[ a -ot b ]]"
prop_checkConstantIfs7 = verifyNot checkConstantIfs "[ a -nt b ]"
prop_checkConstantIfs8 = verifyNot checkConstantIfs "[[ ~foo == '~foo' ]]"
prop_checkConstantIfs9 = verify checkConstantIfs "[[ *.png == [a-z] ]]"
checkConstantIfs _ (TC_Binary id typ op lhs rhs) | not isDynamic =
    if isConstant lhs && isConstant rhs
        then  warn id 2050 "This expression is constant. Did you forget the $ on a variable?"
        else checkUnmatchable id op lhs rhs
  where
    isDynamic =
        op `elem` [ "-lt", "-gt", "-le", "-ge", "-eq", "-ne" ]
            && typ == DoubleBracket
        || op `elem` [ "-nt", "-ot", "-ef"]

    checkUnmatchable id op lhs rhs =
        when (op `elem` ["=", "==", "!="] && not (wordsCanBeEqual lhs rhs)) $
            warn id 2193 "The arguments to this comparison can never be equal. Make sure your syntax is correct."
checkConstantIfs _ _ = return ()

prop_checkLiteralBreakingTest = verify checkLiteralBreakingTest "[[ a==$foo ]]"
prop_checkLiteralBreakingTest2 = verify checkLiteralBreakingTest "[ $foo=3 ]"
prop_checkLiteralBreakingTest3 = verify checkLiteralBreakingTest "[ $foo!=3 ]"
prop_checkLiteralBreakingTest4 = verify checkLiteralBreakingTest "[ \"$(ls) \" ]"
prop_checkLiteralBreakingTest5 = verify checkLiteralBreakingTest "[ -n \"$(true) \" ]"
prop_checkLiteralBreakingTest6 = verify checkLiteralBreakingTest "[ -z $(true)z ]"
prop_checkLiteralBreakingTest7 = verifyNot checkLiteralBreakingTest "[ -z $(true) ]"
prop_checkLiteralBreakingTest8 = verifyNot checkLiteralBreakingTest "[ $(true)$(true) ]"
prop_checkLiteralBreakingTest10 = verify checkLiteralBreakingTest "[ -z foo ]"
checkLiteralBreakingTest _ t = potentially $
        case t of
            (TC_Nullary _ _ w@(T_NormalWord _ l)) -> do
                guard . not $ isConstant w -- Covered by SC2078
                comparisonWarning l `mplus` tautologyWarning w "Argument to implicit -n is always true due to literal strings."
            (TC_Unary _ _ op w@(T_NormalWord _ l)) ->
                case op of
                    "-n" -> tautologyWarning w "Argument to -n is always true due to literal strings."
                    "-z" -> tautologyWarning w "Argument to -z is always false due to literal strings."
                    _ -> fail "not relevant"
            _ -> fail "not my problem"
  where
    hasEquals = matchToken ('=' `elem`)
    isNonEmpty = matchToken (not . null)
    matchToken m t = isJust $ do
        str <- getLiteralString t
        guard $ m str
        return ()

    comparisonWarning list = do
        token <- listToMaybe $ filter hasEquals list
        return $ err (getId token) 2077 "You need spaces around the comparison operator."
    tautologyWarning t s = do
        token <- listToMaybe $ filter isNonEmpty $ getWordParts t
        return $ err (getId token) 2157 s

prop_checkConstantNullary = verify checkConstantNullary "[[ '$(foo)' ]]"
prop_checkConstantNullary2 = verify checkConstantNullary "[ \"-f lol\" ]"
prop_checkConstantNullary3 = verify checkConstantNullary "[[ cmd ]]"
prop_checkConstantNullary4 = verify checkConstantNullary "[[ ! cmd ]]"
prop_checkConstantNullary5 = verify checkConstantNullary "[[ true ]]"
prop_checkConstantNullary6 = verify checkConstantNullary "[ 1 ]"
prop_checkConstantNullary7 = verify checkConstantNullary "[ false ]"
checkConstantNullary _ (TC_Nullary _ _ t) | isConstant t =
    case fromMaybe "" $ getLiteralString t of
        "false" -> err (getId t) 2158 "[ false ] is true. Remove the brackets."
        "0" -> err (getId t) 2159 "[ 0 ] is true. Use 'false' instead."
        "true" -> style (getId t) 2160 "Instead of '[ true ]', just use 'true'."
        "1" -> style (getId t) 2161 "Instead of '[ 1 ]', use 'true'."
        _ -> err (getId t) 2078 "This expression is constant. Did you forget a $ somewhere?"
  where
    string = fromMaybe "" $ getLiteralString t

checkConstantNullary _ _ = return ()

prop_checkForDecimals1 = verify checkForDecimals "((3.14*c))"
prop_checkForDecimals2 = verify checkForDecimals "foo[1.2]=bar"
prop_checkForDecimals3 = verifyNot checkForDecimals "declare -A foo; foo[1.2]=bar"
checkForDecimals params t@(TA_Expansion id _) = potentially $ do
    guard $ not (hasFloatingPoint params)
    str <- getLiteralString t
    first <- str !!! 0
    guard $ isDigit first && '.' `elem` str
    return $ err id 2079 "(( )) doesn't support decimals. Use bc or awk."
checkForDecimals _ _ = return ()

prop_checkDivBeforeMult = verify checkDivBeforeMult "echo $((c/n*100))"
prop_checkDivBeforeMult2 = verifyNot checkDivBeforeMult "echo $((c*100/n))"
prop_checkDivBeforeMult3 = verifyNot checkDivBeforeMult "echo $((c/10*10))"
checkDivBeforeMult params (TA_Binary _ "*" (TA_Binary id "/" _ x) y)
    | not (hasFloatingPoint params) && x /= y =
        info id 2017 "Increase precision by replacing a/b*c with a*c/b."
checkDivBeforeMult _ _ = return ()

prop_checkArithmeticDeref = verify checkArithmeticDeref "echo $((3+$foo))"
prop_checkArithmeticDeref2 = verify checkArithmeticDeref "cow=14; (( s+= $cow ))"
prop_checkArithmeticDeref3 = verifyNot checkArithmeticDeref "cow=1/40; (( s+= ${cow%%/*} ))"
prop_checkArithmeticDeref4 = verifyNot checkArithmeticDeref "(( ! $? ))"
prop_checkArithmeticDeref5 = verifyNot checkArithmeticDeref "(($1))"
prop_checkArithmeticDeref6 = verify checkArithmeticDeref "(( a[$i] ))"
prop_checkArithmeticDeref7 = verifyNot checkArithmeticDeref "(( 10#$n ))"
prop_checkArithmeticDeref8 = verifyNot checkArithmeticDeref "let i=$i+1"
prop_checkArithmeticDeref9 = verifyNot checkArithmeticDeref "(( a[foo] ))"
prop_checkArithmeticDeref10= verifyNot checkArithmeticDeref "(( a[\\$foo] ))"
prop_checkArithmeticDeref11= verifyNot checkArithmeticDeref "a[$foo]=wee"
prop_checkArithmeticDeref12= verify checkArithmeticDeref "for ((i=0; $i < 3; i)); do true; done"
prop_checkArithmeticDeref13= verifyNot checkArithmeticDeref "(( $$ ))"
prop_checkArithmeticDeref14= verifyNot checkArithmeticDeref "(( $! ))"
prop_checkArithmeticDeref15= verifyNot checkArithmeticDeref "(( ${!var} ))"
checkArithmeticDeref params t@(TA_Expansion _ [b@(T_DollarBraced id _)]) =
    unless (isException $ bracedString b) getWarning
  where
    isException [] = True
    isException s = any (`elem` "/.:#%?*@$-!") s || isDigit (head s)
    getWarning = fromMaybe noWarning . msum . map warningFor $ parents params t
    warningFor t =
        case t of
            T_Arithmetic {} -> return normalWarning
            T_DollarArithmetic {} -> return normalWarning
            T_ForArithmetic {} -> return normalWarning
            TA_Index {} -> return indexWarning
            T_SimpleCommand {} -> return noWarning
            _ -> Nothing

    normalWarning = style id 2004 "$/${} is unnecessary on arithmetic variables."
    indexWarning = style id 2149 "Remove $/${} for numeric index, or escape it for string."
    noWarning = return ()
checkArithmeticDeref _ _ = return ()

prop_checkArithmeticBadOctal1 = verify checkArithmeticBadOctal "(( 0192 ))"
prop_checkArithmeticBadOctal2 = verifyNot checkArithmeticBadOctal "(( 0x192 ))"
prop_checkArithmeticBadOctal3 = verifyNot checkArithmeticBadOctal "(( 1 ^ 0777 ))"
checkArithmeticBadOctal _ t@(TA_Expansion id _) = potentially $ do
    str <- getLiteralString t
    guard $ str `matches` octalRE
    return $ err id 2080 "Numbers with leading 0 are considered octal."
  where
    octalRE = mkRegex "^0[0-7]*[8-9]"
checkArithmeticBadOctal _ _ = return ()

prop_checkComparisonAgainstGlob = verify checkComparisonAgainstGlob "[[ $cow == $bar ]]"
prop_checkComparisonAgainstGlob2 = verifyNot checkComparisonAgainstGlob "[[ $cow == \"$bar\" ]]"
prop_checkComparisonAgainstGlob3 = verify checkComparisonAgainstGlob "[ $cow = *foo* ]"
prop_checkComparisonAgainstGlob4 = verifyNot checkComparisonAgainstGlob "[ $cow = foo ]"
prop_checkComparisonAgainstGlob5 = verify checkComparisonAgainstGlob "[[ $cow != $bar ]]"
checkComparisonAgainstGlob _ (TC_Binary _ DoubleBracket op _ (T_NormalWord id [T_DollarBraced _ _]))
    | op `elem` ["=", "==", "!="] =
        warn id 2053 $ "Quote the rhs of " ++ op ++ " in [[ ]] to prevent glob matching."
checkComparisonAgainstGlob _ (TC_Binary _ SingleBracket op _ word)
        | (op == "=" || op == "==") && isGlob word =
    err (getId word) 2081 "[ .. ] can't match globs. Use [[ .. ]] or case statement."
checkComparisonAgainstGlob _ _ = return ()

prop_checkCommarrays1 = verify checkCommarrays "a=(1, 2)"
prop_checkCommarrays2 = verify checkCommarrays "a+=(1,2,3)"
prop_checkCommarrays3 = verifyNot checkCommarrays "cow=(1 \"foo,bar\" 3)"
prop_checkCommarrays4 = verifyNot checkCommarrays "cow=('one,' 'two')"
prop_checkCommarrays5 = verify checkCommarrays "a=([a]=b, [c]=d)"
prop_checkCommarrays6 = verify checkCommarrays "a=([a]=b,[c]=d,[e]=f)"
checkCommarrays _ (T_Array id l) =
    when (any (isCommaSeparated . literal) l) $
        warn id 2054 "Use spaces, not commas, to separate array elements."
  where
    literal (T_IndexedElement _ _ l) = literal l
    literal (T_NormalWord _ l) = concatMap literal l
    literal (T_Literal _ str) = str
    literal _ = "str"

    isCommaSeparated str = "," `isSuffixOf` str || length (filter (== ',') str) > 1
checkCommarrays _ _ = return ()

prop_checkOrNeq1 = verify checkOrNeq "if [[ $lol -ne cow || $lol -ne foo ]]; then echo foo; fi"
prop_checkOrNeq2 = verify checkOrNeq "(( a!=lol || a!=foo ))"
prop_checkOrNeq3 = verify checkOrNeq "[ \"$a\" != lol || \"$a\" != foo ]"
prop_checkOrNeq4 = verifyNot checkOrNeq "[ a != $cow || b != $foo ]"
prop_checkOrNeq5 = verifyNot checkOrNeq "[[ $a != /home || $a != */public_html/* ]]"
-- This only catches the most idiomatic cases. Fixme?
checkOrNeq _ (TC_Or id typ op (TC_Binary _ _ op1 lhs1 rhs1 ) (TC_Binary _ _ op2 lhs2 rhs2))
    | lhs1 == lhs2 && (op1 == op2 && (op1 == "-ne" || op1 == "!=")) && not (any isGlob [rhs1,rhs2]) =
        warn id 2055 $ "You probably wanted " ++ (if typ == SingleBracket then "-a" else "&&") ++ " here."

checkOrNeq _ (TA_Binary id "||" (TA_Binary _ "!=" word1 _) (TA_Binary _ "!=" word2 _))
    | word1 == word2 =
        warn id 2056 "You probably wanted && here."
checkOrNeq _ _ = return ()


prop_checkValidCondOps1 = verify checkValidCondOps "[[ a -xz b ]]"
prop_checkValidCondOps2 = verify checkValidCondOps "[ -M a ]"
prop_checkValidCondOps2a= verifyNot checkValidCondOps "[ 3 \\> 2 ]"
prop_checkValidCondOps3 = verifyNot checkValidCondOps "[ 1 = 2 -a 3 -ge 4 ]"
prop_checkValidCondOps4 = verifyNot checkValidCondOps "[[ ! -v foo ]]"
checkValidCondOps _ (TC_Binary id _ s _ _)
    | s `notElem` binaryTestOps =
        warn id 2057 "Unknown binary operator."
checkValidCondOps _ (TC_Unary id _ s _)
    | s `notElem`  unaryTestOps =
        warn id 2058 "Unknown unary operator."
checkValidCondOps _ _ = return ()

prop_checkUuoeVar1 = verify checkUuoeVar "for f in $(echo $tmp); do echo lol; done"
prop_checkUuoeVar2 = verify checkUuoeVar "date +`echo \"$format\"`"
prop_checkUuoeVar3 = verifyNot checkUuoeVar "foo \"$(echo -e '\r')\""
prop_checkUuoeVar4 = verifyNot checkUuoeVar "echo $tmp"
prop_checkUuoeVar5 = verify checkUuoeVar "foo \"$(echo \"$(date) value:\" $value)\""
prop_checkUuoeVar6 = verifyNot checkUuoeVar "foo \"$(echo files: *.png)\""
prop_checkUuoeVar7 = verifyNot checkUuoeVar "foo $(echo $(bar))" -- covered by 2005
prop_checkUuoeVar8 = verifyNot checkUuoeVar "#!/bin/sh\nz=$(echo)"
prop_checkUuoeVar9 = verify checkUuoeVar "foo $(echo $(<file))"
checkUuoeVar _ p =
    case p of
        T_Backticked id [cmd] -> check id cmd
        T_DollarExpansion id [cmd] -> check id cmd
        _ -> return ()
  where
    couldBeOptimized f = case f of
        T_Glob {} -> False
        T_Extglob {} -> False
        T_BraceExpansion {} -> False
        T_NormalWord _ l -> all couldBeOptimized l
        T_DoubleQuoted _ l -> all couldBeOptimized l
        _ -> True

    check id (T_Pipeline _ _ [T_Redirecting _ _ c]) = warnForEcho id c
    check _ _ = return ()
    isCovered first rest = null rest && tokenIsJustCommandOutput first
    warnForEcho id = checkUnqualifiedCommand "echo" $ \_ vars ->
        case vars of
          (first:rest) ->
            unless (isCovered first rest || "-" `isPrefixOf` onlyLiteralString first) $
                when (all couldBeOptimized vars) $ style id 2116
                    "Useless echo? Instead of 'cmd $(echo foo)', just use 'cmd foo'."
          _ -> return ()


prop_checkTestRedirects1 = verify checkTestRedirects "test 3 > 1"
prop_checkTestRedirects2 = verifyNot checkTestRedirects "test 3 \\> 1"
prop_checkTestRedirects3 = verify checkTestRedirects "/usr/bin/test $var > $foo"
prop_checkTestRedirects4 = verifyNot checkTestRedirects "test 1 -eq 2 2> file"
checkTestRedirects _ (T_Redirecting id redirs cmd) | cmd `isCommand` "test" =
    mapM_ check redirs
  where
    check t =
        when (suspicious t) $
            warn (getId t) 2065 "This is interpreted as a shell file redirection, not a comparison."
    suspicious t = -- Ignore redirections of stderr because these are valid for squashing e.g. int errors,
        case t of  -- and >> and similar redirections because these are probably not comparisons.
            T_FdRedirect _ fd (T_IoFile _ op _) -> fd /= "2" && isComparison op
            _ -> False
    isComparison t =
        case t of
            T_Greater _ -> True
            T_Less _ -> True
            _ -> False
checkTestRedirects _ _ = return ()

prop_checkSudoRedirect1 = verify checkSudoRedirect "sudo echo 3 > /proc/file"
prop_checkSudoRedirect2 = verify checkSudoRedirect "sudo cmd < input"
prop_checkSudoRedirect3 = verify checkSudoRedirect "sudo cmd >> file"
prop_checkSudoRedirect4 = verify checkSudoRedirect "sudo cmd &> file"
prop_checkSudoRedirect5 = verifyNot checkSudoRedirect "sudo cmd 2>&1"
prop_checkSudoRedirect6 = verifyNot checkSudoRedirect "sudo cmd 2> log"
prop_checkSudoRedirect7 = verifyNot checkSudoRedirect "sudo cmd > /dev/null 2>&1"
checkSudoRedirect _ (T_Redirecting _ redirs cmd) | cmd `isCommand` "sudo" =
    mapM_ warnAbout redirs
  where
    warnAbout (T_FdRedirect _ s (T_IoFile id op file))
        | (s == "" || s == "&") && not (special file) =
        case op of
            T_Less _ ->
              info (getId op) 2024
                "sudo doesn't affect redirects. Use sudo cat file | .."
            T_Greater _ ->
              warn (getId op) 2024
                "sudo doesn't affect redirects. Use ..| sudo tee file"
            T_DGREAT _ ->
              warn (getId op) 2024
                "sudo doesn't affect redirects. Use .. | sudo tee -a file"
            _ -> return ()
    warnAbout _ = return ()
    special file = concat (oversimplify file) == "/dev/null"
checkSudoRedirect _ _ = return ()

prop_checkPS11 = verify checkPS1Assignments "PS1='\\033[1;35m\\$ '"
prop_checkPS11a= verify checkPS1Assignments "export PS1='\\033[1;35m\\$ '"
prop_checkPSf2 = verify checkPS1Assignments "PS1='\\h \\e[0m\\$ '"
prop_checkPS13 = verify checkPS1Assignments "PS1=$'\\x1b[c '"
prop_checkPS14 = verify checkPS1Assignments "PS1=$'\\e[3m; '"
prop_checkPS14a= verify checkPS1Assignments "export PS1=$'\\e[3m; '"
prop_checkPS15 = verifyNot checkPS1Assignments "PS1='\\[\\033[1;35m\\]\\$ '"
prop_checkPS16 = verifyNot checkPS1Assignments "PS1='\\[\\e1m\\e[1m\\]\\$ '"
prop_checkPS17 = verifyNot checkPS1Assignments "PS1='e033x1B'"
prop_checkPS18 = verifyNot checkPS1Assignments "PS1='\\[\\e\\]'"
checkPS1Assignments _ (T_Assignment _ _ "PS1" _ word) = warnFor word
  where
    warnFor word =
        let contents = concat $ oversimplify word in
            when (containsUnescaped contents) $
                info (getId word) 2025 "Make sure all escape sequences are enclosed in \\[..\\] to prevent line wrapping issues"
    containsUnescaped s =
        let unenclosed = subRegex enclosedRegex s "" in
           isJust $ matchRegex escapeRegex unenclosed
    enclosedRegex = mkRegex "\\\\\\[.*\\\\\\]" -- FIXME: shouldn't be eager
    escapeRegex = mkRegex "\\\\x1[Bb]|\\\\e|\x1B|\\\\033"
checkPS1Assignments _ _ = return ()

prop_checkBackticks1 = verify checkBackticks "echo `foo`"
prop_checkBackticks2 = verifyNot checkBackticks "echo $(foo)"
prop_checkBackticks3 = verifyNot checkBackticks "echo `#inlined comment` foo"
checkBackticks _ (T_Backticked id list) | not (null list) =
    style id 2006 "Use $(..) instead of legacy `..`."
checkBackticks _ _ = return ()

prop_checkIndirectExpansion1 = verify checkIndirectExpansion "${foo$n}"
prop_checkIndirectExpansion2 = verifyNot checkIndirectExpansion "${foo//$n/lol}"
prop_checkIndirectExpansion3 = verify checkIndirectExpansion "${$#}"
prop_checkIndirectExpansion4 = verify checkIndirectExpansion "${var${n}_$((i%2))}"
prop_checkIndirectExpansion5 = verifyNot checkIndirectExpansion "${bar}"
checkIndirectExpansion _ (T_DollarBraced i (T_NormalWord _ contents)) =
    when (isIndirection contents) $
        err i 2082 "To expand via indirection, use arrays, ${!name} or (for sh only) eval."
  where
    isIndirection vars =
        let list = mapMaybe isIndirectionPart vars in
            not (null list) && and list
    isIndirectionPart t =
        case t of T_DollarExpansion _ _ ->  Just True
                  T_Backticked _ _ ->       Just True
                  T_DollarBraced _ _ ->     Just True
                  T_DollarArithmetic _ _ -> Just True
                  T_Literal _ s -> if all isVariableChar s
                                    then Nothing
                                    else Just False
                  _ -> Just False

checkIndirectExpansion _ _ = return ()

prop_checkInexplicablyUnquoted1 = verify checkInexplicablyUnquoted "echo 'var='value';'"
prop_checkInexplicablyUnquoted2 = verifyNot checkInexplicablyUnquoted "'foo'*"
prop_checkInexplicablyUnquoted3 = verifyNot checkInexplicablyUnquoted "wget --user-agent='something'"
prop_checkInexplicablyUnquoted4 = verify checkInexplicablyUnquoted "echo \"VALUES (\"id\")\""
prop_checkInexplicablyUnquoted5 = verifyNot checkInexplicablyUnquoted "\"$dir\"/\"$file\""
prop_checkInexplicablyUnquoted6 = verifyNot checkInexplicablyUnquoted "\"$dir\"some_stuff\"$file\""
prop_checkInexplicablyUnquoted7 = verifyNot checkInexplicablyUnquoted "${dir/\"foo\"/\"bar\"}"
prop_checkInexplicablyUnquoted8 = verifyNot checkInexplicablyUnquoted "  'foo'\\\n  'bar'"
checkInexplicablyUnquoted _ (T_NormalWord id tokens) = mapM_ check (tails tokens)
  where
    check (T_SingleQuoted _ _:T_Literal id str:_)
        | not (null str) && all isAlphaNum str =
        info id 2026 "This word is outside of quotes. Did you intend to 'nest '\"'single quotes'\"' instead'? "

    check (T_DoubleQuoted _ a:trapped:T_DoubleQuoted _ b:_) =
        case trapped of
            T_DollarExpansion id _ -> warnAboutExpansion id
            T_DollarBraced id _ -> warnAboutExpansion id
            T_Literal id s ->
                unless (quotesSingleThing a && quotesSingleThing b) $
                    warnAboutLiteral id
            _ -> return ()

    check _ = return ()

    -- If the surrounding quotes quote single things, like "$foo"_and_then_some_"$stuff",
    -- the quotes were probably intentional and harmless.
    quotesSingleThing x = case x of
        [T_DollarExpansion _ _] -> True
        [T_DollarBraced _ _] -> True
        [T_Backticked _ _] -> True
        _ -> False

    warnAboutExpansion id =
        warn id 2027 "The surrounding quotes actually unquote this. Remove or escape them."
    warnAboutLiteral id =
        warn id 2140 "Word is of the form \"A\"B\"C\" (B indicated). Did you mean \"ABC\" or \"A\\\"B\\\"C\"?"
checkInexplicablyUnquoted _ _ = return ()

prop_checkTildeInQuotes1 = verify checkTildeInQuotes "var=\"~/out.txt\""
prop_checkTildeInQuotes2 = verify checkTildeInQuotes "foo > '~/dir'"
prop_checkTildeInQuotes4 = verifyNot checkTildeInQuotes "~/file"
prop_checkTildeInQuotes5 = verifyNot checkTildeInQuotes "echo '/~foo/cow'"
prop_checkTildeInQuotes6 = verifyNot checkTildeInQuotes "awk '$0 ~ /foo/'"
checkTildeInQuotes _ = check
  where
    verify id ('~':'/':_) = warn id 2088 "Tilde does not expand in quotes. Use $HOME."
    verify _ _ = return ()
    check (T_NormalWord _ (T_SingleQuoted id str:_)) =
        verify id str
    check (T_NormalWord _ (T_DoubleQuoted _ (T_Literal id str:_):_)) =
        verify id str
    check _ = return ()

prop_checkLonelyDotDash1 = verify checkLonelyDotDash "./ file"
prop_checkLonelyDotDash2 = verifyNot checkLonelyDotDash "./file"
checkLonelyDotDash _ t@(T_Redirecting id _ _)
    | isUnqualifiedCommand t "./" =
        err id 2083 "Don't add spaces after the slash in './file'."
checkLonelyDotDash _ _ = return ()


prop_checkSpuriousExec1 = verify checkSpuriousExec "exec foo; true"
prop_checkSpuriousExec2 = verify checkSpuriousExec "if a; then exec b; exec c; fi"
prop_checkSpuriousExec3 = verifyNot checkSpuriousExec "echo cow; exec foo"
prop_checkSpuriousExec4 = verifyNot checkSpuriousExec "if a; then exec b; fi"
prop_checkSpuriousExec5 = verifyNot checkSpuriousExec "exec > file; cmd"
prop_checkSpuriousExec6 = verify checkSpuriousExec "exec foo > file; cmd"
prop_checkSpuriousExec7 = verifyNot checkSpuriousExec "exec file; echo failed; exit 3"
checkSpuriousExec _ = doLists
  where
    doLists (T_Script _ _ cmds) = doList cmds
    doLists (T_BraceGroup _ cmds) = doList cmds
    doLists (T_WhileExpression _ _ cmds) = doList cmds
    doLists (T_UntilExpression _ _ cmds) = doList cmds
    doLists (T_ForIn _ _ _ cmds) = doList cmds
    doLists (T_ForArithmetic _ _ _ _ cmds) = doList cmds
    doLists (T_IfExpression _ thens elses) = do
        mapM_ (\(_, l) -> doList l) thens
        doList elses
    doLists _ = return ()

    stripCleanup = reverse . dropWhile cleanup . reverse
    cleanup (T_Pipeline _ _ [cmd]) =
        isCommandMatch cmd (`elem` ["echo", "exit"])
    cleanup _ = False

    doList = doList' . stripCleanup
    doList' t@(current:following:_) = do
        commentIfExec current
        doList (tail t)
    doList' _ = return ()

    commentIfExec (T_Pipeline id _ list) =
      mapM_ commentIfExec $ take 1 list
    commentIfExec (T_Redirecting _ _ f@(
      T_SimpleCommand id _ (cmd:arg:_))) =
        when (f `isUnqualifiedCommand` "exec") $
          warn id 2093
            "Remove \"exec \" if script should continue after this command."
    commentIfExec _ = return ()


prop_checkSpuriousExpansion1 = verify checkSpuriousExpansion "if $(true); then true; fi"
prop_checkSpuriousExpansion2 = verify checkSpuriousExpansion "while \"$(cmd)\"; do :; done"
prop_checkSpuriousExpansion3 = verifyNot checkSpuriousExpansion "$(cmd) --flag1 --flag2"
prop_checkSpuriousExpansion4 = verify checkSpuriousExpansion "$((i++))"
checkSpuriousExpansion _ (T_SimpleCommand _ _ [T_NormalWord _ [word]]) = check word
  where
    check word = case word of
        T_DollarExpansion id _ ->
            warn id 2091 "Remove surrounding $() to avoid executing output."
        T_Backticked id _ ->
            warn id 2092 "Remove backticks to avoid executing output."
        T_DollarArithmetic id _ ->
            err id 2084 "Remove '$' or use '_=$((expr))' to avoid executing output."
        T_DoubleQuoted id [subword] -> check subword
        _ -> return ()
checkSpuriousExpansion _ _ = return ()


prop_checkDollarBrackets1 = verify checkDollarBrackets "echo $[1+2]"
prop_checkDollarBrackets2 = verifyNot checkDollarBrackets "echo $((1+2))"
checkDollarBrackets _ (T_DollarBracket id _) =
    style id 2007 "Use $((..)) instead of deprecated $[..]"
checkDollarBrackets _ _ = return ()

prop_checkSshHereDoc1 = verify checkSshHereDoc "ssh host << foo\necho $PATH\nfoo"
prop_checkSshHereDoc2 = verifyNot checkSshHereDoc "ssh host << 'foo'\necho $PATH\nfoo"
checkSshHereDoc _ (T_Redirecting _ redirs cmd)
        | cmd `isCommand` "ssh" =
    mapM_ checkHereDoc redirs
  where
    hasVariables = mkRegex "[`$]"
    checkHereDoc (T_FdRedirect _ _ (T_HereDoc id _ Unquoted token tokens))
        | not (all isConstant tokens) =
        warn id 2087 $ "Quote '" ++ token ++ "' to make here document expansions happen on the server side rather than on the client."
    checkHereDoc _ = return ()
checkSshHereDoc _ _ = return ()

--- Subshell detection
prop_subshellAssignmentCheck = verifyTree     subshellAssignmentCheck "cat foo | while read bar; do a=$bar; done; echo \"$a\""
prop_subshellAssignmentCheck2 = verifyNotTree subshellAssignmentCheck "while read bar; do a=$bar; done < file; echo \"$a\""
prop_subshellAssignmentCheck3 = verifyTree    subshellAssignmentCheck "( A=foo; ); rm $A"
prop_subshellAssignmentCheck4 = verifyNotTree subshellAssignmentCheck "( A=foo; rm $A; )"
prop_subshellAssignmentCheck5 = verifyTree    subshellAssignmentCheck "cat foo | while read cow; do true; done; echo $cow;"
prop_subshellAssignmentCheck6 = verifyTree    subshellAssignmentCheck "( export lol=$(ls); ); echo $lol;"
prop_subshellAssignmentCheck6a= verifyTree    subshellAssignmentCheck "( typeset -a lol=a; ); echo $lol;"
prop_subshellAssignmentCheck7 = verifyTree    subshellAssignmentCheck "cmd | while read foo; do (( n++ )); done; echo \"$n lines\""
prop_subshellAssignmentCheck8 = verifyTree    subshellAssignmentCheck "n=3 & echo $((n++))"
prop_subshellAssignmentCheck9 = verifyTree    subshellAssignmentCheck "read n & n=foo$n"
prop_subshellAssignmentCheck10 = verifyTree    subshellAssignmentCheck "(( n <<= 3 )) & (( n |= 4 )) &"
prop_subshellAssignmentCheck11 = verifyTree subshellAssignmentCheck "cat /etc/passwd | while read line; do let n=n+1; done\necho $n"
prop_subshellAssignmentCheck12 = verifyTree subshellAssignmentCheck "cat /etc/passwd | while read line; do let ++n; done\necho $n"
prop_subshellAssignmentCheck13 = verifyTree subshellAssignmentCheck "#!/bin/bash\necho foo | read bar; echo $bar"
prop_subshellAssignmentCheck14 = verifyNotTree subshellAssignmentCheck "#!/bin/ksh93\necho foo | read bar; echo $bar"
prop_subshellAssignmentCheck15 = verifyNotTree subshellAssignmentCheck "#!/bin/ksh\ncat foo | while read bar; do a=$bar; done\necho \"$a\""
prop_subshellAssignmentCheck16 = verifyNotTree subshellAssignmentCheck "(set -e); echo $@"
prop_subshellAssignmentCheck17 = verifyNotTree subshellAssignmentCheck "foo=${ { bar=$(baz); } 2>&1; }; echo $foo $bar"
prop_subshellAssignmentCheck18 = verifyTree subshellAssignmentCheck "( exec {n}>&2; ); echo $n"
prop_subshellAssignmentCheck19 = verifyNotTree subshellAssignmentCheck "#!/bin/bash\nshopt -s lastpipe; echo a | read -r b; echo \"$b\""
subshellAssignmentCheck params t =
    let flow = variableFlow params
        check = findSubshelled flow [("oops",[])] Map.empty
    in execWriter check


findSubshelled [] _ _ = return ()
findSubshelled (Assignment x@(_, _, str, _):rest) ((reason,scope):lol) deadVars =
    findSubshelled rest ((reason, x:scope):lol) $ Map.insert str Alive deadVars
findSubshelled (Reference (_, readToken, str):rest) scopes deadVars = do
    unless (shouldIgnore str) $ case Map.findWithDefault Alive str deadVars of
        Alive -> return ()
        Dead writeToken reason -> do
                    info (getId writeToken) 2030 $ "Modification of " ++ str ++ " is local (to subshell caused by "++ reason ++")."
                    info (getId readToken) 2031 $ str ++ " was modified in a subshell. That change might be lost."
    findSubshelled rest scopes deadVars
  where
    shouldIgnore str =
        str `elem` ["@", "*", "IFS"]

findSubshelled (StackScope (SubshellScope reason):rest) scopes deadVars =
    findSubshelled rest ((reason,[]):scopes) deadVars

findSubshelled (StackScopeEnd:rest) ((reason, scope):oldScopes) deadVars =
    findSubshelled rest oldScopes $
        foldl (\m (_, token, var, _) ->
            Map.insert var (Dead token reason) m) deadVars scope


-- FIXME: This is a very strange way of doing it.
-- For each variable read/write, run a stateful function that emits
-- comments. The comments are collected and returned.
doVariableFlowAnalysis ::
    (Token -> Token -> String -> State t [v])
    -> (Token -> Token -> String -> DataType -> State t [v])
    -> t
    -> [StackData]
    -> [v]

doVariableFlowAnalysis readFunc writeFunc empty flow = evalState (
    foldM (\list x -> do { l <- doFlow x;  return $ l ++ list; }) [] flow
    ) empty
  where
    doFlow (Reference (base, token, name)) =
        readFunc base token name
    doFlow (Assignment (base, token, name, values)) =
        writeFunc base token name values
    doFlow _ = return []

---- Check whether variables could have spaces/globs
prop_checkSpacefulness1 = verifyTree checkSpacefulness "a='cow moo'; echo $a"
prop_checkSpacefulness2 = verifyNotTree checkSpacefulness "a='cow moo'; [[ $a ]]"
prop_checkSpacefulness3 = verifyNotTree checkSpacefulness "a='cow*.mp3'; echo \"$a\""
prop_checkSpacefulness4 = verifyTree checkSpacefulness "for f in *.mp3; do echo $f; done"
prop_checkSpacefulness4a= verifyNotTree checkSpacefulness "foo=3; foo=$(echo $foo)"
prop_checkSpacefulness5 = verifyTree checkSpacefulness "a='*'; b=$a; c=lol${b//foo/bar}; echo $c"
prop_checkSpacefulness6 = verifyTree checkSpacefulness "a=foo$(lol); echo $a"
prop_checkSpacefulness7 = verifyTree checkSpacefulness "a=foo\\ bar; rm $a"
prop_checkSpacefulness8 = verifyNotTree checkSpacefulness "a=foo\\ bar; a=foo; rm $a"
prop_checkSpacefulness10= verifyTree checkSpacefulness "rm $1"
prop_checkSpacefulness11= verifyTree checkSpacefulness "rm ${10//foo/bar}"
prop_checkSpacefulness12= verifyNotTree checkSpacefulness "(( $1 + 3 ))"
prop_checkSpacefulness13= verifyNotTree checkSpacefulness "if [[ $2 -gt 14 ]]; then true; fi"
prop_checkSpacefulness14= verifyNotTree checkSpacefulness "foo=$3 env"
prop_checkSpacefulness15= verifyNotTree checkSpacefulness "local foo=$1"
prop_checkSpacefulness16= verifyNotTree checkSpacefulness "declare foo=$1"
prop_checkSpacefulness17= verifyTree checkSpacefulness "echo foo=$1"
prop_checkSpacefulness18= verifyNotTree checkSpacefulness "$1 --flags"
prop_checkSpacefulness19= verifyTree checkSpacefulness "echo $PWD"
prop_checkSpacefulness20= verifyNotTree checkSpacefulness "n+='foo bar'"
prop_checkSpacefulness21= verifyNotTree checkSpacefulness "select foo in $bar; do true; done"
prop_checkSpacefulness22= verifyNotTree checkSpacefulness "echo $\"$1\""
prop_checkSpacefulness23= verifyNotTree checkSpacefulness "a=(1); echo ${a[@]}"
prop_checkSpacefulness24= verifyTree checkSpacefulness "a='a    b'; cat <<< $a"
prop_checkSpacefulness25= verifyTree checkSpacefulness "a='s/[0-9]//g'; sed $a"
prop_checkSpacefulness26= verifyTree checkSpacefulness "a='foo bar'; echo {1,2,$a}"
prop_checkSpacefulness27= verifyNotTree checkSpacefulness "echo ${a:+'foo'}"
prop_checkSpacefulness28= verifyNotTree checkSpacefulness "exec {n}>&1; echo $n"
prop_checkSpacefulness29= verifyNotTree checkSpacefulness "n=$(stuff); exec {n}>&-;"
prop_checkSpacefulness30= verifyTree checkSpacefulness "file='foo bar'; echo foo > $file;"
prop_checkSpacefulness31= verifyNotTree checkSpacefulness "echo \"`echo \\\"$1\\\"`\""
prop_checkSpacefulness32= verifyNotTree checkSpacefulness "var=$1; [ -v var ]"
prop_checkSpacefulness33= verifyTree checkSpacefulness "for file; do echo $file; done"
prop_checkSpacefulness34= verifyTree checkSpacefulness "declare foo$n=$1"
prop_checkSpacefulness35= verifyNotTree checkSpacefulness "echo ${1+\"$1\"}"

checkSpacefulness params t =
    doVariableFlowAnalysis readF writeF (Map.fromList defaults) (variableFlow params)
  where
    defaults = zip variablesWithoutSpaces (repeat False)

    hasSpaces name = do
        map <- get
        return $ Map.findWithDefault True name map

    setSpaces name bool =
        modify $ Map.insert name bool

    readF _ token name = do
        spaces <- hasSpaces name
        return [warning |
                  isExpansion token && spaces
                  && not (isArrayExpansion token) -- There's another warning for this
                  && not (isCountingReference token)
                  && not (isQuoteFree parents token)
                  && not (isQuotedAlternativeReference token)
                  && not (usedAsCommandName parents token)]
      where
        warning =
            if isDefaultAssignment (parentMap params) token
            then
                makeComment InfoC (getId token) 2223
                    "This default assignment may cause DoS due to globbing. Quote it."
            else
                makeComment InfoC (getId token) 2086
                    "Double quote to prevent globbing and word splitting."

    writeF _ _ name (DataString SourceExternal) = setSpaces name True >> return []
    writeF _ _ name (DataString SourceInteger) = setSpaces name False >> return []

    writeF _ _ name (DataString (SourceFrom vals)) = do
        map <- get
        setSpaces name
            (isSpacefulWord (\x -> Map.findWithDefault True x map) vals)
        return []

    writeF _ _ _ _ = return []

    parents = parentMap params

    isExpansion t =
        case t of
            (T_DollarBraced _ _ ) -> True
            _ -> False

    isSpacefulWord :: (String -> Bool) -> [Token] -> Bool
    isSpacefulWord f = any (isSpaceful f)
    isSpaceful :: (String -> Bool) -> Token -> Bool
    isSpaceful spacefulF x =
        case x of
          T_DollarExpansion _ _ -> True
          T_Backticked _ _ -> True
          T_Glob _ _         -> True
          T_Extglob {}       -> True
          T_Literal _ s      -> s `containsAny` globspace
          T_SingleQuoted _ s -> s `containsAny` globspace
          T_DollarBraced _ _ -> spacefulF $ getBracedReference $ bracedString x
          T_NormalWord _ w   -> isSpacefulWord spacefulF w
          T_DoubleQuoted _ w -> isSpacefulWord spacefulF w
          _ -> False
      where
        globspace = "*?[] \t\n"
        containsAny s = any (`elem` s)

    isDefaultAssignment parents token =
        let modifier = getBracedModifier $ bracedString token in
            isExpansion token
            && any (`isPrefixOf` modifier) ["=", ":="]
            && isParamTo parents ":" token

prop_checkQuotesInLiterals1 = verifyTree checkQuotesInLiterals "param='--foo=\"bar\"'; app $param"
prop_checkQuotesInLiterals1a= verifyTree checkQuotesInLiterals "param=\"--foo='lolbar'\"; app $param"
prop_checkQuotesInLiterals2 = verifyNotTree checkQuotesInLiterals "param='--foo=\"bar\"'; app \"$param\""
prop_checkQuotesInLiterals3 =verifyNotTree checkQuotesInLiterals "param=('--foo='); app \"${param[@]}\""
prop_checkQuotesInLiterals4 = verifyNotTree checkQuotesInLiterals "param=\"don't bother with this one\"; app $param"
prop_checkQuotesInLiterals5 = verifyNotTree checkQuotesInLiterals "param=\"--foo='lolbar'\"; eval app $param"
prop_checkQuotesInLiterals6 = verifyTree checkQuotesInLiterals "param='my\\ file'; cmd=\"rm $param\"; $cmd"
prop_checkQuotesInLiterals6a= verifyNotTree checkQuotesInLiterals "param='my\\ file'; cmd=\"rm ${#param}\"; $cmd"
prop_checkQuotesInLiterals7 = verifyTree checkQuotesInLiterals "param='my\\ file'; rm $param"
prop_checkQuotesInLiterals8 = verifyTree checkQuotesInLiterals "param=\"/foo/'bar baz'/etc\"; rm $param"
prop_checkQuotesInLiterals9 = verifyNotTree checkQuotesInLiterals "param=\"/foo/'bar baz'/etc\"; rm ${#param}"
checkQuotesInLiterals params t =
    doVariableFlowAnalysis readF writeF Map.empty (variableFlow params)
  where
    getQuotes name = fmap (Map.lookup name) get
    setQuotes name ref = modify $ Map.insert name ref
    deleteQuotes = modify . Map.delete
    parents = parentMap params
    quoteRegex = mkRegex "\"|([/= ]|^)'|'( |$)|\\\\ "
    containsQuotes s = s `matches` quoteRegex

    writeF _ _ name (DataString (SourceFrom values)) = do
        quoteMap <- get
        let quotedVars = msum $ map (forToken quoteMap) values
        case quotedVars of
            Nothing -> deleteQuotes name
            Just x -> setQuotes name x
        return []
    writeF _ _ _ _ = return []

    forToken map (T_DollarBraced id t) =
        -- skip getBracedReference here to avoid false positives on PE
        Map.lookup (concat . oversimplify $ t) map
    forToken quoteMap (T_DoubleQuoted id tokens) =
        msum $ map (forToken quoteMap) tokens
    forToken quoteMap (T_NormalWord id tokens) =
        msum $ map (forToken quoteMap) tokens
    forToken _ t =
        if containsQuotes (concat $ oversimplify t)
        then return $ getId t
        else Nothing

    squashesQuotes t =
        case t of
            T_DollarBraced id _ -> "#" `isPrefixOf` bracedString t
            _ -> False

    readF _ expr name = do
        assignment <- getQuotes name
        return
          (if isJust assignment
              && not (isParamTo parents "eval" expr)
              && not (isQuoteFree parents expr)
              && not (squashesQuotes expr)
              then [
                  makeComment WarningC (fromJust assignment) 2089
                      "Quotes/backslashes will be treated literally. Use an array.",
                  makeComment WarningC (getId expr) 2090
                      "Quotes/backslashes in this variable will not be respected."
                ]
              else [])


prop_checkFunctionsUsedExternally1 =
  verifyTree checkFunctionsUsedExternally "foo() { :; }; sudo foo"
prop_checkFunctionsUsedExternally2 =
  verifyTree checkFunctionsUsedExternally "alias f='a'; xargs -n 1 f"
prop_checkFunctionsUsedExternally3 =
  verifyNotTree checkFunctionsUsedExternally "f() { :; }; echo f"
prop_checkFunctionsUsedExternally4 =
  verifyNotTree checkFunctionsUsedExternally "foo() { :; }; sudo \"foo\""
checkFunctionsUsedExternally params t =
    runNodeAnalysis checkCommand params t
  where
    invokingCmds = [
        "chroot",
        "find",
        "screen",
        "ssh",
        "su",
        "sudo",
        "xargs"
        ]
    checkCommand _ t@(T_SimpleCommand _ _ (cmd:args)) =
        let name = fromMaybe "" $ getCommandBasename t in
          when (name `elem` invokingCmds) $
            mapM_ (checkArg name) args
    checkCommand _ _ = return ()

    analyse f t = execState (doAnalysis f t) []
    functions = Map.fromList $ analyse findFunctions t
    findFunctions (T_Function id _ _ name _) = modify ((name, id):)
    findFunctions t@(T_SimpleCommand id _ (_:args))
        | t `isUnqualifiedCommand` "alias" = mapM_ getAlias args
    findFunctions _ = return ()
    getAlias arg =
        let string = concat $ oversimplify arg
        in when ('=' `elem` string) $
            modify ((takeWhile (/= '=') string, getId arg):)
    checkArg cmd arg = potentially $ do
        literalArg <- getUnquotedLiteral arg  -- only consider unquoted literals
        definitionId <- Map.lookup literalArg functions
        return $ do
            warn (getId arg) 2033
              "Shell functions can't be passed to external commands."
            info definitionId 2032 $
              "Use own script or sh -c '..' to run this from " ++ cmd ++ "."

prop_checkUnused0 = verifyNotTree checkUnusedAssignments "var=foo; echo $var"
prop_checkUnused1 = verifyTree checkUnusedAssignments "var=foo; echo $bar"
prop_checkUnused2 = verifyNotTree checkUnusedAssignments "var=foo; export var;"
prop_checkUnused3 = verifyTree checkUnusedAssignments "for f in *; do echo '$f'; done"
prop_checkUnused4 = verifyTree checkUnusedAssignments "local i=0"
prop_checkUnused5 = verifyNotTree checkUnusedAssignments "read lol; echo $lol"
prop_checkUnused6 = verifyNotTree checkUnusedAssignments "var=4; (( var++ ))"
prop_checkUnused7 = verifyNotTree checkUnusedAssignments "var=2; $((var))"
prop_checkUnused8 = verifyTree checkUnusedAssignments "var=2; var=3;"
prop_checkUnused9 = verifyNotTree checkUnusedAssignments "read ''"
prop_checkUnused10= verifyNotTree checkUnusedAssignments "read -p 'test: '"
prop_checkUnused11= verifyNotTree checkUnusedAssignments "bar=5; export foo[$bar]=3"
prop_checkUnused12= verifyNotTree checkUnusedAssignments "read foo; echo ${!foo}"
prop_checkUnused13= verifyNotTree checkUnusedAssignments "x=(1); (( x[0] ))"
prop_checkUnused14= verifyNotTree checkUnusedAssignments "x=(1); n=0; echo ${x[n]}"
prop_checkUnused15= verifyNotTree checkUnusedAssignments "x=(1); n=0; (( x[n] ))"
prop_checkUnused16= verifyNotTree checkUnusedAssignments "foo=5; declare -x foo"
prop_checkUnused17= verifyNotTree checkUnusedAssignments "read -i 'foo' -e -p 'Input: ' bar; $bar;"
prop_checkUnused18= verifyNotTree checkUnusedAssignments "a=1; arr=( [$a]=42 ); echo \"${arr[@]}\""
prop_checkUnused19= verifyNotTree checkUnusedAssignments "a=1; let b=a+1; echo $b"
prop_checkUnused20= verifyNotTree checkUnusedAssignments "a=1; PS1='$a'"
prop_checkUnused21= verifyNotTree checkUnusedAssignments "a=1; trap 'echo $a' INT"
prop_checkUnused22= verifyNotTree checkUnusedAssignments "a=1; [ -v a ]"
prop_checkUnused23= verifyNotTree checkUnusedAssignments "a=1; [ -R a ]"
prop_checkUnused24= verifyNotTree checkUnusedAssignments "mapfile -C a b; echo ${b[@]}"
prop_checkUnused25= verifyNotTree checkUnusedAssignments "readarray foo; echo ${foo[@]}"
prop_checkUnused26= verifyNotTree checkUnusedAssignments "declare -F foo"
prop_checkUnused27= verifyTree checkUnusedAssignments "var=3; [ var -eq 3 ]"
prop_checkUnused28= verifyNotTree checkUnusedAssignments "var=3; [[ var -eq 3 ]]"
prop_checkUnused29= verifyNotTree checkUnusedAssignments "var=(a b); declare -p var"
prop_checkUnused30= verifyTree checkUnusedAssignments "let a=1"
prop_checkUnused31= verifyTree checkUnusedAssignments "let 'a=1'"
prop_checkUnused32= verifyTree checkUnusedAssignments "let a=b=c; echo $a"
prop_checkUnused33= verifyNotTree checkUnusedAssignments "a=foo; [[ foo =~ ^{$a}$ ]]"
prop_checkUnused34= verifyNotTree checkUnusedAssignments "foo=1; (( t = foo )); echo $t"
prop_checkUnused35= verifyNotTree checkUnusedAssignments "a=foo; b=2; echo ${a:b}"
prop_checkUnused36= verifyNotTree checkUnusedAssignments "if [[ -v foo ]]; then true; fi"
prop_checkUnused37= verifyNotTree checkUnusedAssignments "fd=2; exec {fd}>&-"
checkUnusedAssignments params t = execWriter (mapM_ warnFor unused)
  where
    flow = variableFlow params
    references = foldl (flip ($)) defaultMap (map insertRef flow)
    insertRef (Reference (base, token, name)) =
        Map.insert (stripSuffix name) ()
    insertRef _ = id

    assignments = foldl (flip ($)) Map.empty (map insertAssignment flow)
    insertAssignment (Assignment (_, token, name, _)) | isVariableName name =
        Map.insert name token
    insertAssignment _ = id

    unused = Map.assocs $ Map.difference assignments references

    warnFor (name, token) =
        warn (getId token) 2034 $
            name ++ " appears unused. Verify it or export it."

    stripSuffix = takeWhile isVariableChar
    defaultMap = Map.fromList $ zip internalVariables $ repeat ()

prop_checkUnassignedReferences1 = verifyTree checkUnassignedReferences "echo $foo"
prop_checkUnassignedReferences2 = verifyNotTree checkUnassignedReferences "foo=hello; echo $foo"
prop_checkUnassignedReferences3 = verifyTree checkUnassignedReferences "MY_VALUE=3; echo $MYVALUE"
prop_checkUnassignedReferences4 = verifyNotTree checkUnassignedReferences "RANDOM2=foo; echo $RANDOM"
prop_checkUnassignedReferences5 = verifyNotTree checkUnassignedReferences "declare -A foo=([bar]=baz); echo ${foo[bar]}"
prop_checkUnassignedReferences6 = verifyNotTree checkUnassignedReferences "foo=..; echo ${foo-bar}"
prop_checkUnassignedReferences7 = verifyNotTree checkUnassignedReferences "getopts ':h' foo; echo $foo"
prop_checkUnassignedReferences8 = verifyNotTree checkUnassignedReferences "let 'foo = 1'; echo $foo"
prop_checkUnassignedReferences9 = verifyNotTree checkUnassignedReferences "echo ${foo-bar}"
prop_checkUnassignedReferences10= verifyNotTree checkUnassignedReferences "echo ${foo:?}"
prop_checkUnassignedReferences11= verifyNotTree checkUnassignedReferences "declare -A foo; echo \"${foo[@]}\""
prop_checkUnassignedReferences12= verifyNotTree checkUnassignedReferences "typeset -a foo; echo \"${foo[@]}\""
prop_checkUnassignedReferences13= verifyNotTree checkUnassignedReferences "f() { local foo; echo $foo; }"
prop_checkUnassignedReferences14= verifyNotTree checkUnassignedReferences "foo=; echo $foo"
prop_checkUnassignedReferences15= verifyNotTree checkUnassignedReferences "f() { true; }; export -f f"
prop_checkUnassignedReferences16= verifyNotTree checkUnassignedReferences "declare -A foo=( [a b]=bar ); echo ${foo[a b]}"
prop_checkUnassignedReferences17= verifyNotTree checkUnassignedReferences "USERS=foo; echo $USER"
prop_checkUnassignedReferences18= verifyNotTree checkUnassignedReferences "FOOBAR=42; export FOOBAR="
prop_checkUnassignedReferences19= verifyNotTree checkUnassignedReferences "readonly foo=bar; echo $foo"
prop_checkUnassignedReferences20= verifyNotTree checkUnassignedReferences "printf -v foo bar; echo $foo"
prop_checkUnassignedReferences21= verifyTree checkUnassignedReferences "echo ${#foo}"
prop_checkUnassignedReferences22= verifyNotTree checkUnassignedReferences "echo ${!os*}"
prop_checkUnassignedReferences23= verifyTree checkUnassignedReferences "declare -a foo; foo[bar]=42;"
prop_checkUnassignedReferences24= verifyNotTree checkUnassignedReferences "declare -A foo; foo[bar]=42;"
prop_checkUnassignedReferences25= verifyNotTree checkUnassignedReferences "declare -A foo=(); foo[bar]=42;"
prop_checkUnassignedReferences26= verifyNotTree checkUnassignedReferences "a::b() { foo; }; readonly -f a::b"
prop_checkUnassignedReferences27= verifyNotTree checkUnassignedReferences ": ${foo:=bar}"
prop_checkUnassignedReferences28= verifyNotTree checkUnassignedReferences "#!/bin/ksh\necho \"${.sh.version}\"\n"
prop_checkUnassignedReferences29= verifyNotTree checkUnassignedReferences "if [[ -v foo ]]; then echo $foo; fi"
prop_checkUnassignedReferences30= verifyNotTree checkUnassignedReferences "if [[ -v foo[3] ]]; then echo ${foo[3]}; fi"
prop_checkUnassignedReferences31= verifyNotTree checkUnassignedReferences "X=1; if [[ -v foo[$X+42] ]]; then echo ${foo[$X+42]}; fi"
prop_checkUnassignedReferences32= verifyNotTree checkUnassignedReferences "if [[ -v \"foo[1]\" ]]; then echo ${foo[@]}; fi"
checkUnassignedReferences params t = warnings
  where
    (readMap, writeMap) = execState (mapM tally $ variableFlow params) (Map.empty, Map.empty)
    defaultAssigned = Map.fromList $ map (\a -> (a, ())) $ filter (not . null) internalVariables

    tally (Assignment (_, _, name, _))  =
        modify (\(read, written) -> (read, Map.insert name () written))
    tally (Reference (_, place, name)) =
        modify (\(read, written) -> (Map.insertWith (const id) name place read, written))
    tally _ = return ()

    unassigned = Map.toList $ Map.difference (Map.difference readMap writeMap) defaultAssigned
    writtenVars = filter isVariableName $ Map.keys writeMap

    getBestMatch var = do
        (match, score) <- listToMaybe best
        guard $ goodMatch var match score
        return match
      where
        matches = map (\x -> (x, match var x)) writtenVars
        best = sortBy (comparing snd) matches
        goodMatch var match score =
            let l = length match in
                l > 3 && score <= 1
                || l > 7 && score <= 2

    isLocal = any isLower

    warningForGlobals var place = do
        match <- getBestMatch var
        return $ warn (getId place) 2153 $
            "Possible misspelling: " ++ var ++ " may not be assigned, but " ++ match ++ " is."

    warningForLocals var place =
        return $ warn (getId place) 2154 $
            var ++ " is referenced but not assigned" ++ optionalTip ++ "."
      where
        optionalTip =
            if var `elem` commonCommands
            then " (for output from commands, use \"$(" ++ var ++ " ..." ++ ")\" )"
            else fromMaybe "" $ do
                    match <- getBestMatch var
                    return $ " (did you mean '" ++ match ++ "'?)"

    warningFor var place = do
        guard . not $ isInArray var place || isGuarded place
        (if isLocal var then warningForLocals else warningForGlobals) var place

    warnings = execWriter . sequence $ mapMaybe (uncurry warningFor) unassigned

    -- Due to parsing, foo=( [bar]=baz ) parses 'bar' as a reference even for assoc arrays.
    -- Similarly, ${foo[bar baz]} may not be referencing bar/baz. Just skip these.
    isInArray var t = any isArray $ getPath (parentMap params) t
      where
        isArray T_Array {} = True
        isArray b@(T_DollarBraced _ _) | var /= getBracedReference (bracedString b) = True
        isArray _ = False

    isGuarded (T_DollarBraced _ v) =
        any (`isPrefixOf` rest) ["-", ":-", "?", ":?"]
      where
        name = concat $ oversimplify v
        rest = dropWhile isVariableChar $ dropWhile (`elem` "#!") name
    isGuarded _ = False

    match var candidate =
        if var /= candidate && map toLower var == map toLower candidate
        then 1
        else dist var candidate


prop_checkGlobsAsOptions1 = verify checkGlobsAsOptions "rm *.txt"
prop_checkGlobsAsOptions2 = verify checkGlobsAsOptions "ls ??.*"
prop_checkGlobsAsOptions3 = verifyNot checkGlobsAsOptions "rm -- *.txt"
prop_checkGlobsAsOptions4 = verifyNot checkGlobsAsOptions "*.txt"
checkGlobsAsOptions _ (T_SimpleCommand _ _ args) =
    mapM_ check $ takeWhile (not . isEndOfArgs) (drop 1 args)
  where
    check v@(T_NormalWord _ (T_Glob id s:_)) | s == "*" || s == "?" =
        info id 2035 "Use ./*glob* or -- *glob* so names with dashes won't become options."
    check _ = return ()

    isEndOfArgs t =
        case concat $ oversimplify t of
            "--" -> True
            ":::" -> True
            "::::" -> True
            _ -> False

checkGlobsAsOptions _ _ = return ()


prop_checkWhileReadPitfalls1 = verify checkWhileReadPitfalls "while read foo; do ssh $foo uptime; done < file"
prop_checkWhileReadPitfalls2 = verifyNot checkWhileReadPitfalls "while read -u 3 foo; do ssh $foo uptime; done 3< file"
prop_checkWhileReadPitfalls3 = verifyNot checkWhileReadPitfalls "while true; do ssh host uptime; done"
prop_checkWhileReadPitfalls4 = verifyNot checkWhileReadPitfalls "while read foo; do ssh $foo hostname < /dev/null; done"
prop_checkWhileReadPitfalls5 = verifyNot checkWhileReadPitfalls "while read foo; do echo ls | ssh $foo; done"
prop_checkWhileReadPitfalls6 = verifyNot checkWhileReadPitfalls "while read foo <&3; do ssh $foo; done 3< foo"
prop_checkWhileReadPitfalls7 = verify checkWhileReadPitfalls "while read foo; do if true; then ssh $foo uptime; fi; done < file"
prop_checkWhileReadPitfalls8 = verifyNot checkWhileReadPitfalls "while read foo; do ssh -n $foo uptime; done < file"

checkWhileReadPitfalls _ (T_WhileExpression id [command] contents)
        | isStdinReadCommand command =
    mapM_ checkMuncher contents
  where
    munchers = [ "ssh", "ffmpeg", "mplayer", "HandBrakeCLI" ]
    preventionFlags = ["n", "noconsolecontrols" ]

    isStdinReadCommand (T_Pipeline _ _ [T_Redirecting id redirs cmd]) =
        let plaintext = oversimplify cmd
        in head (plaintext ++ [""]) == "read"
            && ("-u" `notElem` plaintext)
            && all (not . stdinRedirect) redirs
    isStdinReadCommand _ = False

    checkMuncher (T_Pipeline _ _ (T_Redirecting _ redirs cmd:_)) | not $ any stdinRedirect redirs =
        case cmd of
            (T_IfExpression _ thens elses) ->
              mapM_ checkMuncher . concat $ map fst thens ++ map snd thens ++ [elses]

            _ -> potentially $ do
                name <- getCommandBasename cmd
                guard $ name `elem` munchers

                -- Sloppily check if the command has a flag to prevent eating stdin.
                let flags = getAllFlags cmd
                guard . not $ any (`elem` preventionFlags) $ map snd flags
                return $ do
                    info id 2095 $
                        name ++ " may swallow stdin, preventing this loop from working properly."
                    warn (getId cmd) 2095 $
                        "Add < /dev/null to prevent " ++ name ++ " from swallowing stdin."
    checkMuncher _ = return ()

    stdinRedirect (T_FdRedirect _ fd _)
        | fd == "" || fd == "0" = True
    stdinRedirect _ = False
checkWhileReadPitfalls _ _ = return ()


prop_checkPrefixAssign1 = verify checkPrefixAssignmentReference "var=foo echo $var"
prop_checkPrefixAssign2 = verifyNot checkPrefixAssignmentReference "var=$(echo $var) cmd"
checkPrefixAssignmentReference params t@(T_DollarBraced id value) =
    check path
  where
    name = getBracedReference $ bracedString t
    path = getPath (parentMap params) t
    idPath = map getId path

    check [] = return ()
    check (t:rest) =
        case t of
            T_SimpleCommand _ vars (_:_) -> mapM_ checkVar vars
            _ -> check rest
    checkVar (T_Assignment aId mode aName [] value) |
            aName == name && (aId `notElem` idPath) = do
        warn aId 2097 "This assignment is only seen by the forked process."
        warn id 2098 "This expansion will not see the mentioned assignment."
    checkVar _ = return ()

checkPrefixAssignmentReference _ _ = return ()

prop_checkCharRangeGlob1 = verify checkCharRangeGlob "ls *[:digit:].jpg"
prop_checkCharRangeGlob2 = verifyNot checkCharRangeGlob "ls *[[:digit:]].jpg"
prop_checkCharRangeGlob3 = verify checkCharRangeGlob "ls [10-15]"
prop_checkCharRangeGlob4 = verifyNot checkCharRangeGlob "ls [a-zA-Z]"
prop_checkCharRangeGlob5 = verifyNot checkCharRangeGlob "tr -d [a-zA-Z]" -- tr has 2060
checkCharRangeGlob p t@(T_Glob id str) |
  isCharClass str && not (isParamTo (parentMap p) "tr" t) =
    if ":" `isPrefixOf` contents
        && ":" `isSuffixOf` contents
        && contents /= ":"
    then warn id 2101 "Named class needs outer [], e.g. [[:digit:]]."
    else
        when ('[' `notElem` contents && hasDupes) $
            info id 2102 "Ranges can only match single chars (mentioned due to duplicates)."
  where
    isCharClass str = "[" `isPrefixOf` str && "]" `isSuffixOf` str
    contents = drop 1 . take (length str - 1) $ str
    hasDupes = any (>1) . map length . group . sort . filter (/= '-') $ contents
checkCharRangeGlob _ _ = return ()



prop_checkCdAndBack1 = verify checkCdAndBack "for f in *; do cd $f; git pull; cd ..; done"
prop_checkCdAndBack2 = verifyNot checkCdAndBack "for f in *; do cd $f || continue; git pull; cd ..; done"
prop_checkCdAndBack3 = verifyNot checkCdAndBack "while [[ $PWD != / ]]; do cd ..; done"
prop_checkCdAndBack4 = verify checkCdAndBack "cd $tmp; foo; cd -"
checkCdAndBack params = doLists
  where
    shell = shellType params
    doLists (T_ForIn _ _ _ cmds) = doList cmds
    doLists (T_ForArithmetic _ _ _ _ cmds) = doList cmds
    doLists (T_WhileExpression _ _ cmds) = doList cmds
    doLists (T_UntilExpression _ _ cmds) = doList cmds
    doLists (T_Script _ _ cmds) = doList cmds
    doLists (T_IfExpression _ thens elses) = do
        mapM_ (\(_, l) -> doList l) thens
        doList elses
    doLists _ = return ()

    isCdRevert t =
        case oversimplify t of
            ["cd", p] -> p `elem` ["..", "-"]
            _ -> False

    getCmd (T_Annotation id _ x) = getCmd x
    getCmd (T_Pipeline id _ [x]) = getCommandName x
    getCmd _ = Nothing

    doList list =
        let cds = filter ((== Just "cd") . getCmd) list in
            when (length cds >= 2 && isCdRevert (last cds)) $
               info (getId $ last cds) 2103 message

    message = "Use a ( subshell ) to avoid having to cd back."

prop_checkLoopKeywordScope1 = verify checkLoopKeywordScope "continue 2"
prop_checkLoopKeywordScope2 = verify checkLoopKeywordScope "for f; do ( break; ); done"
prop_checkLoopKeywordScope3 = verify checkLoopKeywordScope "if true; then continue; fi"
prop_checkLoopKeywordScope4 = verifyNot checkLoopKeywordScope "while true; do break; done"
prop_checkLoopKeywordScope5 = verify checkLoopKeywordScope "if true; then break; fi"
prop_checkLoopKeywordScope6 = verify checkLoopKeywordScope "while true; do true | { break; }; done"
prop_checkLoopKeywordScope7 = verifyNot checkLoopKeywordScope "#!/bin/ksh\nwhile true; do true | { break; }; done"
checkLoopKeywordScope params t |
        name `elem` map Just ["continue", "break"] =
    if not $ any isLoop path
    then if any isFunction $ take 1 path
        -- breaking at a source/function invocation is an abomination. Let's ignore it.
        then err (getId t) 2104 $ "In functions, use return instead of " ++ fromJust name ++ "."
        else err (getId t) 2105 $ fromJust name ++ " is only valid in loops."
    else case map subshellType $ filter (not . isFunction) path of
        Just str:_ -> warn (getId t) 2106 $
            "This only exits the subshell caused by the " ++ str ++ "."
        _ -> return ()
  where
    name = getCommandName t
    path = let p = getPath (parentMap params) t in filter relevant p
    subshellType t = case leadType params t of
        NoneScope -> Nothing
        SubshellScope str -> return str
    relevant t = isLoop t || isFunction t || isJust (subshellType t)
checkLoopKeywordScope _ _ = return ()


prop_checkFunctionDeclarations1 = verify checkFunctionDeclarations "#!/bin/ksh\nfunction foo() { command foo --lol \"$@\"; }"
prop_checkFunctionDeclarations2 = verify checkFunctionDeclarations "#!/bin/dash\nfunction foo { lol; }"
prop_checkFunctionDeclarations3 = verifyNot checkFunctionDeclarations "foo() { echo bar; }"
checkFunctionDeclarations params
        (T_Function id (FunctionKeyword hasKeyword) (FunctionParentheses hasParens) _ _) =
    case shellType params of
        Bash -> return ()
        Ksh ->
            when (hasKeyword && hasParens) $
                err id 2111 "ksh does not allow 'function' keyword and '()' at the same time."
        Dash -> forSh
        Sh   -> forSh

    where
        forSh = do
            when (hasKeyword && hasParens) $
                warn id 2112 "'function' keyword is non-standard. Delete it."
            when (hasKeyword && not hasParens) $
                warn id 2113 "'function' keyword is non-standard. Use 'foo()' instead of 'function foo'."
checkFunctionDeclarations _ _ = return ()



prop_checkStderrPipe1 = verify checkStderrPipe "#!/bin/ksh\nfoo |& bar"
prop_checkStderrPipe2 = verifyNot checkStderrPipe "#!/bin/bash\nfoo |& bar"
checkStderrPipe params =
    case shellType params of
        Ksh -> match
        _ -> const $ return ()
  where
    match (T_Pipe id "|&") =
        err id 2118 "Ksh does not support |&. Use 2>&1 |."
    match _ = return ()

prop_checkUnpassedInFunctions1 = verifyTree checkUnpassedInFunctions "foo() { echo $1; }; foo"
prop_checkUnpassedInFunctions2 = verifyNotTree checkUnpassedInFunctions "foo() { echo $1; };"
prop_checkUnpassedInFunctions3 = verifyNotTree checkUnpassedInFunctions "foo() { echo $lol; }; foo"
prop_checkUnpassedInFunctions4 = verifyNotTree checkUnpassedInFunctions "foo() { echo $0; }; foo"
prop_checkUnpassedInFunctions5 = verifyNotTree checkUnpassedInFunctions "foo() { echo $1; }; foo 'lol'; foo"
prop_checkUnpassedInFunctions6 = verifyNotTree checkUnpassedInFunctions "foo() { set -- *; echo $1; }; foo"
prop_checkUnpassedInFunctions7 = verifyTree checkUnpassedInFunctions "foo() { echo $1; }; foo; foo;"
prop_checkUnpassedInFunctions8 = verifyNotTree checkUnpassedInFunctions "foo() { echo $((1)); }; foo;"
prop_checkUnpassedInFunctions9 = verifyNotTree checkUnpassedInFunctions "foo() { echo $(($b)); }; foo;"
prop_checkUnpassedInFunctions10= verifyNotTree checkUnpassedInFunctions "foo() { echo $!; }; foo;"
prop_checkUnpassedInFunctions11= verifyNotTree checkUnpassedInFunctions "foo() { bar() { echo $1; }; bar baz; }; foo;"
prop_checkUnpassedInFunctions12= verifyNotTree checkUnpassedInFunctions "foo() { echo ${!var*}; }; foo;"
checkUnpassedInFunctions params root =
    execWriter $ mapM_ warnForGroup referenceGroups
  where
    functionMap :: Map.Map String Token
    functionMap = Map.fromList $
        map (\t@(T_Function _ _ _ name _) -> (name,t)) functions
    functions = execWriter $ doAnalysis (tell . maybeToList . findFunction) root

    findFunction t@(T_Function id _ _ name body) =
        let flow = getVariableFlow params body
        in
          if any (isPositionalReference t) flow && not (any isPositionalAssignment flow)
            then return t
            else Nothing
    findFunction _ = Nothing

    isPositionalAssignment x =
        case x of
            Assignment (_, _, str, _) -> isPositional str
            _ -> False
    isPositionalReference function x =
        case x of
            Reference (_, t, str) -> isPositional str && t `isDirectChildOf` function
            _ -> False

    isDirectChildOf child parent = fromMaybe False $ do
        function <- find (\x -> case x of
            T_Function {} -> True
            T_Script {} -> True  -- for sourced files
            _ -> False) $
                getPath (parentMap params) child
        return $ getId parent == getId function

    referenceList :: [(String, Bool, Token)]
    referenceList = execWriter $
        doAnalysis (fromMaybe (return ()) . checkCommand) root
    checkCommand :: Token -> Maybe (Writer [(String, Bool, Token)] ())
    checkCommand t@(T_SimpleCommand _ _ (cmd:args)) = do
        str <- getLiteralString cmd
        guard $ Map.member str functionMap
        return $ tell [(str, null args, t)]
    checkCommand _ = Nothing

    isPositional str = str == "*" || str == "@"
        || (all isDigit str && str /= "0" && str /= "")

    isArgumentless (_, b, _) = b
    referenceGroups = Map.elems $ foldr updateWith Map.empty referenceList
    updateWith x@(name, _, _) = Map.insertWith (++) name [x]

    warnForGroup group =
        when (all isArgumentless group) $ do
            mapM_ suggestParams group
            warnForDeclaration group

    suggestParams (name, _, thing) =
        info (getId thing) 2119 $
            "Use " ++ name ++ " \"$@\" if function's $1 should mean script's $1."
    warnForDeclaration ((name, _, _):_) =
        warn (getId . fromJust $ Map.lookup name functionMap) 2120 $
            name ++ " references arguments, but none are ever passed."


prop_checkOverridingPath1 = verify checkOverridingPath "PATH=\"$var/$foo\""
prop_checkOverridingPath2 = verify checkOverridingPath "PATH=\"mydir\""
prop_checkOverridingPath3 = verify checkOverridingPath "PATH=/cow/foo"
prop_checkOverridingPath4 = verifyNot checkOverridingPath "PATH=/cow/foo/bin"
prop_checkOverridingPath5 = verifyNot checkOverridingPath "PATH='/bin:/sbin'"
prop_checkOverridingPath6 = verifyNot checkOverridingPath "PATH=\"$var/$foo\" cmd"
prop_checkOverridingPath7 = verifyNot checkOverridingPath "PATH=$OLDPATH"
prop_checkOverridingPath8 = verifyNot checkOverridingPath "PATH=$PATH:/stuff"
checkOverridingPath _ (T_SimpleCommand _ vars []) =
    mapM_ checkVar vars
  where
    checkVar (T_Assignment id Assign "PATH" [] word) =
        let string = concat $ oversimplify word
        in unless (any (`isInfixOf` string) ["/bin", "/sbin" ]) $ do
            when ('/' `elem` string && ':' `notElem` string) $ notify id
            when (isLiteral word && ':' `notElem` string && '/' `notElem` string) $ notify id
    checkVar _ = return ()
    notify id = warn id 2123 "PATH is the shell search path. Use another name."
checkOverridingPath _ _ = return ()

prop_checkTildeInPath1 = verify checkTildeInPath "PATH=\"$PATH:~/bin\""
prop_checkTildeInPath2 = verify checkTildeInPath "PATH='~foo/bin'"
prop_checkTildeInPath3 = verifyNot checkTildeInPath "PATH=~/bin"
checkTildeInPath _ (T_SimpleCommand _ vars _) =
    mapM_ checkVar vars
  where
    checkVar (T_Assignment id Assign "PATH" [] (T_NormalWord _ parts)) =
        when (any (\x -> isQuoted x && hasTilde x) parts) $
            warn id 2147 "Literal tilde in PATH works poorly across programs."
    checkVar _ = return ()

    hasTilde t = fromMaybe False (liftM2 elem (return '~') (getLiteralStringExt (const $ return "") t))
    isQuoted T_DoubleQuoted {} = True
    isQuoted T_SingleQuoted {} = True
    isQuoted _ = False
checkTildeInPath _ _ = return ()

prop_checkUnsupported3 = verify checkUnsupported "#!/bin/sh\ncase foo in bar) baz ;& esac"
prop_checkUnsupported4 = verify checkUnsupported "#!/bin/ksh\ncase foo in bar) baz ;;& esac"
prop_checkUnsupported5 = verify checkUnsupported "#!/bin/bash\necho \"${ ls; }\""
checkUnsupported params t =
    when (not (null support) && (shellType params `notElem` support)) $
        report name
 where
    (name, support) = shellSupport t
    report s = err (getId t) 2127 $
        "To use " ++ s ++ ", specify #!/usr/bin/env " ++
            (map toLower . intercalate " or " . map show $ support)

-- TODO: Move more of these checks here
shellSupport t =
  case t of
    T_CaseExpression _ _ list -> forCase (map (\(a,_,_) -> a) list)
    T_DollarBraceCommandExpansion {} -> ("${ ..; } command expansion", [Ksh])
    _ -> ("", [])
  where
    forCase seps | CaseContinue `elem` seps = ("cases with ;;&", [Bash])
    forCase seps | CaseFallThrough `elem` seps = ("cases with ;&", [Bash, Ksh])
    forCase _ = ("", [])


groupWith f = groupBy ((==) `on` f)

prop_checkMultipleAppends1 = verify checkMultipleAppends "foo >> file; bar >> file; baz >> file;"
prop_checkMultipleAppends2 = verify checkMultipleAppends "foo >> file; bar | grep f >> file; baz >> file;"
prop_checkMultipleAppends3 = verifyNot checkMultipleAppends "foo < file; bar < file; baz < file;"
checkMultipleAppends params t =
    mapM_ checkList $ getCommandSequences t
  where
    checkList list =
        mapM_ checkGroup (groupWith (fmap fst) $ map getTarget list)
    checkGroup (f:_:_:_) | isJust f =
        style (snd $ fromJust f) 2129
            "Consider using { cmd1; cmd2; } >> file instead of individual redirects."
    checkGroup _ = return ()
    getTarget (T_Annotation _ _ t) = getTarget t
    getTarget (T_Pipeline _ _ args@(_:_)) = getTarget (last args)
    getTarget (T_Redirecting id list _) = do
        file <- mapMaybe getAppend list !!! 0
        return (file, id)
    getTarget _ = Nothing
    getAppend (T_FdRedirect _ _ (T_IoFile _ T_DGREAT {} f)) = return f
    getAppend _ = Nothing


prop_checkSuspiciousIFS1 = verify checkSuspiciousIFS "IFS=\"\\n\""
prop_checkSuspiciousIFS2 = verifyNot checkSuspiciousIFS "IFS=$'\\t'"
checkSuspiciousIFS params (T_Assignment id Assign "IFS" [] value) =
    potentially $ do
        str <- getLiteralString value
        return $ check str
  where
    n = if shellType params == Sh then "'<literal linefeed here>'" else "$'\\n'"
    t = if shellType params == Sh then "\"$(printf '\\t')\"" else "$'\\t'"
    check value =
        case value of
            "\\n" -> suggest n
            "/n" -> suggest n
            "\\t" -> suggest t
            "/t" -> suggest t
            _ -> return ()
    suggest r = warn id 2141 $ "Did you mean IFS=" ++ r ++ " ?"
checkSuspiciousIFS _ _ = return ()


prop_checkGrepQ1= verify checkShouldUseGrepQ "[[ $(foo | grep bar) ]]"
prop_checkGrepQ2= verify checkShouldUseGrepQ "[ -z $(fgrep lol) ]"
prop_checkGrepQ3= verify checkShouldUseGrepQ "[ -n \"$(foo | zgrep lol)\" ]"
prop_checkGrepQ4= verifyNot checkShouldUseGrepQ "[ -z $(grep bar | cmd) ]"
prop_checkGrepQ5= verifyNot checkShouldUseGrepQ "rm $(ls | grep file)"
prop_checkGrepQ6= verifyNot checkShouldUseGrepQ "[[ -n $(pgrep foo) ]]"
checkShouldUseGrepQ params t =
    potentially $ case t of
        TC_Nullary id _ token -> check id True token
        TC_Unary id _ "-n" token -> check id True token
        TC_Unary id _ "-z" token -> check id False token
        _ -> fail "not check"
  where
    check id bool token = do
        name <- getFinalGrep token
        let op = if bool then "-n" else "-z"
        let flip = if bool then "" else "! "
        return . style id 2143 $
            "Use " ++ flip ++ name ++ " -q instead of " ++
                "comparing output with [ " ++ op ++ " .. ]."

    getFinalGrep t = do
        cmds <- getPipeline t
        guard . not . null $ cmds
        name <- getCommandBasename $ last cmds
        guard . isGrep $ name
        return name
    getPipeline t =
        case t of
            T_NormalWord _ [x] -> getPipeline x
            T_DoubleQuoted _ [x] -> getPipeline x
            T_DollarExpansion _ [x] -> getPipeline x
            T_Pipeline _ _ cmds -> return cmds
            _ -> fail "unknown"
    isGrep = (`elem` ["grep", "egrep", "fgrep", "zgrep"])

prop_checkTestArgumentSplitting1 = verify checkTestArgumentSplitting "[ -e *.mp3 ]"
prop_checkTestArgumentSplitting2 = verifyNot checkTestArgumentSplitting "[[ $a == *b* ]]"
prop_checkTestArgumentSplitting3 = verify checkTestArgumentSplitting "[[ *.png == '' ]]"
prop_checkTestArgumentSplitting4 = verify checkTestArgumentSplitting "[[ foo == f{o,oo,ooo} ]]"
prop_checkTestArgumentSplitting5 = verify checkTestArgumentSplitting "[[ $@ ]]"
prop_checkTestArgumentSplitting6 = verify checkTestArgumentSplitting "[ -e $@ ]"
prop_checkTestArgumentSplitting7 = verify checkTestArgumentSplitting "[ $@ == $@ ]"
prop_checkTestArgumentSplitting8 = verify checkTestArgumentSplitting "[[ $@ = $@ ]]"
prop_checkTestArgumentSplitting9 = verifyNot checkTestArgumentSplitting "[[ foo =~ bar{1,2} ]]"
prop_checkTestArgumentSplitting10 = verifyNot checkTestArgumentSplitting "[ \"$@\" ]"
prop_checkTestArgumentSplitting11 = verify checkTestArgumentSplitting "[[ \"$@\" ]]"
prop_checkTestArgumentSplitting12 = verify checkTestArgumentSplitting "[ *.png ]"
prop_checkTestArgumentSplitting13 = verify checkTestArgumentSplitting "[ \"$@\" == \"\" ]"
prop_checkTestArgumentSplitting14 = verify checkTestArgumentSplitting "[[ \"$@\" == \"\" ]]"
prop_checkTestArgumentSplitting15 = verifyNot checkTestArgumentSplitting "[[ \"$*\" == \"\" ]]"
prop_checkTestArgumentSplitting16 = verifyNot checkTestArgumentSplitting "[[ -v foo[123] ]]"
checkTestArgumentSplitting :: Parameters -> Token -> Writer [TokenComment] ()
checkTestArgumentSplitting _ t =
    case t of
        (TC_Unary _ typ op token) | isGlob token ->
            if op == "-v"
            then
                when (typ == SingleBracket) $
                    err (getId token) 2208 $
                      "Use [[ ]] or quote arguments to -v to avoid glob expansion."
            else
                err (getId token) 2144 $
                   op ++ " doesn't work with globs. Use a for loop."

        (TC_Nullary _ typ token) -> do
            checkBraces typ token
            checkGlobs typ token
            when (typ == DoubleBracket) $
                checkArrays typ token

        (TC_Unary _ typ op token) -> checkAll typ token

        (TC_Binary _ typ op lhs rhs) ->
            if op `elem` ["=", "==", "!=", "=~"]
            then do
                checkAll typ lhs
                checkArrays typ rhs
                checkBraces typ rhs
            else mapM_ (checkAll typ) [lhs, rhs]
        _ -> return ()
  where
    checkAll typ token = do
        checkArrays typ token
        checkBraces typ token
        checkGlobs typ token

    checkArrays typ token =
        when (any isArrayExpansion $ getWordParts token) $
            if typ == SingleBracket
            then warn (getId token) 2198 "Arrays don't work as operands in [ ]. Use a loop (or concatenate with * instead of @)."
            else err (getId token) 2199 "Arrays implicitly concatenate in [[ ]]. Use a loop (or explicit * instead of @)."

    checkBraces typ token =
        when (any isBraceExpansion $ getWordParts token) $
            if typ == SingleBracket
            then warn (getId token) 2200 "Brace expansions don't work as operands in [ ]. Use a loop."
            else err (getId token) 2201 "Brace expansion doesn't happen in [[ ]]. Use a loop."

    checkGlobs typ token =
        when (isGlob token) $
            if typ == SingleBracket
            then warn (getId token) 2202 "Globs don't work as operands in [ ]. Use a loop."
            else err (getId token) 2203 "Globs are ignored in [[ ]] except right of =/!=. Use a loop."


prop_checkMaskedReturns1 = verify checkMaskedReturns "f() { local a=$(false); }"
prop_checkMaskedReturns2 = verify checkMaskedReturns "declare a=$(false)"
prop_checkMaskedReturns3 = verify checkMaskedReturns "declare a=\"`false`\""
prop_checkMaskedReturns4 = verifyNot checkMaskedReturns "declare a; a=$(false)"
prop_checkMaskedReturns5 = verifyNot checkMaskedReturns "f() { local -r a=$(false); }"
checkMaskedReturns _ t@(T_SimpleCommand id _ (cmd:rest)) = potentially $ do
    name <- getCommandName t
    guard $ name `elem` ["declare", "export"]
        || name == "local" && "r" `notElem` map snd (getAllFlags t)
    return $ mapM_ checkArgs rest
  where
    checkArgs (T_Assignment id _ _ _ word) | any hasReturn $ getWordParts word =
        warn id 2155 "Declare and assign separately to avoid masking return values."
    checkArgs _ = return ()

    hasReturn t = case t of
        T_Backticked {} -> True
        T_DollarExpansion {} -> True
        _ -> False
checkMaskedReturns _ _ = return ()


prop_checkReadWithoutR1 = verify checkReadWithoutR "read -a foo"
prop_checkReadWithoutR2 = verifyNot checkReadWithoutR "read -ar foo"
checkReadWithoutR _ t@T_SimpleCommand {} | t `isUnqualifiedCommand` "read" =
    unless ("r" `elem` map snd (getAllFlags t)) $
        info (getId t) 2162 "read without -r will mangle backslashes."
checkReadWithoutR _ _ = return ()

prop_checkUncheckedCd1 = verifyTree checkUncheckedCdPushdPopd "cd ~/src; rm -r foo"
prop_checkUncheckedCd2 = verifyNotTree checkUncheckedCdPushdPopd "cd ~/src || exit; rm -r foo"
prop_checkUncheckedCd3 = verifyNotTree checkUncheckedCdPushdPopd "set -e; cd ~/src; rm -r foo"
prop_checkUncheckedCd4 = verifyNotTree checkUncheckedCdPushdPopd "if cd foo; then rm foo; fi"
prop_checkUncheckedCd5 = verifyTree checkUncheckedCdPushdPopd "if true; then cd foo; fi"
prop_checkUncheckedCd6 = verifyNotTree checkUncheckedCdPushdPopd "cd .."
prop_checkUncheckedCd7 = verifyNotTree checkUncheckedCdPushdPopd "#!/bin/bash -e\ncd foo\nrm bar"
prop_checkUncheckedCd8 = verifyNotTree checkUncheckedCdPushdPopd "set -o errexit; cd foo; rm bar"
prop_checkUncheckedPushd1 = verifyTree checkUncheckedCdPushdPopd "pushd ~/src; rm -r foo"
prop_checkUncheckedPushd2 = verifyNotTree checkUncheckedCdPushdPopd "pushd ~/src || exit; rm -r foo"
prop_checkUncheckedPushd3 = verifyNotTree checkUncheckedCdPushdPopd "set -e; pushd ~/src; rm -r foo"
prop_checkUncheckedPushd4 = verifyNotTree checkUncheckedCdPushdPopd "if pushd foo; then rm foo; fi"
prop_checkUncheckedPushd5 = verifyTree checkUncheckedCdPushdPopd "if true; then pushd foo; fi"
prop_checkUncheckedPushd6 = verifyNotTree checkUncheckedCdPushdPopd "pushd .."
prop_checkUncheckedPushd7 = verifyNotTree checkUncheckedCdPushdPopd "#!/bin/bash -e\npushd foo\nrm bar"
prop_checkUncheckedPushd8 = verifyNotTree checkUncheckedCdPushdPopd "set -o errexit; pushd foo; rm bar"
prop_checkUncheckedPushd9 = verifyNotTree checkUncheckedCdPushdPopd "pushd -n foo"
prop_checkUncheckedPopd1 = verifyTree checkUncheckedCdPushdPopd "popd; rm -r foo"
prop_checkUncheckedPopd2 = verifyNotTree checkUncheckedCdPushdPopd "popd || exit; rm -r foo"
prop_checkUncheckedPopd3 = verifyNotTree checkUncheckedCdPushdPopd "set -e; popd; rm -r foo"
prop_checkUncheckedPopd4 = verifyNotTree checkUncheckedCdPushdPopd "if popd; then rm foo; fi"
prop_checkUncheckedPopd5 = verifyTree checkUncheckedCdPushdPopd "if true; then popd; fi"
prop_checkUncheckedPopd6 = verifyTree checkUncheckedCdPushdPopd "popd"
prop_checkUncheckedPopd7 = verifyNotTree checkUncheckedCdPushdPopd "#!/bin/bash -e\npopd\nrm bar"
prop_checkUncheckedPopd8 = verifyNotTree checkUncheckedCdPushdPopd "set -o errexit; popd; rm bar"

checkUncheckedCdPushdPopd params root =
    if hasSetE params then
        []
    else execWriter $ doAnalysis checkElement root
  where
    checkElement t@T_SimpleCommand {} =
        when(name t `elem` ["cd", "pushd", "popd"]
            && not (isSafeDir t)
            && not (name t == "pushd" && ("n" `elem` map snd (getAllFlags t)))
            && not (isCondition $ getPath (parentMap params) t)) $
                warn (getId t) 2164 "Use 'cd ... || exit' or 'cd ... || return' in case cd fails."
    checkElement _ = return ()
    name t = fromMaybe "" $ getCommandName t
    isSafeDir t = case oversimplify t of
          [_, ".."] -> True;
          _ -> False

prop_checkLoopVariableReassignment1 = verify checkLoopVariableReassignment "for i in *; do for i in *.bar; do true; done; done"
prop_checkLoopVariableReassignment2 = verify checkLoopVariableReassignment "for i in *; do for((i=0; i<3; i++)); do true; done; done"
prop_checkLoopVariableReassignment3 = verifyNot checkLoopVariableReassignment "for i in *; do for j in *.bar; do true; done; done"
checkLoopVariableReassignment params token =
    potentially $ case token of
        T_ForIn {} -> check
        T_ForArithmetic {} -> check
        _ -> Nothing
  where
    check = do
        str <- loopVariable token
        next <- listToMaybe $ filter (\x -> loopVariable x == Just str) path
        return $ do
            warn (getId token) 2165 "This nested loop overrides the index variable of its parent."
            warn (getId next)  2167 "This parent loop has its index variable overridden."
    path = drop 1 $ getPath (parentMap params) token
    loopVariable :: Token -> Maybe String
    loopVariable t =
        case t of
            T_ForIn _ s _ _ -> return s
            T_ForArithmetic _
                (TA_Sequence _
                    [TA_Assignment _ "="
                        (TA_Expansion _ [T_Literal _ var]) _])
                            _ _ _ -> return var
            _ -> fail "not loop"

prop_checkTrailingBracket1 = verify checkTrailingBracket "if -z n ]]; then true; fi "
prop_checkTrailingBracket2 = verifyNot checkTrailingBracket "if [[ -z n ]]; then true; fi "
prop_checkTrailingBracket3 = verify checkTrailingBracket "a || b ] && thing"
prop_checkTrailingBracket4 = verifyNot checkTrailingBracket "run [ foo ]"
prop_checkTrailingBracket5 = verifyNot checkTrailingBracket "run bar ']'"
checkTrailingBracket _ token =
    case token of
        T_SimpleCommand _ _ tokens@(_:_) -> check (last tokens) token
        _ -> return ()
  where
    check t command =
        case t of
            T_NormalWord id [T_Literal _ str] -> potentially $ do
                guard $ str `elem` [ "]]", "]" ]
                let opposite = invert str
                    parameters = oversimplify command
                guard $ opposite `notElem` parameters
                return $ warn id 2171 $
                    "Found trailing " ++ str ++ " outside test. Missing " ++ opposite ++ "?"
            _ -> return ()
    invert s =
        case s of
            "]]" -> "[["
            "]" -> "["
            x -> x

prop_checkReturnAgainstZero1 = verify checkReturnAgainstZero "[ $? -eq 0 ]"
prop_checkReturnAgainstZero2 = verify checkReturnAgainstZero "[[ \"$?\" -gt 0 ]]"
prop_checkReturnAgainstZero3 = verify checkReturnAgainstZero "[[ 0 -ne $? ]]"
prop_checkReturnAgainstZero4 = verifyNot checkReturnAgainstZero "[[ $? -eq 4 ]]"
prop_checkReturnAgainstZero5 = verify checkReturnAgainstZero "[[ 0 -eq $? ]]"
prop_checkReturnAgainstZero6 = verifyNot checkReturnAgainstZero "[[ $R -eq 0 ]]"
prop_checkReturnAgainstZero7 = verify checkReturnAgainstZero "(( $? == 0 ))"
prop_checkReturnAgainstZero8 = verify checkReturnAgainstZero "(( $? ))"
prop_checkReturnAgainstZero9 = verify checkReturnAgainstZero "(( ! $? ))"
checkReturnAgainstZero _ token =
    case token of
        TC_Binary id _ _ lhs rhs -> check lhs rhs
        TA_Binary id _ lhs rhs -> check lhs rhs
        TA_Unary id _ exp ->
            when (isExitCode exp) $ message (getId exp)
        TA_Sequence _ [exp] ->
            when (isExitCode exp) $ message (getId exp)
        _ -> return ()
  where
    check lhs rhs =
        if isZero rhs && isExitCode lhs
        then message (getId lhs)
        else when (isZero lhs && isExitCode rhs) $ message (getId rhs)
    isZero t = getLiteralString t == Just "0"
    isExitCode t =
        case getWordParts t of
            [exp@T_DollarBraced {}] -> bracedString exp == "?"
            _ -> False
    message id = style id 2181 "Check exit code directly with e.g. 'if mycmd;', not indirectly with $?."

prop_checkRedirectedNowhere1 = verify checkRedirectedNowhere "> file"
prop_checkRedirectedNowhere2 = verify checkRedirectedNowhere "> file | grep foo"
prop_checkRedirectedNowhere3 = verify checkRedirectedNowhere "grep foo | > bar"
prop_checkRedirectedNowhere4 = verifyNot checkRedirectedNowhere "grep foo > bar"
prop_checkRedirectedNowhere5 = verifyNot checkRedirectedNowhere "foo | grep bar > baz"
prop_checkRedirectedNowhere6 = verifyNot checkRedirectedNowhere "var=$(value) 2> /dev/null"
prop_checkRedirectedNowhere7 = verifyNot checkRedirectedNowhere "var=$(< file)"
prop_checkRedirectedNowhere8 = verifyNot checkRedirectedNowhere "var=`< file`"
checkRedirectedNowhere params token =
    case token of
        T_Pipeline _ _ [single] -> potentially $ do
            redir <- getDanglingRedirect single
            guard . not $ isInExpansion token
            return $ warn (getId redir) 2188 "This redirection doesn't have a command. Move to its command (or use 'true' as no-op)."

        T_Pipeline _ _ list -> forM_ list $ \x -> potentially $ do
            redir <- getDanglingRedirect x
            return $ err (getId redir) 2189 "You can't have | between this redirection and the command it should apply to."

        _ -> return ()
  where
    isInExpansion t =
        case drop 1 $ getPath (parentMap params) t of
            T_DollarExpansion _ [_] : _ -> True
            T_Backticked _ [_] : _ -> True
            T_Annotation _ _ u : _ -> isInExpansion u
            _ -> False
    getDanglingRedirect token =
        case token of
            T_Redirecting _ (first:_) (T_SimpleCommand _ [] []) -> return first
            _ -> Nothing


prop_checkArrayAssignmentIndices1 = verifyTree checkArrayAssignmentIndices "declare -A foo; foo=(bar)"
prop_checkArrayAssignmentIndices2 = verifyNotTree checkArrayAssignmentIndices "declare -a foo; foo=(bar)"
prop_checkArrayAssignmentIndices3 = verifyNotTree checkArrayAssignmentIndices "declare -A foo; foo=([i]=bar)"
prop_checkArrayAssignmentIndices4 = verifyTree checkArrayAssignmentIndices "typeset -A foo; foo+=(bar)"
prop_checkArrayAssignmentIndices5 = verifyTree checkArrayAssignmentIndices "arr=( [foo]= bar )"
prop_checkArrayAssignmentIndices6 = verifyTree checkArrayAssignmentIndices "arr=( [foo] = bar )"
prop_checkArrayAssignmentIndices7 = verifyTree checkArrayAssignmentIndices "arr=( var=value )"
prop_checkArrayAssignmentIndices8 = verifyNotTree checkArrayAssignmentIndices "arr=( [foo]=bar )"
prop_checkArrayAssignmentIndices9 = verifyNotTree checkArrayAssignmentIndices "arr=( [foo]=\"\" )"
checkArrayAssignmentIndices params root =
    runNodeAnalysis check params root
  where
    assocs = getAssociativeArrays root
    check _ t =
        case t of
            T_Assignment _ _ name [] (T_Array _ list) ->
                let isAssoc = name `elem` assocs in
                    mapM_ (checkElement isAssoc) list
            _ -> return ()

    checkElement isAssociative t =
        case t of
            T_IndexedElement _ _ (T_Literal id "") ->
                warn id 2192 "This array element has no value. Remove spaces after = or use \"\" for empty string."
            T_IndexedElement {} ->
                return ()

            T_NormalWord _ parts ->
                let literalEquals = do
                    part <- parts
                    (id, str) <- case part of
                        T_Literal id str -> [(id,str)]
                        _ -> []
                    guard $ '=' `elem` str
                    return $ warn id 2191 "The = here is literal. To assign by index, use ( [index]=value ) with no spaces. To keep as literal, quote it."
                in
                    if null literalEquals && isAssociative
                    then warn (getId t) 2190 "Elements in associative arrays need index, e.g. array=( [index]=value ) ."
                    else sequence_ literalEquals

            _ -> return ()

prop_checkUnmatchableCases1 = verify checkUnmatchableCases "case foo in bar) true; esac"
prop_checkUnmatchableCases2 = verify checkUnmatchableCases "case foo-$bar in ??|*) true; esac"
prop_checkUnmatchableCases3 = verify checkUnmatchableCases "case foo in foo) true; esac"
prop_checkUnmatchableCases4 = verifyNot checkUnmatchableCases "case foo-$bar in foo*|*bar|*baz*) true; esac"
prop_checkUnmatchableCases5 = verify checkUnmatchableCases "case $f in *.txt) true;; f??.txt) false;; esac"
prop_checkUnmatchableCases6 = verifyNot checkUnmatchableCases "case $f in ?*) true;; *) false;; esac"
prop_checkUnmatchableCases7 = verifyNot checkUnmatchableCases "case $f in $(x)) true;; asdf) false;; esac"
prop_checkUnmatchableCases8 = verify checkUnmatchableCases "case $f in cow) true;; bar|cow) false;; esac"
checkUnmatchableCases _ t =
    case t of
        T_CaseExpression _ word list -> do
            let patterns = concatMap snd3 list

            if isConstant word
                then warn (getId word) 2194
                        "This word is constant. Did you forget the $ on a variable?"
                else  potentially $ do
                    pg <- wordToPseudoGlob word
                    return $ mapM_ (check pg) patterns

            let exactGlobs = tupMap wordToExactPseudoGlob patterns
            let fuzzyGlobs = tupMap wordToPseudoGlob patterns
            let dominators = zip exactGlobs (tails $ drop 1 fuzzyGlobs)

            mapM_ checkDoms dominators

        _ -> return ()
  where
    snd3 (_,x,_) = x
    check target candidate = potentially $ do
        candidateGlob <- wordToPseudoGlob candidate
        guard . not $ pseudoGlobsCanOverlap target candidateGlob
        return $ warn (getId candidate) 2195
                    "This pattern will never match the case statement's word. Double check them."

    tupMap f l = zip l (map f l)
    checkDoms ((glob, Just x), rest) =
        case filter (\(_, p) -> x `pseudoGlobIsSuperSetof` p) valids of
            ((first,_):_) -> do
                warn (getId glob) 2221 "This pattern always overrides a later one."
                warn (getId first) 2222 "This pattern never matches because of a previous pattern."
            _ -> return ()
      where
        valids = concatMap f rest
        f (x, Just y) = [(x,y)]
        f _ = []
    checkDoms _ = return ()


prop_checkSubshellAsTest1 = verify checkSubshellAsTest "( -e file )"
prop_checkSubshellAsTest2 = verify checkSubshellAsTest "( 1 -gt 2 )"
prop_checkSubshellAsTest3 = verifyNot checkSubshellAsTest "( grep -c foo bar )"
prop_checkSubshellAsTest4 = verifyNot checkSubshellAsTest "[ 1 -gt 2 ]"
prop_checkSubshellAsTest5 = verify checkSubshellAsTest "( -e file && -x file )"
prop_checkSubshellAsTest6 = verify checkSubshellAsTest "( -e file || -x file && -t 1 )"
prop_checkSubshellAsTest7 = verify checkSubshellAsTest "( ! -d file )"
checkSubshellAsTest _ t =
    case t of
        T_Subshell id [w] -> check id w
        _ -> return ()
  where
    check id t = case t of
        (T_Banged _ w) -> check id w
        (T_AndIf _ w _) -> check id w
        (T_OrIf _ w _) -> check id w
        (T_Pipeline _ _ [T_Redirecting _ _ (T_SimpleCommand _ [] (first:second:_))]) ->
            checkParams id first second
        _ -> return ()


    checkParams id first second = do
        when (fromMaybe False $ (`elem` unaryTestOps) <$> getLiteralString first) $
            err id 2204 "(..) is a subshell. Did you mean [ .. ], a test expression?"
        when (fromMaybe False $ (`elem` binaryTestOps) <$> getLiteralString second) $
            warn id 2205 "(..) is a subshell. Did you mean [ .. ], a test expression?"


prop_checkSplittingInArrays1 = verify checkSplittingInArrays "a=( $var )"
prop_checkSplittingInArrays2 = verify checkSplittingInArrays "a=( $(cmd) )"
prop_checkSplittingInArrays3 = verifyNot checkSplittingInArrays "a=( \"$var\" )"
prop_checkSplittingInArrays4 = verifyNot checkSplittingInArrays "a=( \"$(cmd)\" )"
prop_checkSplittingInArrays5 = verifyNot checkSplittingInArrays "a=( $! $$ $# )"
prop_checkSplittingInArrays6 = verifyNot checkSplittingInArrays "a=( ${#arr[@]} )"
prop_checkSplittingInArrays7 = verifyNot checkSplittingInArrays "a=( foo{1,2} )"
prop_checkSplittingInArrays8 = verifyNot checkSplittingInArrays "a=( * )"
checkSplittingInArrays params t =
    case t of
        T_Array _ elements -> mapM_ check elements
        _ -> return ()
  where
    check word = case word of
        T_NormalWord _ parts -> mapM_ checkPart parts
        _ -> return ()
    checkPart part = case part of
        T_DollarExpansion id _ -> forCommand id
        T_DollarBraceCommandExpansion id _ -> forCommand id
        T_Backticked id _ -> forCommand id
        T_DollarBraced id str |
            not (isCountingReference part)
            && not (isQuotedAlternativeReference part)
            && not (getBracedReference (bracedString part) `elem` variablesWithoutSpaces)
            -> warn id 2206 $
                if shellType params == Ksh
                then "Quote to prevent word splitting, or split robustly with read -A or while read."
                else "Quote to prevent word splitting, or split robustly with mapfile or read -a."
        _ -> return ()

    forCommand id =
        warn id 2207 $
            if shellType params == Ksh
            then "Prefer read -A or while read to split command output (or quote to avoid splitting)."
            else "Prefer mapfile or read -a to split command output (or quote to avoid splitting)."


prop_checkRedirectionToNumber1 = verify checkRedirectionToNumber "( 1 > 2 )"
prop_checkRedirectionToNumber2 = verify checkRedirectionToNumber "foo 1>2"
prop_checkRedirectionToNumber3 = verifyNot checkRedirectionToNumber "echo foo > '2'"
prop_checkRedirectionToNumber4 = verifyNot checkRedirectionToNumber "foo 1>&2"
checkRedirectionToNumber _ t = case t of
    T_IoFile id _ word -> potentially $ do
        file <- getUnquotedLiteral word
        guard $ all isDigit file
        return $ warn id 2210 "This is a file redirection. Was it supposed to be a comparison or fd operation?"
    _ -> return ()

prop_checkGlobAsCommand1 = verify checkGlobAsCommand "foo*"
prop_checkGlobAsCommand2 = verify checkGlobAsCommand "$(var[i])"
prop_checkGlobAsCommand3 = verifyNot checkGlobAsCommand "echo foo*"
checkGlobAsCommand _ t = case t of
    T_SimpleCommand _ _ (first:_) ->
        when (isGlob first) $
            warn (getId first) 2211 "This is a glob used as a command name. Was it supposed to be in ${..}, array, or is it missing quoting?"
    _ -> return ()


prop_checkFlagAsCommand1 = verify checkFlagAsCommand "-e file"
prop_checkFlagAsCommand2 = verify checkFlagAsCommand "foo\n  --bar=baz"
prop_checkFlagAsCommand3 = verifyNot checkFlagAsCommand "'--myexec--' args"
prop_checkFlagAsCommand4 = verifyNot checkFlagAsCommand "var=cmd --arg"  -- Handled by SC2037
checkFlagAsCommand _ t = case t of
    T_SimpleCommand _ [] (first:_) ->
        when (isUnquotedFlag first) $
            warn (getId first) 2215 "This flag is used as a command name. Bad line break or missing [ .. ]?"
    _ -> return ()


prop_checkEmptyCondition1 = verify checkEmptyCondition "if [ ]; then ..; fi"
prop_checkEmptyCondition2 = verifyNot checkEmptyCondition "[ foo -o bar ]"
checkEmptyCondition _ t = case t of
    TC_Empty id _ -> style id 2212 "Use 'false' instead of empty [/[[ conditionals."
    _ -> return ()

prop_checkPipeToNowhere1 = verify checkPipeToNowhere "foo | echo bar"
prop_checkPipeToNowhere2 = verify checkPipeToNowhere "basename < file.txt"
prop_checkPipeToNowhere3 = verify checkPipeToNowhere "printf 'Lol' <<< str"
prop_checkPipeToNowhere4 = verify checkPipeToNowhere "printf 'Lol' << eof\nlol\neof\n"
prop_checkPipeToNowhere5 = verifyNot checkPipeToNowhere "echo foo | xargs du"
prop_checkPipeToNowhere6 = verifyNot checkPipeToNowhere "ls | echo $(cat)"
prop_checkPipeToNowhere7 = verifyNot checkPipeToNowhere "echo foo | var=$(cat) ls"
prop_checkPipeToNowhere8 = verify checkPipeToNowhere "foo | true"
checkPipeToNowhere :: Parameters -> Token -> WriterT [TokenComment] Identity ()
checkPipeToNowhere _ t =
    case t of
        T_Pipeline _ _ (first:rest) -> mapM_ checkPipe rest
        T_Redirecting _ redirects cmd -> when (any redirectsStdin redirects) $ checkRedir cmd
        _ -> return ()
  where
    checkPipe redir = potentially $ do
        cmd <- getCommand redir
        name <- getCommandBasename cmd
        guard $ name `elem` nonReadingCommands
        guard . not $ hasAdditionalConsumers cmd
        return $ warn (getId cmd) 2216 $
            "Piping to '" ++ name ++ "', a command that doesn't read stdin. Wrong command or missing xargs?"

    checkRedir cmd = potentially $ do
        name <- getCommandBasename cmd
        guard $ name `elem` nonReadingCommands
        guard . not $ hasAdditionalConsumers cmd
        return $ warn (getId cmd) 2217 $
            "Redirecting to '" ++ name ++ "', a command that doesn't read stdin. Bad quoting or missing xargs?"

    -- Could any words in a SimpleCommand consume stdin (e.g. echo "$(cat)")?
    hasAdditionalConsumers t = fromMaybe True $ do
        doAnalysis (guard . not . mayConsume) t
        return False

    mayConsume t =
        case t of
            T_ProcSub {} -> True
            T_Backticked {} -> True
            T_DollarExpansion {} -> True
            _ -> False

    redirectsStdin t =
        case t of
            T_FdRedirect _ _ (T_IoFile _ T_Less {} _) -> True
            T_FdRedirect _ _ T_HereDoc {} -> True
            T_FdRedirect _ _ T_HereString {} -> True
            _ -> False

prop_checkUseBeforeDefinition1 = verifyTree checkUseBeforeDefinition "f; f() { true; }"
prop_checkUseBeforeDefinition2 = verifyNotTree checkUseBeforeDefinition "f() { true; }; f"
prop_checkUseBeforeDefinition3 = verifyNotTree checkUseBeforeDefinition "if ! mycmd --version; then mycmd() { true; }; fi"
prop_checkUseBeforeDefinition4 = verifyNotTree checkUseBeforeDefinition "mycmd || mycmd() { f; }"
checkUseBeforeDefinition _ t =
    execWriter $ evalStateT (mapM_ examine $ revCommands) Map.empty
  where
    examine t = case t of
        T_Pipeline _ _ [T_Redirecting _ _ (T_Function _ _ _ name _)] ->
            modify $ Map.insert name t
        T_Annotation _ _ w -> examine w
        T_Pipeline _ _ cmds -> do
            m <- get
            unless (Map.null m) $
                mapM_ (checkUsage m) $ concatMap recursiveSequences cmds
        _ -> return ()

    checkUsage map cmd = potentially $ do
        name <- getCommandName cmd
        def <- Map.lookup name map
        return $
            err (getId cmd) 2218
                "This function is only defined later. Move the definition up."

    revCommands = reverse $ concat $ getCommandSequences t
    recursiveSequences x =
        let list = concat $ getCommandSequences x in
            if null list
            then [x]
            else concatMap recursiveSequences list

return []
runTests =  $( [| $(forAllProperties) (quickCheckWithResult (stdArgs { maxSuccess = 1 }) ) |])
