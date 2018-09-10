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
{-# LANGUAGE FlexibleContexts #-}
module ShellCheck.Analytics (runAnalytics) where

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
    ,checkForLoopGlobVariables
    ,checkSubshelledTests
    ,checkInvertedStringTest
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

-- prop> verify checkEchoWc "n=$(echo $foo | wc -c)"
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

-- |
-- prop> verify checkPipedAssignment "A=ls | grep foo"
-- prop> verifyNot checkPipedAssignment "A=foo cmd | grep foo"
-- prop> verifyNot checkPipedAssignment "A=foo"
checkPipedAssignment _ (T_Pipeline _ _ (T_Redirecting _ _ (T_SimpleCommand id (_:_) []):_:_)) =
    warn id 2036 "If you wanted to assign the output of the pipeline, use a=$(b | c) ."
checkPipedAssignment _ _ = return ()

-- |
-- prop> verify checkAssignAteCommand "A=ls -l"
-- prop> verify checkAssignAteCommand "A=ls --sort=$foo"
-- prop> verify checkAssignAteCommand "A=cat foo | grep bar"
-- prop> verifyNot checkAssignAteCommand "A=foo ls -l"
-- prop> verify checkAssignAteCommand "PAGER=cat grep bar"
-- prop> verifyNot checkAssignAteCommand "PAGER=\"cat\" grep bar"
-- prop> verify checkAssignAteCommand "here=pwd"
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

-- |
-- prop> verify checkArithmeticOpCommand "i=i + 1"
-- prop> verify checkArithmeticOpCommand "foo=bar * 2"
-- prop> verifyNot checkArithmeticOpCommand "foo + opts"
checkArithmeticOpCommand _ (T_SimpleCommand id [T_Assignment {}] (firstWord:_)) =
    fromMaybe (return ()) $ check <$> getGlobOrLiteralString firstWord
  where
    check op =
        when (op `elem` ["+", "-", "*", "/"]) $
            warn (getId firstWord) 2099 $
                "Use $((..)) for arithmetics, e.g. i=$((i " ++ op ++ " 2))"
checkArithmeticOpCommand _ _ = return ()

-- |
-- prop> verify checkWrongArithmeticAssignment "i=i+1"
-- prop> verify checkWrongArithmeticAssignment "n=2; i=n*2"
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


-- |
-- prop> verify checkUuoc "cat foo | grep bar"
-- prop> verifyNot checkUuoc "cat * | grep bar"
-- prop> verify checkUuoc "cat $var | grep bar"
-- prop> verifyNot checkUuoc "cat $var"
-- prop> verifyNot checkUuoc "cat \"$@\""
-- prop> verifyNot checkUuoc "cat -n | grep bar"
checkUuoc _ (T_Pipeline _ _ (T_Redirecting _ _ cmd:_:_)) =
    checkCommand "cat" (const f) cmd
  where
    f [word] = unless (mayBecomeMultipleArgs word || isOption word) $
        style (getId word) 2002 "Useless cat. Consider 'cmd < file | ..' or 'cmd file | ..' instead."
    f _ = return ()
    isOption word = "-" `isPrefixOf` onlyLiteralString word
checkUuoc _ _ = return ()

-- |
-- prop> verify checkPipePitfalls "ls | grep -v mp3"
-- prop> verifyNot checkPipePitfalls "find . -print0 | xargs -0 foo"
-- prop> verifyNot checkPipePitfalls "ls -N | foo"
-- prop> verify checkPipePitfalls "find . | xargs foo"
-- prop> verifyNot checkPipePitfalls "find . -printf '%s\\n' | xargs foo"
-- prop> verify checkPipePitfalls "foo | grep bar | wc -l"
-- prop> verifyNot checkPipePitfalls "foo | grep -o bar | wc -l"
-- prop> verifyNot checkPipePitfalls "foo | grep -o bar | wc"
-- prop> verifyNot checkPipePitfalls "foo | grep bar | wc"
-- prop> verifyNot checkPipePitfalls "foo | grep -o bar | wc -c"
-- prop> verifyNot checkPipePitfalls "foo | grep bar | wc -c"
-- prop> verifyNot checkPipePitfalls "foo | grep -o bar | wc -cmwL"
-- prop> verifyNot checkPipePitfalls "foo | grep bar | wc -cmwL"
-- prop> verifyNot checkPipePitfalls "foo | grep -r bar | wc -l"
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
    first func (x:_) = func (getId $ getCommandTokenOrThis x)
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


-- |
-- prop> verifyTree checkShebangParameters "#!/usr/bin/env bash -x\necho cow"
-- prop> verifyNotTree checkShebangParameters "#! /bin/sh  -l "
checkShebangParameters p (T_Annotation _ _ t) = checkShebangParameters p t
checkShebangParameters _ (T_Script id sb _) =
    [makeComment ErrorC id 2096 "On most OS, shebangs can only specify a single parameter." | length (words sb) > 2]

-- |
-- prop> verifyNotTree checkShebang "#!/usr/bin/env bash -x\necho cow"
-- prop> verifyNotTree checkShebang "#! /bin/sh  -l "
-- prop> verifyTree checkShebang "ls -l"
-- prop> verifyNotTree checkShebang "#shellcheck shell=sh\nfoo"
-- prop> verifyTree checkShebang "#!/usr/bin/env ash"
-- prop> verifyNotTree checkShebang "#!/usr/bin/env ash\n# shellcheck shell=dash\n"
-- prop> verifyNotTree checkShebang "#!/usr/bin/env ash\n# shellcheck shell=sh\n"
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


-- |
-- prop> verify checkForInQuoted "for f in \"$(ls)\"; do echo foo; done"
-- prop> verifyNot checkForInQuoted "for f in \"$@\"; do echo foo; done"
-- prop> verifyNot checkForInQuoted "for f in *.mp3; do echo foo; done"
-- prop> verify checkForInQuoted "for f in \"*.mp3\"; do echo foo; done"
-- prop> verify checkForInQuoted "for f in 'find /'; do true; done"
-- prop> verify checkForInQuoted "for f in 1,2,3; do true; done"
-- prop> verifyNot checkForInQuoted "for f in foo{1,2,3}; do true; done"
-- prop> verify checkForInQuoted "for f in ls; do true; done"
-- prop> verifyNot checkForInQuoted "for f in \"${!arr}\"; do true; done"
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

-- |
-- prop> verify checkForInCat "for f in $(cat foo); do stuff; done"
-- prop> verify checkForInCat "for f in `cat foo`; do stuff; done"
-- prop> verify checkForInCat "for f in $(cat foo | grep lol); do stuff; done"
-- prop> verify checkForInCat "for f in `cat foo | grep lol`; do stuff; done"
-- prop> verifyNot checkForInCat "for f in $(cat foo | grep bar | wc -l); do stuff; done"
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

-- |
-- prop> verify checkForInLs "for f in $(ls *.mp3); do mplayer \"$f\"; done"
-- prop> verify checkForInLs "for f in `ls *.mp3`; do mplayer \"$f\"; done"
-- prop> verify checkForInLs "for f in `find / -name '*.mp3'`; do mplayer \"$f\"; done"
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


-- |
-- prop> verify checkFindExec "find / -name '*.php' -exec rm {};"
-- prop> verify checkFindExec "find / -exec touch {} && ls {} \\;"
-- prop> verify checkFindExec "find / -execdir cat {} | grep lol +"
-- prop> verifyNot checkFindExec "find / -name '*.php' -exec foo {} +"
-- prop> verifyNot checkFindExec "find / -execdir bash -c 'a && b' \\;"
-- prop> verify checkFindExec "find / -type d -execdir rm *.jpg \\;"
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


-- |
-- prop> verify checkUnquotedExpansions "rm $(ls)"
-- prop> verify checkUnquotedExpansions "rm `ls`"
-- prop> verify checkUnquotedExpansions "rm foo$(date)"
-- prop> verify checkUnquotedExpansions "[ $(foo) == cow ]"
-- prop> verify checkUnquotedExpansions "[ ! $(foo) ]"
-- prop> verifyNot checkUnquotedExpansions "[[ $(foo) == cow ]]"
-- prop> verifyNot checkUnquotedExpansions "for f in $(cmd); do echo $f; done"
-- prop> verifyNot checkUnquotedExpansions "$(cmd)"
-- prop> verifyNot checkUnquotedExpansions "cat << foo\n$(ls)\nfoo"
-- prop> verifyNot checkUnquotedExpansions "set -- $(seq 1 4)"
-- prop> verifyNot checkUnquotedExpansions "echo foo `# inline comment`"
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


-- |
-- prop> verify checkRedirectToSame "cat foo > foo"
-- prop> verify checkRedirectToSame "cat lol | sed -e 's/a/b/g' > lol"
-- prop> verifyNot checkRedirectToSame "cat lol | sed -e 's/a/b/g' > foo.bar && mv foo.bar lol"
-- prop> verifyNot checkRedirectToSame "foo /dev/null > /dev/null"
-- prop> verifyNot checkRedirectToSame "foo > bar 2> bar"
-- prop> verifyNot checkRedirectToSame "echo foo > foo"
-- prop> verifyNot checkRedirectToSame "sed 's/foo/bar/g' file | sponge file"
-- prop> verifyNot checkRedirectToSame "while read -r line; do _=\"$fname\"; done <\"$fname\""
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
                && not (any isHarmlessCommand [t,u])
                && not (any containsAssignment [u])) $ do
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
    containsAssignment arg = fromMaybe False $ do
        cmd <- getClosestCommand (parentMap params) arg
        return $ isAssignment cmd

checkRedirectToSame _ _ = return ()


-- |
-- prop> verify checkShorthandIf "[[ ! -z file ]] && scp file host || rm file"
-- prop> verifyNot checkShorthandIf "[[ ! -z file ]] && { scp file host || echo 'Eek'; }"
-- prop> verifyNot checkShorthandIf "foo && bar || echo baz"
-- prop> verifyNot checkShorthandIf "foo && a=b || a=c"
-- prop> verifyNot checkShorthandIf "foo && rm || printf b"
-- prop> verifyNot checkShorthandIf "if foo && bar || baz; then true; fi"
-- prop> verifyNot checkShorthandIf "while foo && bar || baz; do true; done"
-- prop> verify checkShorthandIf "if true; then foo && bar || baz; fi"
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


-- |
-- prop> verify checkDollarStar "for f in $*; do ..; done"
-- prop> verifyNot checkDollarStar "a=$*"
-- prop> verifyNot checkDollarStar "[[ $* = 'a b' ]]"
checkDollarStar p t@(T_NormalWord _ [b@(T_DollarBraced id _)])
      | bracedString b == "*"  =
    unless (isStrictlyQuoteFree (parentMap p) t) $
        warn id 2048 "Use \"$@\" (with quotes) to prevent whitespace problems."
checkDollarStar _ _ = return ()


-- |
-- prop> verify checkUnquotedDollarAt "ls $@"
-- prop> verifyNot checkUnquotedDollarAt "ls ${#@}"
-- prop> verify checkUnquotedDollarAt "ls ${foo[@]}"
-- prop> verifyNot checkUnquotedDollarAt "ls ${#foo[@]}"
-- prop> verifyNot checkUnquotedDollarAt "ls \"$@\""
-- prop> verifyNot checkUnquotedDollarAt "ls ${foo/@/ at }"
-- prop> verifyNot checkUnquotedDollarAt "a=$@"
-- prop> verify checkUnquotedDollarAt "for f in ${var[@]}; do true; done"
-- prop> verifyNot checkUnquotedDollarAt "echo \"${args[@]:+${args[@]}}\""
-- prop> verifyNot checkUnquotedDollarAt "echo ${args[@]:+\"${args[@]}\"}"
-- prop> verifyNot checkUnquotedDollarAt "echo ${@+\"$@\"}"
checkUnquotedDollarAt p word@(T_NormalWord _ parts) | not $ isStrictlyQuoteFree (parentMap p) word =
    forM_ (take 1 $ filter isArrayExpansion parts) $ \x ->
        unless (isQuotedAlternativeReference x) $
            err (getId x) 2068
                "Double quote array expansions to avoid re-splitting elements."
checkUnquotedDollarAt _ _ = return ()

-- |
-- prop> verify checkConcatenatedDollarAt "echo \"foo$@\""
-- prop> verify checkConcatenatedDollarAt "echo ${arr[@]}lol"
-- prop> verify checkConcatenatedDollarAt "echo $a$@"
-- prop> verifyNot checkConcatenatedDollarAt "echo $@"
-- prop> verifyNot checkConcatenatedDollarAt "echo \"${arr[@]}\""
checkConcatenatedDollarAt p word@T_NormalWord {}
    | not $ isQuoteFree (parentMap p) word =
        unless (null $ drop 1 parts) $
            mapM_ for array
  where
    parts = getWordParts word
    array = take 1 $ filter isArrayExpansion parts
    for t = err (getId t) 2145 "Argument mixes string and array. Use * or separate argument."
checkConcatenatedDollarAt _ _ = return ()

-- |
-- prop> verify checkArrayAsString "a=$@"
-- prop> verify checkArrayAsString "a=\"${arr[@]}\""
-- prop> verify checkArrayAsString "a=*.png"
-- prop> verify checkArrayAsString "a={1..10}"
-- prop> verifyNot checkArrayAsString "a='*.gif'"
-- prop> verifyNot checkArrayAsString "a=$*"
-- prop> verifyNot checkArrayAsString "a=( $@ )"
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

-- |
-- prop> verifyTree checkArrayWithoutIndex "foo=(a b); echo $foo"
-- prop> verifyNotTree checkArrayWithoutIndex "foo='bar baz'; foo=($foo); echo ${foo[0]}"
-- prop> verifyTree checkArrayWithoutIndex "coproc foo while true; do echo cow; done; echo $foo"
-- prop> verifyTree checkArrayWithoutIndex "coproc tail -f log; echo $COPROC"
-- prop> verifyTree checkArrayWithoutIndex "a[0]=foo; echo $a"
-- prop> verifyTree checkArrayWithoutIndex "echo $PIPESTATUS"
-- prop> verifyTree checkArrayWithoutIndex "a=(a b); a+=c"
-- prop> verifyTree checkArrayWithoutIndex "declare -a foo; foo=bar;"
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

-- |
-- prop> verify checkStderrRedirect "test 2>&1 > cow"
-- prop> verifyNot checkStderrRedirect "test > cow 2>&1"
-- prop> verifyNot checkStderrRedirect "test 2>&1 > file | grep stderr"
-- prop> verifyNot checkStderrRedirect "errors=$(test 2>&1 > file)"
-- prop> verifyNot checkStderrRedirect "read < <(test 2>&1 > file)"
-- prop> verify checkStderrRedirect "foo | bar 2>&1 > /dev/null"
-- prop> verifyNot checkStderrRedirect "{ cmd > file; } 2>&1"
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
        warn id 2069 "To redirect stdout+stderr, 2>&1 must be last (or use '{ cmd > file; } 2>&1' to clarify)."

checkStderrRedirect _ _ = return ()

lt x = trace ("Tracing " ++ show x) x
ltt t = trace ("Tracing " ++ show t)


-- |
-- prop> verify checkSingleQuotedVariables "echo '$foo'"
-- prop> verify checkSingleQuotedVariables "echo 'lol$1.jpg'"
-- prop> verifyNot checkSingleQuotedVariables "sed 's/foo$/bar/'"
-- prop> verify checkSingleQuotedVariables "sed 's/${foo}/bar/'"
-- prop> verify checkSingleQuotedVariables "sed 's/$(echo cow)/bar/'"
-- prop> verify checkSingleQuotedVariables "sed 's/$((1+foo))/bar/'"
-- prop> verifyNot checkSingleQuotedVariables "awk '{print $1}'"
-- prop> verifyNot checkSingleQuotedVariables "trap 'echo $SECONDS' EXIT"
-- prop> verifyNot checkSingleQuotedVariables "sed -n '$p'"
-- prop> verify checkSingleQuotedVariables "sed -n '$pattern'"
-- prop> verifyNot checkSingleQuotedVariables "PS1='$PWD \\$ '"
-- prop> verify checkSingleQuotedVariables "find . -exec echo '$1' {} +"
-- prop> verifyNot checkSingleQuotedVariables "find . -exec awk '{print $1}' {} \\;"
-- prop> verify checkSingleQuotedVariables "echo '`pwd`'"
-- prop> verifyNot checkSingleQuotedVariables "sed '${/lol/d}'"
-- prop> verifyNot checkSingleQuotedVariables "eval 'echo $1'"
-- prop> verifyNot checkSingleQuotedVariables "busybox awk '{print $1}'"
-- prop> verifyNot checkSingleQuotedVariables "[ -v 'bar[$foo]' ]"
-- prop> verifyNot checkSingleQuotedVariables "git filter-branch 'test $GIT_COMMIT'"
-- prop> verify checkSingleQuotedVariables "git '$a'"
-- prop> verifyNot checkSingleQuotedVariables "rename 's/(.)a/$1/g' *"

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
        return $ if name == "find" then getFindCommand cmd else if name == "git" then getGitCommand cmd else name

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
                ,"docker" -- like above
                ,"dpkg-query"
                ,"jq"  -- could also check that user provides --arg
                ,"rename"
                ,"unset"
                ,"git filter-branch"
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
    getGitCommand (T_SimpleCommand _ _ words) =
        case map getLiteralString words of
            Just "git":Just "filter-branch":_ -> "git filter-branch"
            _ -> "git"
    getGitCommand (T_Redirecting _ _ cmd) = getGitCommand cmd
    getGitCommand _ = "git"
checkSingleQuotedVariables _ _ = return ()


-- |
-- prop> verify checkUnquotedN "if [ -n $foo ]; then echo cow; fi"
-- prop> verify checkUnquotedN "[ -n $cow ]"
-- prop> verifyNot checkUnquotedN "[[ -n $foo ]] && echo cow"
-- prop> verify checkUnquotedN "[ -n $cow -o -t 1 ]"
-- prop> verifyNot checkUnquotedN "[ -n \"$@\" ]"
checkUnquotedN _ (TC_Unary _ SingleBracket "-n" (T_NormalWord id [t])) | willSplit t =
       err id 2070 "-n doesn't work with unquoted arguments. Quote or use [[ ]]."
checkUnquotedN _ _ = return ()

-- |
-- prop> verify checkNumberComparisons "[[ $foo < 3 ]]"
-- prop> verify checkNumberComparisons "[[ 0 >= $(cmd) ]]"
-- prop> verifyNot checkNumberComparisons "[[ $foo ]] > 3"
-- prop> verify checkNumberComparisons "[[ $foo > 2.72 ]]"
-- prop> verify checkNumberComparisons "[[ $foo -le 2.72 ]]"
-- prop> verify checkNumberComparisons "[[ 3.14 -eq $foo ]]"
-- prop> verifyNot checkNumberComparisons "[[ 3.14 == $foo ]]"
-- prop> verify checkNumberComparisons "[ foo <= bar ]"
-- prop> verify checkNumberComparisons "[ foo \\>= bar ]"
-- prop> verify checkNumberComparisons "[ $foo -eq 'N' ]"
-- prop> verify checkNumberComparisons "[ x$foo -gt x${N} ]"
-- prop> verify checkNumberComparisons "[ $foo > $bar ]"
-- prop> verifyNot checkNumberComparisons "[[ foo < bar ]]"
-- prop> verifyNot checkNumberComparisons "[ $foo '>' $bar ]"
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

-- |
-- prop> verify checkSingleBracketOperators "[ test =~ foo ]"
checkSingleBracketOperators params (TC_Binary id SingleBracket "=~" lhs rhs) =
    when (shellType params `elem` [Bash, Ksh]) $
        err id 2074 $ "Can't use =~ in [ ]. Use [[..]] instead."
checkSingleBracketOperators _ _ = return ()

-- |
-- prop> verify checkDoubleBracketOperators "[[ 3 \\< 4 ]]"
-- prop> verifyNot checkDoubleBracketOperators "[[ foo < bar ]]"
checkDoubleBracketOperators _ x@(TC_Binary id typ op lhs rhs)
    | typ == DoubleBracket && op `elem` ["\\<", "\\>"] =
        err id 2075 $ "Escaping " ++ op ++" is required in [..], but invalid in [[..]]"
checkDoubleBracketOperators _ _ = return ()

-- |
-- prop> verify checkConditionalAndOrs "[ foo && bar ]"
-- prop> verify checkConditionalAndOrs "[[ foo -o bar ]]"
-- prop> verifyNot checkConditionalAndOrs "[[ foo || bar ]]"
-- prop> verify checkConditionalAndOrs "[ foo -a bar ]"
-- prop> verify checkConditionalAndOrs "[ -z 3 -o a = b ]"
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

-- |
-- prop> verify checkQuotedCondRegex "[[ $foo =~ \"bar.*\" ]]"
-- prop> verify checkQuotedCondRegex "[[ $foo =~ '(cow|bar)' ]]"
-- prop> verifyNot checkQuotedCondRegex "[[ $foo =~ $foo ]]"
-- prop> verifyNot checkQuotedCondRegex "[[ $foo =~ \"bar\" ]]"
-- prop> verifyNot checkQuotedCondRegex "[[ $foo =~ 'cow bar' ]]"
-- prop> verify checkQuotedCondRegex "[[ $foo =~ 'cow|bar' ]]"
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

-- |
-- prop> verify checkGlobbedRegex "[[ $foo =~ *foo* ]]"
-- prop> verify checkGlobbedRegex "[[ $foo =~ f* ]]"
-- prop> verify checkGlobbedRegex "[[ $foo =~ \\#* ]]"
-- prop> verifyNot checkGlobbedRegex "[[ $foo =~ $foo ]]"
-- prop> verifyNot checkGlobbedRegex "[[ $foo =~ ^c.* ]]"
checkGlobbedRegex _ (TC_Binary _ DoubleBracket "=~" _ rhs) =
    let s = concat $ oversimplify rhs in
        when (isConfusedGlobRegex s) $
            warn (getId rhs) 2049 "=~ is for regex. Use == for globs."
checkGlobbedRegex _ _ = return ()


-- |
-- prop> verify checkConstantIfs "[[ foo != bar ]]"
-- prop> verify checkConstantIfs "[ n -le 4 ]"
-- prop> verifyNot checkConstantIfs "[[ n -le 4 ]]"
-- prop> verify checkConstantIfs "[[ $n -le 4 && n != 2 ]]"
-- prop> verifyNot checkConstantIfs "[[ $n -le 3 ]]"
-- prop> verifyNot checkConstantIfs "[[ $n -le $n ]]"
-- prop> verifyNot checkConstantIfs "[[ a -ot b ]]"
-- prop> verifyNot checkConstantIfs "[ a -nt b ]"
-- prop> verifyNot checkConstantIfs "[[ ~foo == '~foo' ]]"
-- prop> verify checkConstantIfs "[[ *.png == [a-z] ]]"
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

-- |
-- prop> verify checkLiteralBreakingTest "[[ a==$foo ]]"
-- prop> verify checkLiteralBreakingTest "[ $foo=3 ]"
-- prop> verify checkLiteralBreakingTest "[ $foo!=3 ]"
-- prop> verify checkLiteralBreakingTest "[ \"$(ls) \" ]"
-- prop> verify checkLiteralBreakingTest "[ -n \"$(true) \" ]"
-- prop> verify checkLiteralBreakingTest "[ -z $(true)z ]"
-- prop> verifyNot checkLiteralBreakingTest "[ -z $(true) ]"
-- prop> verifyNot checkLiteralBreakingTest "[ $(true)$(true) ]"
-- prop> verify checkLiteralBreakingTest "[ -z foo ]"
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

-- |
-- prop> verify checkConstantNullary "[[ '$(foo)' ]]"
-- prop> verify checkConstantNullary "[ \"-f lol\" ]"
-- prop> verify checkConstantNullary "[[ cmd ]]"
-- prop> verify checkConstantNullary "[[ ! cmd ]]"
-- prop> verify checkConstantNullary "[[ true ]]"
-- prop> verify checkConstantNullary "[ 1 ]"
-- prop> verify checkConstantNullary "[ false ]"
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

-- |
-- prop> verify checkForDecimals "((3.14*c))"
-- prop> verify checkForDecimals "foo[1.2]=bar"
-- prop> verifyNot checkForDecimals "declare -A foo; foo[1.2]=bar"
checkForDecimals params t@(TA_Expansion id _) = potentially $ do
    guard $ not (hasFloatingPoint params)
    str <- getLiteralString t
    first <- str !!! 0
    guard $ isDigit first && '.' `elem` str
    return $ err id 2079 "(( )) doesn't support decimals. Use bc or awk."
checkForDecimals _ _ = return ()

-- |
-- prop> verify checkDivBeforeMult "echo $((c/n*100))"
-- prop> verifyNot checkDivBeforeMult "echo $((c*100/n))"
-- prop> verifyNot checkDivBeforeMult "echo $((c/10*10))"
checkDivBeforeMult params (TA_Binary _ "*" (TA_Binary id "/" _ x) y)
    | not (hasFloatingPoint params) && x /= y =
        info id 2017 "Increase precision by replacing a/b*c with a*c/b."
checkDivBeforeMult _ _ = return ()

-- |
-- prop> verify checkArithmeticDeref "echo $((3+$foo))"
-- prop> verify checkArithmeticDeref "cow=14; (( s+= $cow ))"
-- prop> verifyNot checkArithmeticDeref "cow=1/40; (( s+= ${cow%%/*} ))"
-- prop> verifyNot checkArithmeticDeref "(( ! $? ))"
-- prop> verifyNot checkArithmeticDeref "(($1))"
-- prop> verify checkArithmeticDeref "(( a[$i] ))"
-- prop> verifyNot checkArithmeticDeref "(( 10#$n ))"
-- prop> verifyNot checkArithmeticDeref "let i=$i+1"
-- prop> verifyNot checkArithmeticDeref "(( a[foo] ))"
-- prop> verifyNot checkArithmeticDeref "(( a[\\$foo] ))"
-- prop> verifyNot checkArithmeticDeref "a[$foo]=wee"
-- prop> verify checkArithmeticDeref "for ((i=0; $i < 3; i)); do true; done"
-- prop> verifyNot checkArithmeticDeref "(( $$ ))"
-- prop> verifyNot checkArithmeticDeref "(( $! ))"
-- prop> verifyNot checkArithmeticDeref "(( ${!var} ))"
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
            T_SimpleCommand {} -> return noWarning
            _ -> Nothing

    normalWarning = style id 2004 "$/${} is unnecessary on arithmetic variables."
    noWarning = return ()
checkArithmeticDeref _ _ = return ()

-- |
-- prop> verify checkArithmeticBadOctal "(( 0192 ))"
-- prop> verifyNot checkArithmeticBadOctal "(( 0x192 ))"
-- prop> verifyNot checkArithmeticBadOctal "(( 1 ^ 0777 ))"
checkArithmeticBadOctal _ t@(TA_Expansion id _) = potentially $ do
    str <- getLiteralString t
    guard $ str `matches` octalRE
    return $ err id 2080 "Numbers with leading 0 are considered octal."
  where
    octalRE = mkRegex "^0[0-7]*[8-9]"
checkArithmeticBadOctal _ _ = return ()

-- |
-- prop> verify checkComparisonAgainstGlob "[[ $cow == $bar ]]"
-- prop> verifyNot checkComparisonAgainstGlob "[[ $cow == \"$bar\" ]]"
-- prop> verify checkComparisonAgainstGlob "[ $cow = *foo* ]"
-- prop> verifyNot checkComparisonAgainstGlob "[ $cow = foo ]"
-- prop> verify checkComparisonAgainstGlob "[[ $cow != $bar ]]"
checkComparisonAgainstGlob _ (TC_Binary _ DoubleBracket op _ (T_NormalWord id [T_DollarBraced _ _]))
    | op `elem` ["=", "==", "!="] =
        warn id 2053 $ "Quote the rhs of " ++ op ++ " in [[ ]] to prevent glob matching."
checkComparisonAgainstGlob _ (TC_Binary _ SingleBracket op _ word)
        | (op == "=" || op == "==") && isGlob word =
    err (getId word) 2081 "[ .. ] can't match globs. Use [[ .. ]] or case statement."
checkComparisonAgainstGlob _ _ = return ()

-- |
-- prop> verify checkCommarrays "a=(1, 2)"
-- prop> verify checkCommarrays "a+=(1,2,3)"
-- prop> verifyNot checkCommarrays "cow=(1 \"foo,bar\" 3)"
-- prop> verifyNot checkCommarrays "cow=('one,' 'two')"
-- prop> verify checkCommarrays "a=([a]=b, [c]=d)"
-- prop> verify checkCommarrays "a=([a]=b,[c]=d,[e]=f)"
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

-- |
-- prop> verify checkOrNeq "if [[ $lol -ne cow || $lol -ne foo ]]; then echo foo; fi"
-- prop> verify checkOrNeq "(( a!=lol || a!=foo ))"
-- prop> verify checkOrNeq "[ \"$a\" != lol || \"$a\" != foo ]"
-- prop> verifyNot checkOrNeq "[ a != $cow || b != $foo ]"
-- prop> verifyNot checkOrNeq "[[ $a != /home || $a != */public_html/* ]]"
-- This only catches the most idiomatic cases. Fixme?
checkOrNeq _ (TC_Or id typ op (TC_Binary _ _ op1 lhs1 rhs1 ) (TC_Binary _ _ op2 lhs2 rhs2))
    | lhs1 == lhs2 && (op1 == op2 && (op1 == "-ne" || op1 == "!=")) && not (any isGlob [rhs1,rhs2]) =
        warn id 2055 $ "You probably wanted " ++ (if typ == SingleBracket then "-a" else "&&") ++ " here."

checkOrNeq _ (TA_Binary id "||" (TA_Binary _ "!=" word1 _) (TA_Binary _ "!=" word2 _))
    | word1 == word2 =
        warn id 2056 "You probably wanted && here."
checkOrNeq _ _ = return ()


-- |
-- prop> verify checkValidCondOps "[[ a -xz b ]]"
-- prop> verify checkValidCondOps "[ -M a ]"
-- prop> verifyNot checkValidCondOps "[ 3 \\> 2 ]"
-- prop> verifyNot checkValidCondOps "[ 1 = 2 -a 3 -ge 4 ]"
-- prop> verifyNot checkValidCondOps "[[ ! -v foo ]]"
checkValidCondOps _ (TC_Binary id _ s _ _)
    | s `notElem` binaryTestOps =
        warn id 2057 "Unknown binary operator."
checkValidCondOps _ (TC_Unary id _ s _)
    | s `notElem`  unaryTestOps =
        warn id 2058 "Unknown unary operator."
checkValidCondOps _ _ = return ()

-- |
-- prop> verify checkUuoeVar "for f in $(echo $tmp); do echo lol; done"
-- prop> verify checkUuoeVar "date +`echo \"$format\"`"
-- prop> verifyNot checkUuoeVar "foo \"$(echo -e '\r')\""
-- prop> verifyNot checkUuoeVar "echo $tmp"
-- prop> verify checkUuoeVar "foo \"$(echo \"$(date) value:\" $value)\""
-- prop> verifyNot checkUuoeVar "foo \"$(echo files: *.png)\""
-- prop> verifyNot checkUuoeVar "foo $(echo $(bar))" -- covered by 2005
-- prop> verifyNot checkUuoeVar "#!/bin/sh\nz=$(echo)"
-- prop> verify checkUuoeVar "foo $(echo $(<file))"
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


-- |
-- prop> verify checkTestRedirects "test 3 > 1"
-- prop> verifyNot checkTestRedirects "test 3 \\> 1"
-- prop> verify checkTestRedirects "/usr/bin/test $var > $foo"
-- prop> verifyNot checkTestRedirects "test 1 -eq 2 2> file"
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

-- |
-- prop> verify checkPS1Assignments "PS1='\\033[1;35m\\$ '"
-- prop> verify checkPS1Assignments "export PS1='\\033[1;35m\\$ '"
-- prop> verify checkPS1Assignments "PS1='\\h \\e[0m\\$ '"
-- prop> verify checkPS1Assignments "PS1=$'\\x1b[c '"
-- prop> verify checkPS1Assignments "PS1=$'\\e[3m; '"
-- prop> verify checkPS1Assignments "export PS1=$'\\e[3m; '"
-- prop> verifyNot checkPS1Assignments "PS1='\\[\\033[1;35m\\]\\$ '"
-- prop> verifyNot checkPS1Assignments "PS1='\\[\\e1m\\e[1m\\]\\$ '"
-- prop> verifyNot checkPS1Assignments "PS1='e033x1B'"
-- prop> verifyNot checkPS1Assignments "PS1='\\[\\e\\]'"
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

-- prop> verify checkBackticks "echo `foo`"
-- prop> verifyNot checkBackticks "echo $(foo)"
-- prop> verifyNot checkBackticks "echo `#inlined comment` foo"
checkBackticks _ (T_Backticked id list) | not (null list) =
    style id 2006 "Use $(...) notation instead of legacy backticked `...`."
checkBackticks _ _ = return ()

-- |
-- prop> verify checkIndirectExpansion "${foo$n}"
-- prop> verifyNot checkIndirectExpansion "${foo//$n/lol}"
-- prop> verify checkIndirectExpansion "${$#}"
-- prop> verify checkIndirectExpansion "${var${n}_$((i%2))}"
-- prop> verifyNot checkIndirectExpansion "${bar}"
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

-- |
-- prop> verify checkInexplicablyUnquoted "echo 'var='value';'"
-- prop> verifyNot checkInexplicablyUnquoted "'foo'*"
-- prop> verifyNot checkInexplicablyUnquoted "wget --user-agent='something'"
-- prop> verify checkInexplicablyUnquoted "echo \"VALUES (\"id\")\""
-- prop> verifyNot checkInexplicablyUnquoted "\"$dir\"/\"$file\""
-- prop> verifyNot checkInexplicablyUnquoted "\"$dir\"some_stuff\"$file\""
-- prop> verifyNot checkInexplicablyUnquoted "${dir/\"foo\"/\"bar\"}"
-- prop> verifyNot checkInexplicablyUnquoted "  'foo'\\\n  'bar'"
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

-- |
-- prop> verify checkTildeInQuotes "var=\"~/out.txt\""
-- prop> verify checkTildeInQuotes "foo > '~/dir'"
-- prop> verifyNot checkTildeInQuotes "~/file"
-- prop> verifyNot checkTildeInQuotes "echo '/~foo/cow'"
-- prop> verifyNot checkTildeInQuotes "awk '$0 ~ /foo/'"
checkTildeInQuotes _ = check
  where
    verify id ('~':'/':_) = warn id 2088 "Tilde does not expand in quotes. Use $HOME."
    verify _ _ = return ()
    check (T_NormalWord _ (T_SingleQuoted id str:_)) =
        verify id str
    check (T_NormalWord _ (T_DoubleQuoted _ (T_Literal id str:_):_)) =
        verify id str
    check _ = return ()

-- prop> verify checkLonelyDotDash "./ file"
-- prop> verifyNot checkLonelyDotDash "./file"
checkLonelyDotDash _ t@(T_Redirecting id _ _)
    | isUnqualifiedCommand t "./" =
        err id 2083 "Don't add spaces after the slash in './file'."
checkLonelyDotDash _ _ = return ()


-- |
-- prop> verify checkSpuriousExec "exec foo; true"
-- prop> verify checkSpuriousExec "if a; then exec b; exec c; fi"
-- prop> verifyNot checkSpuriousExec "echo cow; exec foo"
-- prop> verifyNot checkSpuriousExec "if a; then exec b; fi"
-- prop> verifyNot checkSpuriousExec "exec > file; cmd"
-- prop> verify checkSpuriousExec "exec foo > file; cmd"
-- prop> verifyNot checkSpuriousExec "exec file; echo failed; exit 3"
-- prop> verifyNot checkSpuriousExec "exec {origout}>&1- >tmp.log 2>&1; bar"
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


-- |
-- prop> verify checkSpuriousExpansion "if $(true); then true; fi"
-- prop> verify checkSpuriousExpansion "while \"$(cmd)\"; do :; done"
-- prop> verifyNot checkSpuriousExpansion "$(cmd) --flag1 --flag2"
-- prop> verify checkSpuriousExpansion "$((i++))"
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


-- |
-- prop> verify checkDollarBrackets "echo $[1+2]"
-- prop> verifyNot checkDollarBrackets "echo $((1+2))"
checkDollarBrackets _ (T_DollarBracket id _) =
    style id 2007 "Use $((..)) instead of deprecated $[..]"
checkDollarBrackets _ _ = return ()

-- |
-- prop> verify checkSshHereDoc "ssh host << foo\necho $PATH\nfoo"
-- prop> verifyNot checkSshHereDoc "ssh host << 'foo'\necho $PATH\nfoo"
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

-- | Subshell detection.
--
-- prop> verifyTree     subshellAssignmentCheck "cat foo | while read bar; do a=$bar; done; echo \"$a\""
-- prop> verifyNotTree subshellAssignmentCheck "while read bar; do a=$bar; done < file; echo \"$a\""
-- prop> verifyTree    subshellAssignmentCheck "( A=foo; ); rm $A"
-- prop> verifyNotTree subshellAssignmentCheck "( A=foo; rm $A; )"
-- prop> verifyTree    subshellAssignmentCheck "cat foo | while read cow; do true; done; echo $cow;"
-- prop> verifyTree    subshellAssignmentCheck "( export lol=$(ls); ); echo $lol;"
-- prop> verifyTree    subshellAssignmentCheck "( typeset -a lol=a; ); echo $lol;"
-- prop> verifyTree    subshellAssignmentCheck "cmd | while read foo; do (( n++ )); done; echo \"$n lines\""
-- prop> verifyTree    subshellAssignmentCheck "n=3 & echo $((n++))"
-- prop> verifyTree    subshellAssignmentCheck "read n & n=foo$n"
-- prop> verifyTree    subshellAssignmentCheck "(( n <<= 3 )) & (( n |= 4 )) &"
-- prop> verifyTree subshellAssignmentCheck "cat /etc/passwd | while read line; do let n=n+1; done\necho $n"
-- prop> verifyTree subshellAssignmentCheck "cat /etc/passwd | while read line; do let ++n; done\necho $n"
-- prop> verifyTree subshellAssignmentCheck "#!/bin/bash\necho foo | read bar; echo $bar"
-- prop> verifyNotTree subshellAssignmentCheck "#!/bin/ksh93\necho foo | read bar; echo $bar"
-- prop> verifyNotTree subshellAssignmentCheck "#!/bin/ksh\ncat foo | while read bar; do a=$bar; done\necho \"$a\""
-- prop> verifyNotTree subshellAssignmentCheck "(set -e); echo $@"
-- prop> verifyNotTree subshellAssignmentCheck "foo=${ { bar=$(baz); } 2>&1; }; echo $foo $bar"
-- prop> verifyTree subshellAssignmentCheck "( exec {n}>&2; ); echo $n"
-- prop> verifyNotTree subshellAssignmentCheck "#!/bin/bash\nshopt -s lastpipe; echo a | read -r b; echo \"$b\""
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

-- | Check whether variables could have spaces/globs.
--
-- prop> verifyTree checkSpacefulness "a='cow moo'; echo $a"
-- prop> verifyNotTree checkSpacefulness "a='cow moo'; [[ $a ]]"
-- prop> verifyNotTree checkSpacefulness "a='cow*.mp3'; echo \"$a\""
-- prop> verifyTree checkSpacefulness "for f in *.mp3; do echo $f; done"
-- prop> verifyNotTree checkSpacefulness "foo=3; foo=$(echo $foo)"
-- prop> verifyTree checkSpacefulness "a='*'; b=$a; c=lol${b//foo/bar}; echo $c"
-- prop> verifyTree checkSpacefulness "a=foo$(lol); echo $a"
-- prop> verifyTree checkSpacefulness "a=foo\\ bar; rm $a"
-- prop> verifyNotTree checkSpacefulness "a=foo\\ bar; a=foo; rm $a"
-- prop> verifyTree checkSpacefulness "rm $1"
-- prop> verifyTree checkSpacefulness "rm ${10//foo/bar}"
-- prop> verifyNotTree checkSpacefulness "(( $1 + 3 ))"
-- prop> verifyNotTree checkSpacefulness "if [[ $2 -gt 14 ]]; then true; fi"
-- prop> verifyNotTree checkSpacefulness "foo=$3 env"
-- prop> verifyNotTree checkSpacefulness "local foo=$1"
-- prop> verifyNotTree checkSpacefulness "declare foo=$1"
-- prop> verifyTree checkSpacefulness "echo foo=$1"
-- prop> verifyNotTree checkSpacefulness "$1 --flags"
-- prop> verifyTree checkSpacefulness "echo $PWD"
-- prop> verifyNotTree checkSpacefulness "n+='foo bar'"
-- prop> verifyNotTree checkSpacefulness "select foo in $bar; do true; done"
-- prop> verifyNotTree checkSpacefulness "echo $\"$1\""
-- prop> verifyNotTree checkSpacefulness "a=(1); echo ${a[@]}"
-- prop> verifyTree checkSpacefulness "a='a    b'; cat <<< $a"
-- prop> verifyTree checkSpacefulness "a='s/[0-9]//g'; sed $a"
-- prop> verifyTree checkSpacefulness "a='foo bar'; echo {1,2,$a}"
-- prop> verifyNotTree checkSpacefulness "echo ${a:+'foo'}"
-- prop> verifyNotTree checkSpacefulness "exec {n}>&1; echo $n"
-- prop> verifyNotTree checkSpacefulness "n=$(stuff); exec {n}>&-;"
-- prop> verifyTree checkSpacefulness "file='foo bar'; echo foo > $file;"
-- prop> verifyNotTree checkSpacefulness "echo \"`echo \\\"$1\\\"`\""
-- prop> verifyNotTree checkSpacefulness "var=$1; [ -v var ]"
-- prop> verifyTree checkSpacefulness "for file; do echo $file; done"
-- prop> verifyTree checkSpacefulness "declare foo$n=$1"
-- prop> verifyNotTree checkSpacefulness "echo ${1+\"$1\"}"

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

-- |
-- prop> verifyTree checkQuotesInLiterals "param='--foo=\"bar\"'; app $param"
-- prop> verifyTree checkQuotesInLiterals "param=\"--foo='lolbar'\"; app $param"
-- prop> verifyNotTree checkQuotesInLiterals "param='--foo=\"bar\"'; app \"$param\""
-- prop> verifyNotTree checkQuotesInLiterals "param=('--foo='); app \"${param[@]}\""
-- prop> verifyNotTree checkQuotesInLiterals "param=\"don't bother with this one\"; app $param"
-- prop> verifyNotTree checkQuotesInLiterals "param=\"--foo='lolbar'\"; eval app $param"
-- prop> verifyTree checkQuotesInLiterals "param='my\\ file'; cmd=\"rm $param\"; $cmd"
-- prop> verifyNotTree checkQuotesInLiterals "param='my\\ file'; cmd=\"rm ${#param}\"; $cmd"
-- prop> verifyTree checkQuotesInLiterals "param='my\\ file'; rm $param"
-- prop> verifyTree checkQuotesInLiterals "param=\"/foo/'bar baz'/etc\"; rm $param"
-- prop> verifyNotTree checkQuotesInLiterals "param=\"/foo/'bar baz'/etc\"; rm ${#param}"
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


-- |
-- prop> verifyTree checkFunctionsUsedExternally "foo() { :; }; sudo foo"
-- prop> verifyTree checkFunctionsUsedExternally "alias f='a'; xargs -n 1 f"
-- prop> verifyNotTree checkFunctionsUsedExternally "f() { :; }; echo f"
-- prop> verifyNotTree checkFunctionsUsedExternally "foo() { :; }; sudo \"foo\""
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

-- |
-- prop> verifyNotTree checkUnusedAssignments "var=foo; echo $var"
-- prop> verifyTree checkUnusedAssignments "var=foo; echo $bar"
-- prop> verifyNotTree checkUnusedAssignments "var=foo; export var;"
-- prop> verifyTree checkUnusedAssignments "for f in *; do echo '$f'; done"
-- prop> verifyTree checkUnusedAssignments "local i=0"
-- prop> verifyNotTree checkUnusedAssignments "read lol; echo $lol"
-- prop> verifyNotTree checkUnusedAssignments "var=4; (( var++ ))"
-- prop> verifyNotTree checkUnusedAssignments "var=2; $((var))"
-- prop> verifyTree checkUnusedAssignments "var=2; var=3;"
-- prop> verifyNotTree checkUnusedAssignments "read ''"
-- prop> verifyNotTree checkUnusedAssignments "read -p 'test: '"
-- prop> verifyNotTree checkUnusedAssignments "bar=5; export foo[$bar]=3"
-- prop> verifyNotTree checkUnusedAssignments "read foo; echo ${!foo}"
-- prop> verifyNotTree checkUnusedAssignments "x=(1); (( x[0] ))"
-- prop> verifyNotTree checkUnusedAssignments "x=(1); n=0; echo ${x[n]}"
-- prop> verifyNotTree checkUnusedAssignments "x=(1); n=0; (( x[n] ))"
-- prop> verifyNotTree checkUnusedAssignments "foo=5; declare -x foo"
-- prop> verifyNotTree checkUnusedAssignments "read -i 'foo' -e -p 'Input: ' bar; $bar;"
-- prop> verifyNotTree checkUnusedAssignments "a=1; arr=( [$a]=42 ); echo \"${arr[@]}\""
-- prop> verifyNotTree checkUnusedAssignments "a=1; let b=a+1; echo $b"
-- prop> verifyNotTree checkUnusedAssignments "a=1; PS1='$a'"
-- prop> verifyNotTree checkUnusedAssignments "a=1; trap 'echo $a' INT"
-- prop> verifyNotTree checkUnusedAssignments "a=1; [ -v a ]"
-- prop> verifyNotTree checkUnusedAssignments "a=1; [ -R a ]"
-- prop> verifyNotTree checkUnusedAssignments "mapfile -C a b; echo ${b[@]}"
-- prop> verifyNotTree checkUnusedAssignments "readarray foo; echo ${foo[@]}"
-- prop> verifyNotTree checkUnusedAssignments "declare -F foo"
-- prop> verifyTree checkUnusedAssignments "var=3; [ var -eq 3 ]"
-- prop> verifyNotTree checkUnusedAssignments "var=3; [[ var -eq 3 ]]"
-- prop> verifyNotTree checkUnusedAssignments "var=(a b); declare -p var"
-- prop> verifyTree checkUnusedAssignments "let a=1"
-- prop> verifyTree checkUnusedAssignments "let 'a=1'"
-- prop> verifyTree checkUnusedAssignments "let a=b=c; echo $a"
-- prop> verifyNotTree checkUnusedAssignments "a=foo; [[ foo =~ ^{$a}$ ]]"
-- prop> verifyNotTree checkUnusedAssignments "foo=1; (( t = foo )); echo $t"
-- prop> verifyNotTree checkUnusedAssignments "a=foo; b=2; echo ${a:b}"
-- prop> verifyNotTree checkUnusedAssignments "if [[ -v foo ]]; then true; fi"
-- prop> verifyNotTree checkUnusedAssignments "fd=2; exec {fd}>&-"
-- prop> verifyTree checkUnusedAssignments "(( a=42 ))"
-- prop> verifyNotTree checkUnusedAssignments "declare -x -f foo"
-- prop> verifyNotTree checkUnusedAssignments "arr=(1 2); num=2; echo \"${arr[@]:num}\""
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
            name ++ " appears unused. Verify use (or export if used externally)."

    stripSuffix = takeWhile isVariableChar
    defaultMap = Map.fromList $ zip internalVariables $ repeat ()

-- |
-- prop> verifyTree checkUnassignedReferences "echo $foo"
-- prop> verifyNotTree checkUnassignedReferences "foo=hello; echo $foo"
-- prop> verifyTree checkUnassignedReferences "MY_VALUE=3; echo $MYVALUE"
-- prop> verifyNotTree checkUnassignedReferences "RANDOM2=foo; echo $RANDOM"
-- prop> verifyNotTree checkUnassignedReferences "declare -A foo=([bar]=baz); echo ${foo[bar]}"
-- prop> verifyNotTree checkUnassignedReferences "foo=..; echo ${foo-bar}"
-- prop> verifyNotTree checkUnassignedReferences "getopts ':h' foo; echo $foo"
-- prop> verifyNotTree checkUnassignedReferences "let 'foo = 1'; echo $foo"
-- prop> verifyNotTree checkUnassignedReferences "echo ${foo-bar}"
-- prop> verifyNotTree checkUnassignedReferences "echo ${foo:?}"
-- prop> verifyNotTree checkUnassignedReferences "declare -A foo; echo \"${foo[@]}\""
-- prop> verifyNotTree checkUnassignedReferences "typeset -a foo; echo \"${foo[@]}\""
-- prop> verifyNotTree checkUnassignedReferences "f() { local foo; echo $foo; }"
-- prop> verifyNotTree checkUnassignedReferences "foo=; echo $foo"
-- prop> verifyNotTree checkUnassignedReferences "f() { true; }; export -f f"
-- prop> verifyNotTree checkUnassignedReferences "declare -A foo=( [a b]=bar ); echo ${foo[a b]}"
-- prop> verifyNotTree checkUnassignedReferences "USERS=foo; echo $USER"
-- prop> verifyNotTree checkUnassignedReferences "FOOBAR=42; export FOOBAR="
-- prop> verifyNotTree checkUnassignedReferences "readonly foo=bar; echo $foo"
-- prop> verifyNotTree checkUnassignedReferences "printf -v foo bar; echo $foo"
-- prop> verifyTree checkUnassignedReferences "echo ${#foo}"
-- prop> verifyNotTree checkUnassignedReferences "echo ${!os*}"
-- prop> verifyTree checkUnassignedReferences "declare -a foo; foo[bar]=42;"
-- prop> verifyNotTree checkUnassignedReferences "declare -A foo; foo[bar]=42;"
-- prop> verifyNotTree checkUnassignedReferences "declare -A foo=(); foo[bar]=42;"
-- prop> verifyNotTree checkUnassignedReferences "a::b() { foo; }; readonly -f a::b"
-- prop> verifyNotTree checkUnassignedReferences ": ${foo:=bar}"
-- prop> verifyNotTree checkUnassignedReferences "#!/bin/ksh\necho \"${.sh.version}\"\n"
-- prop> verifyNotTree checkUnassignedReferences "if [[ -v foo ]]; then echo $foo; fi"
-- prop> verifyNotTree checkUnassignedReferences "if [[ -v foo[3] ]]; then echo ${foo[3]}; fi"
-- prop> verifyNotTree checkUnassignedReferences "X=1; if [[ -v foo[$X+42] ]]; then echo ${foo[$X+42]}; fi"
-- prop> verifyNotTree checkUnassignedReferences "if [[ -v \"foo[1]\" ]]; then echo ${foo[@]}; fi"
-- prop> verifyNotTree checkUnassignedReferences "f() { local -A foo; echo \"${foo[@]}\"; }"
-- prop> verifyNotTree checkUnassignedReferences "declare -A foo; (( foo[bar] ))"
-- prop> verifyNotTree checkUnassignedReferences "echo ${arr[foo-bar]:?fail}"
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
        rest `matches` guardRegex
      where
        name = concat $ oversimplify v
        rest = dropWhile isVariableChar $ dropWhile (`elem` "#!") name
    isGuarded _ = False
    --  :? or :- with optional array index and colon
    guardRegex = mkRegex "^(\\[.*\\])?:?[-?]"

    match var candidate =
        if var /= candidate && map toLower var == map toLower candidate
        then 1
        else dist var candidate


-- |
-- prop> verify checkGlobsAsOptions "rm *.txt"
-- prop> verify checkGlobsAsOptions "ls ??.*"
-- prop> verifyNot checkGlobsAsOptions "rm -- *.txt"
-- prop> verifyNot checkGlobsAsOptions "*.txt"
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


-- |
-- prop> verify checkWhileReadPitfalls "while read foo; do ssh $foo uptime; done < file"
-- prop> verifyNot checkWhileReadPitfalls "while read -u 3 foo; do ssh $foo uptime; done 3< file"
-- prop> verifyNot checkWhileReadPitfalls "while true; do ssh host uptime; done"
-- prop> verifyNot checkWhileReadPitfalls "while read foo; do ssh $foo hostname < /dev/null; done"
-- prop> verifyNot checkWhileReadPitfalls "while read foo; do echo ls | ssh $foo; done"
-- prop> verifyNot checkWhileReadPitfalls "while read foo <&3; do ssh $foo; done 3< foo"
-- prop> verify checkWhileReadPitfalls "while read foo; do if true; then ssh $foo uptime; fi; done < file"
-- prop> verifyNot checkWhileReadPitfalls "while read foo; do ssh -n $foo uptime; done < file"

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


-- |
-- prop> verify checkPrefixAssignmentReference "var=foo echo $var"
-- prop> verifyNot checkPrefixAssignmentReference "var=$(echo $var) cmd"
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

-- |
-- prop> verify checkCharRangeGlob "ls *[:digit:].jpg"
-- prop> verifyNot checkCharRangeGlob "ls *[[:digit:]].jpg"
-- prop> verify checkCharRangeGlob "ls [10-15]"
-- prop> verifyNot checkCharRangeGlob "ls [a-zA-Z]"
-- prop> verifyNot checkCharRangeGlob "tr -d [a-zA-Z]" -- tr has 2060
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



-- |
-- prop> verify checkCdAndBack "for f in *; do cd $f; git pull; cd ..; done"
-- prop> verifyNot checkCdAndBack "for f in *; do cd $f || continue; git pull; cd ..; done"
-- prop> verifyNot checkCdAndBack "while [[ $PWD != / ]]; do cd ..; done"
-- prop> verify checkCdAndBack "cd $tmp; foo; cd -"
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

-- |
-- prop> verify checkLoopKeywordScope "continue 2"
-- prop> verify checkLoopKeywordScope "for f; do ( break; ); done"
-- prop> verify checkLoopKeywordScope "if true; then continue; fi"
-- prop> verifyNot checkLoopKeywordScope "while true; do break; done"
-- prop> verify checkLoopKeywordScope "if true; then break; fi"
-- prop> verify checkLoopKeywordScope "while true; do true | { break; }; done"
-- prop> verifyNot checkLoopKeywordScope "#!/bin/ksh\nwhile true; do true | { break; }; done"
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


-- |
-- prop> verify checkFunctionDeclarations "#!/bin/ksh\nfunction foo() { command foo --lol \"$@\"; }"
-- prop> verify checkFunctionDeclarations "#!/bin/dash\nfunction foo { lol; }"
-- prop> verifyNot checkFunctionDeclarations "foo() { echo bar; }"
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



-- |
-- prop> verify checkStderrPipe "#!/bin/ksh\nfoo |& bar"
-- prop> verifyNot checkStderrPipe "#!/bin/bash\nfoo |& bar"
checkStderrPipe params =
    case shellType params of
        Ksh -> match
        _ -> const $ return ()
  where
    match (T_Pipe id "|&") =
        err id 2118 "Ksh does not support |&. Use 2>&1 |."
    match _ = return ()

-- |
-- prop> verifyTree checkUnpassedInFunctions "foo() { echo $1; }; foo"
-- prop> verifyNotTree checkUnpassedInFunctions "foo() { echo $1; };"
-- prop> verifyNotTree checkUnpassedInFunctions "foo() { echo $lol; }; foo"
-- prop> verifyNotTree checkUnpassedInFunctions "foo() { echo $0; }; foo"
-- prop> verifyNotTree checkUnpassedInFunctions "foo() { echo $1; }; foo 'lol'; foo"
-- prop> verifyNotTree checkUnpassedInFunctions "foo() { set -- *; echo $1; }; foo"
-- prop> verifyTree checkUnpassedInFunctions "foo() { echo $1; }; foo; foo;"
-- prop> verifyNotTree checkUnpassedInFunctions "foo() { echo $((1)); }; foo;"
-- prop> verifyNotTree checkUnpassedInFunctions "foo() { echo $(($b)); }; foo;"
-- prop> verifyNotTree checkUnpassedInFunctions "foo() { echo $!; }; foo;"
-- prop> verifyNotTree checkUnpassedInFunctions "foo() { bar() { echo $1; }; bar baz; }; foo;"
-- prop> verifyNotTree checkUnpassedInFunctions "foo() { echo ${!var*}; }; foo;"
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


-- |
-- prop> verify checkOverridingPath "PATH=\"$var/$foo\""
-- prop> verify checkOverridingPath "PATH=\"mydir\""
-- prop> verify checkOverridingPath "PATH=/cow/foo"
-- prop> verifyNot checkOverridingPath "PATH=/cow/foo/bin"
-- prop> verifyNot checkOverridingPath "PATH='/bin:/sbin'"
-- prop> verifyNot checkOverridingPath "PATH=\"$var/$foo\" cmd"
-- prop> verifyNot checkOverridingPath "PATH=$OLDPATH"
-- prop> verifyNot checkOverridingPath "PATH=$PATH:/stuff"
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

-- |
-- prop> verify checkTildeInPath "PATH=\"$PATH:~/bin\""
-- prop> verify checkTildeInPath "PATH='~foo/bin'"
-- prop> verifyNot checkTildeInPath "PATH=~/bin"
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

-- |
-- prop> verify checkUnsupported "#!/bin/sh\ncase foo in bar) baz ;& esac"
-- prop> verify checkUnsupported "#!/bin/ksh\ncase foo in bar) baz ;;& esac"
-- prop> verify checkUnsupported "#!/bin/bash\necho \"${ ls; }\""
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

-- |
-- prop> verify checkMultipleAppends "foo >> file; bar >> file; baz >> file;"
-- prop> verify checkMultipleAppends "foo >> file; bar | grep f >> file; baz >> file;"
-- prop> verifyNot checkMultipleAppends "foo < file; bar < file; baz < file;"
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


-- |
-- prop> verify checkSuspiciousIFS "IFS=\"\\n\""
-- prop> verifyNot checkSuspiciousIFS "IFS=$'\\t'"
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


-- |
-- prop> verify checkShouldUseGrepQ "[[ $(foo | grep bar) ]]"
-- prop> verify checkShouldUseGrepQ "[ -z $(fgrep lol) ]"
-- prop> verify checkShouldUseGrepQ "[ -n \"$(foo | zgrep lol)\" ]"
-- prop> verifyNot checkShouldUseGrepQ "[ -z $(grep bar | cmd) ]"
-- prop> verifyNot checkShouldUseGrepQ "rm $(ls | grep file)"
-- prop> verifyNot checkShouldUseGrepQ "[[ -n $(pgrep foo) ]]"
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

-- |
-- prop> verify checkTestArgumentSplitting "[ -e *.mp3 ]"
-- prop> verifyNot checkTestArgumentSplitting "[[ $a == *b* ]]"
-- prop> verify checkTestArgumentSplitting "[[ *.png == '' ]]"
-- prop> verify checkTestArgumentSplitting "[[ foo == f{o,oo,ooo} ]]"
-- prop> verify checkTestArgumentSplitting "[[ $@ ]]"
-- prop> verify checkTestArgumentSplitting "[ -e $@ ]"
-- prop> verify checkTestArgumentSplitting "[ $@ == $@ ]"
-- prop> verify checkTestArgumentSplitting "[[ $@ = $@ ]]"
-- prop> verifyNot checkTestArgumentSplitting "[[ foo =~ bar{1,2} ]]"
-- prop> verifyNot checkTestArgumentSplitting "[ \"$@\" ]"
-- prop> verify checkTestArgumentSplitting "[[ \"$@\" ]]"
-- prop> verify checkTestArgumentSplitting "[ *.png ]"
-- prop> verify checkTestArgumentSplitting "[ \"$@\" == \"\" ]"
-- prop> verify checkTestArgumentSplitting "[[ \"$@\" == \"\" ]]"
-- prop> verifyNot checkTestArgumentSplitting "[[ \"$*\" == \"\" ]]"
-- prop> verifyNot checkTestArgumentSplitting "[[ -v foo[123] ]]"
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


-- |
-- prop> verify checkMaskedReturns "f() { local a=$(false); }"
-- prop> verify checkMaskedReturns "declare a=$(false)"
-- prop> verify checkMaskedReturns "declare a=\"`false`\""
-- prop> verifyNot checkMaskedReturns "declare a; a=$(false)"
-- prop> verifyNot checkMaskedReturns "f() { local -r a=$(false); }"
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


-- |
-- prop> verify checkReadWithoutR "read -a foo"
-- prop> verifyNot checkReadWithoutR "read -ar foo"
checkReadWithoutR _ t@T_SimpleCommand {} | t `isUnqualifiedCommand` "read" =
    unless ("r" `elem` map snd (getAllFlags t)) $
        info (getId $ getCommandTokenOrThis t) 2162 "read without -r will mangle backslashes."
checkReadWithoutR _ _ = return ()

-- |
-- prop> verifyTree checkUncheckedCdPushdPopd "cd ~/src; rm -r foo"
-- prop> verifyNotTree checkUncheckedCdPushdPopd "cd ~/src || exit; rm -r foo"
-- prop> verifyNotTree checkUncheckedCdPushdPopd "set -e; cd ~/src; rm -r foo"
-- prop> verifyNotTree checkUncheckedCdPushdPopd "if cd foo; then rm foo; fi"
-- prop> verifyTree checkUncheckedCdPushdPopd "if true; then cd foo; fi"
-- prop> verifyNotTree checkUncheckedCdPushdPopd "cd .."
-- prop> verifyNotTree checkUncheckedCdPushdPopd "#!/bin/bash -e\ncd foo\nrm bar"
-- prop> verifyNotTree checkUncheckedCdPushdPopd "set -o errexit; cd foo; rm bar"
-- prop> verifyTree checkUncheckedCdPushdPopd "builtin cd ~/src; rm -r foo"
-- prop> verifyTree checkUncheckedCdPushdPopd "pushd ~/src; rm -r foo"
-- prop> verifyNotTree checkUncheckedCdPushdPopd "pushd ~/src || exit; rm -r foo"
-- prop> verifyNotTree checkUncheckedCdPushdPopd "set -e; pushd ~/src; rm -r foo"
-- prop> verifyNotTree checkUncheckedCdPushdPopd "if pushd foo; then rm foo; fi"
-- prop> verifyTree checkUncheckedCdPushdPopd "if true; then pushd foo; fi"
-- prop> verifyNotTree checkUncheckedCdPushdPopd "pushd .."
-- prop> verifyNotTree checkUncheckedCdPushdPopd "#!/bin/bash -e\npushd foo\nrm bar"
-- prop> verifyNotTree checkUncheckedCdPushdPopd "set -o errexit; pushd foo; rm bar"
-- prop> verifyNotTree checkUncheckedCdPushdPopd "pushd -n foo"
-- prop> verifyTree checkUncheckedCdPushdPopd "popd; rm -r foo"
-- prop> verifyNotTree checkUncheckedCdPushdPopd "popd || exit; rm -r foo"
-- prop> verifyNotTree checkUncheckedCdPushdPopd "set -e; popd; rm -r foo"
-- prop> verifyNotTree checkUncheckedCdPushdPopd "if popd; then rm foo; fi"
-- prop> verifyTree checkUncheckedCdPushdPopd "if true; then popd; fi"
-- prop> verifyTree checkUncheckedCdPushdPopd "popd"
-- prop> verifyNotTree checkUncheckedCdPushdPopd "#!/bin/bash -e\npopd\nrm bar"
-- prop> verifyNotTree checkUncheckedCdPushdPopd "set -o errexit; popd; rm bar"
-- prop> verifyNotTree checkUncheckedCdPushdPopd "popd -n foo"

checkUncheckedCdPushdPopd params root =
    if hasSetE params then
        []
    else execWriter $ doAnalysis checkElement root
  where
    checkElement t@T_SimpleCommand {} =
        when(name t `elem` ["cd", "pushd", "popd"]
            && not (isSafeDir t)
            && not (name t `elem` ["pushd", "popd"] && ("n" `elem` map snd (getAllFlags t)))
            && not (isCondition $ getPath (parentMap params) t)) $
                warn (getId t) 2164 "Use 'cd ... || exit' or 'cd ... || return' in case cd fails."
    checkElement _ = return ()
    name t = fromMaybe "" $ getCommandName t
    isSafeDir t = case oversimplify t of
          [_, ".."] -> True;
          _ -> False

-- |
-- prop> verify checkLoopVariableReassignment "for i in *; do for i in *.bar; do true; done; done"
-- prop> verify checkLoopVariableReassignment "for i in *; do for((i=0; i<3; i++)); do true; done; done"
-- prop> verifyNot checkLoopVariableReassignment "for i in *; do for j in *.bar; do true; done; done"
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
                        (TA_Variable _ var _ ) _])
                            _ _ _ -> return var
            _ -> fail "not loop"

-- |
-- prop> verify checkTrailingBracket "if -z n ]]; then true; fi "
-- prop> verifyNot checkTrailingBracket "if [[ -z n ]]; then true; fi "
-- prop> verify checkTrailingBracket "a || b ] && thing"
-- prop> verifyNot checkTrailingBracket "run [ foo ]"
-- prop> verifyNot checkTrailingBracket "run bar ']'"
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

-- |
-- prop> verify checkReturnAgainstZero "[ $? -eq 0 ]"
-- prop> verify checkReturnAgainstZero "[[ \"$?\" -gt 0 ]]"
-- prop> verify checkReturnAgainstZero "[[ 0 -ne $? ]]"
-- prop> verifyNot checkReturnAgainstZero "[[ $? -eq 4 ]]"
-- prop> verify checkReturnAgainstZero "[[ 0 -eq $? ]]"
-- prop> verifyNot checkReturnAgainstZero "[[ $R -eq 0 ]]"
-- prop> verify checkReturnAgainstZero "(( $? == 0 ))"
-- prop> verify checkReturnAgainstZero "(( $? ))"
-- prop> verify checkReturnAgainstZero "(( ! $? ))"
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

-- |
-- prop> verify checkRedirectedNowhere "> file"
-- prop> verify checkRedirectedNowhere "> file | grep foo"
-- prop> verify checkRedirectedNowhere "grep foo | > bar"
-- prop> verifyNot checkRedirectedNowhere "grep foo > bar"
-- prop> verifyNot checkRedirectedNowhere "foo | grep bar > baz"
-- prop> verifyNot checkRedirectedNowhere "var=$(value) 2> /dev/null"
-- prop> verifyNot checkRedirectedNowhere "var=$(< file)"
-- prop> verifyNot checkRedirectedNowhere "var=`< file`"
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


-- |
-- prop> verifyTree checkArrayAssignmentIndices "declare -A foo; foo=(bar)"
-- prop> verifyNotTree checkArrayAssignmentIndices "declare -a foo; foo=(bar)"
-- prop> verifyNotTree checkArrayAssignmentIndices "declare -A foo; foo=([i]=bar)"
-- prop> verifyTree checkArrayAssignmentIndices "typeset -A foo; foo+=(bar)"
-- prop> verifyTree checkArrayAssignmentIndices "arr=( [foo]= bar )"
-- prop> verifyTree checkArrayAssignmentIndices "arr=( [foo] = bar )"
-- prop> verifyTree checkArrayAssignmentIndices "arr=( var=value )"
-- prop> verifyNotTree checkArrayAssignmentIndices "arr=( [foo]=bar )"
-- prop> verifyNotTree checkArrayAssignmentIndices "arr=( [foo]=\"\" )"
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

-- |
-- prop> verify checkUnmatchableCases "case foo in bar) true; esac"
-- prop> verify checkUnmatchableCases "case foo-$bar in ??|*) true; esac"
-- prop> verify checkUnmatchableCases "case foo in foo) true; esac"
-- prop> verifyNot checkUnmatchableCases "case foo-$bar in foo*|*bar|*baz*) true; esac"
-- prop> verify checkUnmatchableCases "case $f in *.txt) true;; f??.txt) false;; esac"
-- prop> verifyNot checkUnmatchableCases "case $f in ?*) true;; *) false;; esac"
-- prop> verifyNot checkUnmatchableCases "case $f in $(x)) true;; asdf) false;; esac"
-- prop> verify checkUnmatchableCases "case $f in cow) true;; bar|cow) false;; esac"
-- prop> verifyNot checkUnmatchableCases "case $f in x) true;;& x) false;; esac"
checkUnmatchableCases _ t =
    case t of
        T_CaseExpression _ word list -> do
            -- Check all patterns for whether they can ever match
            let allpatterns  = concatMap snd3 list
            -- Check only the non-fallthrough branches for shadowing
            let breakpatterns = concatMap snd3 $ filter (\x -> fst3 x == CaseBreak) list

            if isConstant word
                then warn (getId word) 2194
                        "This word is constant. Did you forget the $ on a variable?"
                else  potentially $ do
                    pg <- wordToPseudoGlob word
                    return $ mapM_ (check pg) allpatterns

            let exactGlobs = tupMap wordToExactPseudoGlob breakpatterns
            let fuzzyGlobs = tupMap wordToPseudoGlob breakpatterns
            let dominators = zip exactGlobs (tails $ drop 1 fuzzyGlobs)

            mapM_ checkDoms dominators

        _ -> return ()
  where
    fst3 (x,_,_) = x
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


-- |
-- prop> verify checkSubshellAsTest "( -e file )"
-- prop> verify checkSubshellAsTest "( 1 -gt 2 )"
-- prop> verifyNot checkSubshellAsTest "( grep -c foo bar )"
-- prop> verifyNot checkSubshellAsTest "[ 1 -gt 2 ]"
-- prop> verify checkSubshellAsTest "( -e file && -x file )"
-- prop> verify checkSubshellAsTest "( -e file || -x file && -t 1 )"
-- prop> verify checkSubshellAsTest "( ! -d file )"
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


-- |
-- prop> verify checkSplittingInArrays "a=( $var )"
-- prop> verify checkSplittingInArrays "a=( $(cmd) )"
-- prop> verifyNot checkSplittingInArrays "a=( \"$var\" )"
-- prop> verifyNot checkSplittingInArrays "a=( \"$(cmd)\" )"
-- prop> verifyNot checkSplittingInArrays "a=( $! $$ $# )"
-- prop> verifyNot checkSplittingInArrays "a=( ${#arr[@]} )"
-- prop> verifyNot checkSplittingInArrays "a=( foo{1,2} )"
-- prop> verifyNot checkSplittingInArrays "a=( * )"
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


-- |
-- prop> verify checkRedirectionToNumber "( 1 > 2 )"
-- prop> verify checkRedirectionToNumber "foo 1>2"
-- prop> verifyNot checkRedirectionToNumber "echo foo > '2'"
-- prop> verifyNot checkRedirectionToNumber "foo 1>&2"
checkRedirectionToNumber _ t = case t of
    T_IoFile id _ word -> potentially $ do
        file <- getUnquotedLiteral word
        guard $ all isDigit file
        return $ warn id 2210 "This is a file redirection. Was it supposed to be a comparison or fd operation?"
    _ -> return ()

-- |
-- prop> verify checkGlobAsCommand "foo*"
-- prop> verify checkGlobAsCommand "$(var[i])"
-- prop> verifyNot checkGlobAsCommand "echo foo*"
checkGlobAsCommand _ t = case t of
    T_SimpleCommand _ _ (first:_) ->
        when (isGlob first) $
            warn (getId first) 2211 "This is a glob used as a command name. Was it supposed to be in ${..}, array, or is it missing quoting?"
    _ -> return ()


-- |
-- prop> verify checkFlagAsCommand "-e file"
-- prop> verify checkFlagAsCommand "foo\n  --bar=baz"
-- prop> verifyNot checkFlagAsCommand "'--myexec--' args"
-- prop> verifyNot checkFlagAsCommand "var=cmd --arg"  -- Handled by SC2037
checkFlagAsCommand _ t = case t of
    T_SimpleCommand _ [] (first:_) ->
        when (isUnquotedFlag first) $
            warn (getId first) 2215 "This flag is used as a command name. Bad line break or missing [ .. ]?"
    _ -> return ()


-- |
-- prop> verify checkEmptyCondition "if [ ]; then ..; fi"
-- prop> verifyNot checkEmptyCondition "[ foo -o bar ]"
checkEmptyCondition _ t = case t of
    TC_Empty id _ -> style id 2212 "Use 'false' instead of empty [/[[ conditionals."
    _ -> return ()

-- |
-- prop> verify checkPipeToNowhere "foo | echo bar"
-- prop> verify checkPipeToNowhere "basename < file.txt"
-- prop> verify checkPipeToNowhere "printf 'Lol' <<< str"
-- prop> verify checkPipeToNowhere "printf 'Lol' << eof\nlol\neof\n"
-- prop> verifyNot checkPipeToNowhere "echo foo | xargs du"
-- prop> verifyNot checkPipeToNowhere "ls | echo $(cat)"
-- prop> verifyNot checkPipeToNowhere "echo foo | var=$(cat) ls"
-- prop> verify checkPipeToNowhere "foo | true"
-- prop> verifyNot checkPipeToNowhere "mv -i f . < /dev/stdin"
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
        -- Confusing echo for cat is so common that it's worth a special case
        let suggestion =
                if name == "echo"
                then "Did you want 'cat' instead?"
                else "Wrong command or missing xargs?"
        return $ warn (getId cmd) 2216 $
            "Piping to '" ++ name ++ "', a command that doesn't read stdin. " ++ suggestion

    checkRedir cmd = potentially $ do
        name <- getCommandBasename cmd
        guard $ name `elem` nonReadingCommands
        guard . not $ hasAdditionalConsumers cmd
        guard . not $ name `elem` ["cp", "mv", "rm"] && cmd `hasFlag` "i"
        let suggestion =
                if name == "echo"
                then "Did you want 'cat' instead?"
                else "Bad quoting, wrong command or missing xargs?"
        return $ warn (getId cmd) 2217 $
            "Redirecting to '" ++ name ++ "', a command that doesn't read stdin. " ++ suggestion

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

-- |
-- prop> verifyTree checkUseBeforeDefinition "f; f() { true; }"
-- prop> verifyNotTree checkUseBeforeDefinition "f() { true; }; f"
-- prop> verifyNotTree checkUseBeforeDefinition "if ! mycmd --version; then mycmd() { true; }; fi"
-- prop> verifyNotTree checkUseBeforeDefinition "mycmd || mycmd() { f; }"
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

-- |
-- prop> verify checkForLoopGlobVariables "for i in $var/*.txt; do true; done"
-- prop> verifyNot checkForLoopGlobVariables "for i in \"$var\"/*.txt; do true; done"
-- prop> verifyNot checkForLoopGlobVariables "for i in $var; do true; done"
checkForLoopGlobVariables _ t =
    case t of
        T_ForIn _ _ words _ -> mapM_ check words
        _ -> return ()
  where
    check (T_NormalWord _ parts) =
        when (any isGlob parts) $
            mapM_ suggest $ filter isQuoteableExpansion parts
    suggest t = info (getId t) 2231
        "Quote expansions in this for loop glob to prevent wordsplitting, e.g. \"$dir\"/*.txt ."

-- |
-- prop> verify checkSubshelledTests "a && ( [ b ] || ! [ c ] )"
-- prop> verify checkSubshelledTests "( [ a ] )"
-- prop> verify checkSubshelledTests "( [ a ] && [ b ] || test c )"
-- prop> verify checkSubshelledTests "( [ a ] && { [ b ] && [ c ]; } )"
checkSubshelledTests params t =
    case t of
        T_Subshell id list | all isTestStructure list ->
            case () of
                -- Special case for if (test) and while (test)
                _ | isCompoundCondition (getPath (parentMap params) t) ->
                    style id 2233 "Remove superfluous (..) around condition."

                -- Special case for ([ x ])
                _ | isSingleTest list ->
                    style id 2234 "Remove superfluous (..) around test command."

                -- General case for ([ x ] || [ y ] && etc)
                _ -> style id 2235 "Use { ..; } instead of (..) to avoid subshell overhead."
        _ -> return ()
  where

    isSingleTest cmds =
        case cmds of
            [c] | isTestCommand c -> True
            _ -> False

    isTestStructure t =
        case t of
            T_Banged _ t -> isTestStructure t
            T_AndIf _ a b -> isTestStructure a && isTestStructure b
            T_OrIf  _ a b -> isTestStructure a && isTestStructure b
            T_Pipeline _ [] [T_Redirecting _ _ cmd] ->
                case cmd of
                    T_BraceGroup _ ts -> all isTestStructure ts
                    T_Subshell   _ ts -> all isTestStructure ts
                    _ -> isTestCommand t
            _ -> isTestCommand t

    isTestCommand t =
        case t of
            T_Pipeline _ [] [T_Redirecting _ _ cmd] ->
                case cmd of
                    T_Condition {} -> True
                    _ -> cmd `isCommand` "test"
            _ -> False

    -- Check if a T_Subshell is used as a condition, e.g. if ( test )
    -- This technically also triggers for `if true; then ( test ); fi`
    -- but it's still a valid suggestion.
    isCompoundCondition chain =
        case dropWhile skippable (drop 1 chain) of
            T_IfExpression {}    : _ -> True
            T_WhileExpression {} : _ -> True
            T_UntilExpression {} : _ -> True
            _ -> False

    -- Skip any parent of a T_Subshell until we reach something interesting
    skippable t =
        case t of
            T_Redirecting _ [] _ -> True
            T_Pipeline _ [] _ -> True
            T_Annotation {} -> True
            _ -> False

-- |
-- prop> verify checkInvertedStringTest "[ ! -z $var ]"
-- prop> verify checkInvertedStringTest "! [[ -n $var ]]"
-- prop> verifyNot checkInvertedStringTest "! [ -x $var ]"
-- prop> verifyNot checkInvertedStringTest "[[ ! -w $var ]]"
-- prop> verifyNot checkInvertedStringTest "[ -z $var ]"
checkInvertedStringTest _ t =
    case t of
        TC_Unary _ _ "!" (TC_Unary _ _ op _) ->
            case op of
                "-n" -> style (getId t) 2236 "Use -z instead of ! -n."
                "-z" -> style (getId t) 2236 "Use -n instead of ! -z."
                _ -> return ()
        T_Banged _ (T_Pipeline _ _
          [T_Redirecting _ _ (T_Condition _ _ (TC_Unary _ _ op _))]) ->
            case op of
                "-n" -> style (getId t) 2237 "Use [ -z .. ] instead of ! [ -n .. ]."
                "-z" -> style (getId t) 2237 "Use [ -n .. ] instead of ! [ -z .. ]."
                _ -> return ()
        _ -> return ()
