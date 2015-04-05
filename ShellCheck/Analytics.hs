{-
    This file is part of ShellCheck.
    http://www.vidarholen.net/contents/shellcheck

    ShellCheck is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    ShellCheck is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module ShellCheck.Analytics (AnalysisOptions(..), defaultAnalysisOptions, filterByAnnotation, runAnalytics, shellForExecutable, runTests) where

import Control.Arrow (first)
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.Char
import Data.Functor
import Data.Function (on)
import Data.List
import Data.Maybe
import Data.Ord
import Debug.Trace
import ShellCheck.AST
import ShellCheck.Options
import ShellCheck.Data
import ShellCheck.Parser hiding (runTests)
import ShellCheck.Regex
import qualified Data.Map as Map
import Test.QuickCheck.All (forAllProperties)
import Test.QuickCheck.Test (quickCheckWithResult, stdArgs, maxSuccess)

data Parameters = Parameters {
    variableFlow :: [StackData],
    parentMap :: Map.Map Id Token,
    shellType :: Shell,
    shellTypeSpecified :: Bool
    }

-- Checks that are run on the AST root
treeChecks :: [Parameters -> Token -> [Note]]
treeChecks = [
    runNodeAnalysis
        (\p t -> (mapM_ ((\ f -> f t) . (\ f -> f p))
            (nodeChecks ++ checksFor (shellType p))))
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
    ]

checksFor Sh = [
    checkBashisms
    ,checkTimeParameters
    ,checkForDecimals
    ]
checksFor Ksh = [
    checkEchoSed
    ]
checksFor Bash = [
    checkTimeParameters
    ,checkBraceExpansionVars
    ,checkEchoSed
    ,checkForDecimals
    ]

runAnalytics :: AnalysisOptions -> Token -> [Note]
runAnalytics options root = runList options root treeChecks

runList options root list = notes
    where
        params = Parameters {
                shellType = fromMaybe (determineShell root) $ optionShellType options,
                shellTypeSpecified = isJust $ optionShellType options,
                parentMap = getParentTree root,
                variableFlow = getVariableFlow (shellType params) (parentMap params) root
        }
        notes = filter (\c -> getCode c `notElem` optionExcludes options) $ concatMap (\f -> f params root) list
        getCode (Note _ _ c _) = c


checkList l t = concatMap (\f -> f t) l

prop_determineShell0 = determineShell (T_Script (Id 0) "#!/bin/sh" []) == Sh
prop_determineShell1 = determineShell (T_Script (Id 0) "#!/usr/bin/env ksh" []) == Ksh
prop_determineShell2 = determineShell (T_Script (Id 0) "" []) == Bash
prop_determineShell3 = determineShell (T_Script (Id 0) "#!/bin/sh -e" []) == Sh
determineShell (T_Script _ shebang _) = fromMaybe Bash . shellForExecutable $ shellFor shebang
    where shellFor s | "/env " `isInfixOf` s = head (drop 1 (words s)++[""])
          shellFor s | ' ' `elem` s = shellFor $ takeWhile (/= ' ') s
          shellFor s = reverse . takeWhile (/= '/') . reverse $ s

shellForExecutable "sh" = return Sh
shellForExecutable "ash" = return Sh
shellForExecutable "dash" = return Sh

shellForExecutable "ksh" = return Ksh
shellForExecutable "ksh88" = return Ksh
shellForExecutable "ksh93" = return Ksh

shellForExecutable "bash" = return Bash
shellForExecutable _ = Nothing

-- Checks that are run on each node in the AST
runNodeAnalysis f p t = execWriter (doAnalysis (f p) t)

nodeChecks :: [Parameters -> Token -> Writer [Note] ()]
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
    ,checkNoaryWasBinary
    ,checkConstantNoary
    ,checkDivBeforeMult
    ,checkArithmeticDeref
    ,checkArithmeticBadOctal
    ,checkComparisonAgainstGlob
    ,checkPrintfVar
    ,checkCommarrays
    ,checkOrNeq
    ,checkEchoWc
    ,checkConstantIfs
    ,checkTr
    ,checkPipedAssignment
    ,checkAssignAteCommand
    ,checkUuoeCmd
    ,checkUuoeVar
    ,checkFindNameGlob
    ,checkGrepRe
    ,checkNeedlessCommands
    ,checkQuotedCondRegex
    ,checkForInCat
    ,checkFindExec
    ,checkValidCondOps
    ,checkGlobbedRegex
    ,checkTrapQuotes
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
    ,checkUnusedEchoEscapes
    ,checkDollarBrackets
    ,checkSshHereDoc
    ,checkSshCommandString
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
    ,checkCatastrophicRm
    ,checkInteractiveSu
    ,checkStderrPipe
    ,checkSetAssignment
    ,checkOverridingPath
    ,checkArrayAsString
    ,checkUnsupported
    ,checkMultipleAppends
    ,checkAliasesExpandEarly
    ,checkSuspiciousIFS
    ,checkAliasesUsesArgs
    ,checkShouldUseGrepQ
    ,checkTestGlobs
    ,checkConcatenatedDollarAt
    ,checkFindActionPrecedence
    ,checkTildeInPath
    ,checkFindExecWithSingleArgument
    ,checkReturn
    ,checkMaskedReturns
    ]


filterByAnnotation token =
    filter (not . shouldIgnore)
  where
    numFor (Note _ _ code _) = code
    idFor (Note id  _ _ _) = id
    shouldIgnore note =
        any (shouldIgnoreFor (numFor note)) $
            getPath parents (T_Bang $ idFor note)
    shouldIgnoreFor num (T_Annotation _ anns _) =
        any hasNum anns
      where
        hasNum (DisableComment ts) = num == ts
    shouldIgnoreFor _ _ = False
    parents = getParentTree token

addNote note = tell [note]
makeNote severity id code note = addNote $ Note id severity code note
warn = makeNote WarningC
err = makeNote ErrorC
info = makeNote InfoC
style = makeNote StyleC

isVariableStartChar x = x == '_' || isAsciiLower x || isAsciiUpper x
isVariableChar x = isVariableStartChar x || isDigit x
variableNameRegex = mkRegex "[_a-zA-Z][_a-zA-Z0-9]*"

prop_isVariableName1 = isVariableName "_fo123"
prop_isVariableName2 = not $ isVariableName "4"
prop_isVariableName3 = not $ isVariableName "test: "
isVariableName (x:r) = isVariableStartChar x && all isVariableChar r
isVariableName _ = False

potentially = fromMaybe (return ())

willSplit x =
  case x of
    T_DollarBraced {} -> True
    T_DollarExpansion {} -> True
    T_Backticked {} -> True
    T_BraceExpansion {} -> True
    T_Glob {} -> True
    T_Extglob {} -> True
    T_NormalWord _ l -> any willSplit l
    _ -> False

isGlob (T_Extglob {}) = True
isGlob (T_Glob {}) = True
isGlob (T_NormalWord _ l) = any isGlob l
isGlob _ = False

wouldHaveBeenGlob s = '*' `elem` s

isConfusedGlobRegex ('*':_) = True
isConfusedGlobRegex [x,'*'] | x /= '\\' = True
isConfusedGlobRegex _ = False

getSuspiciousRegexWildcard str =
    if not $ str `matches` contra
    then do
        match <- matchRegex suspicious str
        str <- match !!! 0
        str !!! 0
    else
        fail "looks good"
  where
    suspicious = mkRegex "([A-Za-z1-9])\\*"
    contra = mkRegex "[^a-zA-Z1-9]\\*|[][^$+\\\\]"

headOrDefault _ (a:_) = a
headOrDefault def _ = def

isConstant token =
    case token of
        T_NormalWord _ l   -> all isConstant l
        T_DoubleQuoted _ l -> all isConstant l
        T_SingleQuoted _ _ -> True
        T_Literal _ _ -> True
        _ -> False

isEmpty token =
    case token of
        T_NormalWord _ l   -> all isEmpty l
        T_DoubleQuoted _ l -> all isEmpty l
        T_SingleQuoted _ "" -> True
        T_Literal _ "" -> True
        _ -> False

makeSimple (T_NormalWord _ [f]) = f
makeSimple (T_Redirecting _ _ f) = f
makeSimple (T_Annotation _ _ f) = f
makeSimple t = t
simplify = doTransform makeSimple

deadSimple (T_NormalWord _ l) = [concat (concatMap deadSimple l)]
deadSimple (T_DoubleQuoted _ l) = [concat (concatMap deadSimple l)]
deadSimple (T_SingleQuoted _ s) = [s]
deadSimple (T_DollarBraced _ _) = ["${VAR}"]
deadSimple (T_DollarArithmetic _ _) = ["${VAR}"]
deadSimple (T_DollarExpansion _ _) = ["${VAR}"]
deadSimple (T_Backticked _ _) = ["${VAR}"]
deadSimple (T_Glob _ s) = [s]
deadSimple (T_Pipeline _ _ [x]) = deadSimple x
deadSimple (T_Literal _ x) = [x]
deadSimple (T_SimpleCommand _ vars words) = concatMap deadSimple words
deadSimple (T_Redirecting _ _ foo) = deadSimple foo
deadSimple (T_DollarSingleQuoted _ s) = [s]
deadSimple (T_Annotation _ _ s) = deadSimple s
-- Workaround for let "foo = bar" parsing
deadSimple (TA_Sequence _ [TA_Expansion _ v]) = concatMap deadSimple v
deadSimple _ = []

-- Turn a SimpleCommand foo -avz --bar=baz into args ["a", "v", "z", "bar"]
getFlagsUntil stopCondition (T_SimpleCommand _ _ (_:args)) =
    let textArgs = takeWhile (not . stopCondition . snd) $ map (\x -> (x, concat $ deadSimple x)) args in
        concatMap flag textArgs
  where
    flag (x, ('-':'-':arg)) = [ (x, takeWhile (/= '=') arg) ]
    flag (x, ('-':args)) = map (\v -> (x, [v])) args
    flag _ = []

getFlagsUntil _ _ = error "Internal shellcheck error, please report! (getFlags on non-command)"

getAllFlags = getFlagsUntil (== "--")
getLeadingFlags = getFlagsUntil (not . ("-" `isPrefixOf`))

(!!!) list i =
    case drop i list of
        [] -> Nothing
        (r:_) -> Just r

verify :: (Parameters -> Token -> Writer [Note] ()) -> String -> Bool
verify f s = checkNode f s == Just True

verifyNot :: (Parameters -> Token -> Writer [Note] ()) -> String -> Bool
verifyNot f s = checkNode f s == Just False

verifyTree :: (Parameters -> Token -> [Note]) -> String -> Bool
verifyTree f s = checkTree f s == Just True

verifyNotTree :: (Parameters -> Token -> [Note]) -> String -> Bool
verifyNotTree f s = checkTree f s == Just False

checkNode f = checkTree (runNodeAnalysis f)
checkTree f s = case parseShell defaultAnalysisOptions "-" s of
        (ParseResult (Just (t, m)) _) -> Just . not . null $ runList defaultAnalysisOptions t [f]
        _ -> Nothing


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
                    doDiag (ach:as) (bch:bs) nw n w = me : (doDiag as bs me (tail n) (tail w))
                        where me = if ach == bch then nw else 1 + min3 (head w) nw (head n)
                    firstelt = 1 + head diagBelow
                    thisdiag = firstelt : doDiag a b firstelt diagAbove (tail diagBelow)
          lab = length a - length b
          min3 x y z = if x < y then x else min y z


prop_checkEchoWc3 = verify checkEchoWc "n=$(echo $foo | wc -c)"
checkEchoWc _ (T_Pipeline id _ [a, b]) =
    when (acmd == ["echo", "${VAR}"]) $
        case bcmd of
            ["wc", "-c"] -> countMsg
            ["wc", "-m"] -> countMsg
            _ -> return ()
  where
    acmd = deadSimple a
    bcmd = deadSimple b
    countMsg = style id 2000 "See if you can use ${#variable} instead."
checkEchoWc _ _ = return ()

prop_checkEchoSed1 = verify checkEchoSed "FOO=$(echo \"$cow\" | sed 's/foo/bar/g')"
prop_checkEchoSed2 = verify checkEchoSed "rm $(echo $cow | sed -e 's,foo,bar,')"
checkEchoSed _ (T_Pipeline id _ [a, b]) =
    when (acmd == ["echo", "${VAR}"]) $
        case bcmd of
            ["sed", v] -> checkIn v
            ["sed", "-e", v] -> checkIn v
            _ -> return ()
  where
    -- This should have used backreferences, but TDFA doesn't support them
    sedRe = mkRegex "^s(.)([^\n]*)g?$"
    isSimpleSed s = fromMaybe False $ do
        [first,rest] <- matchRegex sedRe s
        let delimiters = filter (== (first !! 0)) rest
        guard $ length delimiters == 2
        return True

    acmd = deadSimple a
    bcmd = deadSimple b
    checkIn s =
        when (isSimpleSed s) $
            style id 2001 "See if you can use ${variable//search/replace} instead."
checkEchoSed _ _ = return ()

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
prop_checkAssignAteCommand5 = verifyNot checkAssignAteCommand "PAGER=cat grep bar"
checkAssignAteCommand _ (T_SimpleCommand id (T_Assignment _ _ _ _ assignmentTerm:[]) (firstWord:_)) =
    when ("-" `isPrefixOf` concat (deadSimple firstWord) ||
        isCommonCommand (getLiteralString assignmentTerm)
            && not (isCommonCommand (getLiteralString firstWord))) $
                warn id 2037 "To assign the output of a command, use var=$(cmd) ."
  where
    isCommonCommand (Just s) = s `elem` commonCommands
    isCommonCommand _ = False
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
checkUuoc _ (T_Pipeline _ _ (T_Redirecting _ _ cmd:_:_)) =
    checkCommand "cat" (const f) cmd
  where
    f [word] = unless (mayBecomeMultipleArgs word) $
        style (getId word) 2002 "Useless cat. Consider 'cmd < file | ..' or 'cmd file | ..' instead."
    f _ = return ()
checkUuoc _ _ = return ()

prop_checkNeedlessCommands = verify checkNeedlessCommands "foo=$(expr 3 + 2)"
prop_checkNeedlessCommands2 = verify checkNeedlessCommands "foo=`echo \\`expr 3 + 2\\``"
prop_checkNeedlessCommands3 = verifyNot checkNeedlessCommands "foo=$(expr foo : regex)"
prop_checkNeedlessCommands4 = verifyNot checkNeedlessCommands "foo=$(expr foo \\< regex)"
checkNeedlessCommands _ cmd@(T_SimpleCommand id _ args) |
        cmd `isCommand` "expr" && not (any (`elem` words) exceptions) =
    style id 2003 "expr is antiquated. Consider rewriting this using $((..)), ${} or [[ ]]."
  where
    -- These operators are hard to replicate in POSIX
    exceptions = [ ":", "<", ">", "<=", ">=" ]
    words = mapMaybe getLiteralString args
checkNeedlessCommands _ _ = return ()

prop_checkPipePitfalls3 = verify checkPipePitfalls "ls | grep -v mp3"
prop_checkPipePitfalls4 = verifyNot checkPipePitfalls "find . -print0 | xargs -0 foo"
prop_checkPipePitfalls5 = verifyNot checkPipePitfalls "ls -N | foo"
prop_checkPipePitfalls6 = verify checkPipePitfalls "find . | xargs foo"
prop_checkPipePitfalls7 = verifyNot checkPipePitfalls "find . -printf '%s\\n' | xargs foo"
checkPipePitfalls _ (T_Pipeline id _ commands) = do
    for ["find", "xargs"] $
        \(find:xargs:_) ->
          let args = deadSimple xargs ++ deadSimple find
          in
            unless (any ($ args) [
                hasShortParameter '0',
                hasParameter "null",
                hasParameter "print0",
                hasParameter "printf"
              ]) $ warn (getId find) 2038
                      "Use -print0/-0 or -exec + to allow for non-alphanumeric filenames."

    for ["?", "echo"] $
        \(_:echo:_) -> info (getId echo) 2008 "echo doesn't read from stdin, are you sure you should be piping to it?"

    for' ["ps", "grep"] $
        \x -> info x 2009 "Consider using pgrep instead of grepping ps output."

    for' ["grep", "wc"] $
        \x -> style x 2126 "Consider using grep -c instead of grep|wc."

    didLs <- liftM or . sequence $ [
        for' ["ls", "grep"] $
            \x -> warn x 2010 "Don't use ls | grep. Use a glob or a for loop with a condition to allow non-alphanumeric filenames.",
        for' ["ls", "xargs"] $
            \x -> warn x 2011 "Use 'find .. -print0 | xargs -0 ..' or 'find .. -exec .. +' to allow non-alphanumeric filenames."
        ]
    unless didLs $ do
        for ["ls", "?"] $
            \(ls:_) -> unless (hasShortParameter 'N' (deadSimple ls)) $
                info (getId ls) 2012 "Use find instead of ls to better handle non-alphanumeric filenames."
        return ()
  where
    for l f =
        let indices = indexOfSublists l (map (headOrDefault "" . deadSimple) commands)
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


bracedString l = concat $ deadSimple l

isArrayExpansion (T_DollarBraced _ l) =
    let string = bracedString l in
        "@" `isPrefixOf` string ||
            not ("#" `isPrefixOf` string) && "[@]" `isInfixOf` string
isArrayExpansion _ = False

-- Is it certain that this arg will becomes multiple args?
willBecomeMultipleArgs t = willConcatInAssignment t || f t
  where
    f (T_Extglob {}) = True
    f (T_Glob {}) = True
    f (T_BraceExpansion {}) = True
    f (T_DoubleQuoted _ parts) = any f parts
    f (T_NormalWord _ parts) = any f parts
    f _ = False

willConcatInAssignment t@(T_DollarBraced {}) = isArrayExpansion t
willConcatInAssignment (T_DoubleQuoted _ parts) = any willConcatInAssignment parts
willConcatInAssignment (T_NormalWord _ parts) = any willConcatInAssignment parts
willConcatInAssignment _ = False

-- Is it possible that this arg becomes multiple args?
mayBecomeMultipleArgs t = willBecomeMultipleArgs t || f t
  where
    f (T_DollarBraced _ l) =
        let string = bracedString l in
            "!" `isPrefixOf` string
    f (T_DoubleQuoted _ parts) = any f parts
    f (T_NormalWord _ parts) = any f parts
    f _ = False

prop_checkShebangParameters1 = verifyTree checkShebangParameters "#!/usr/bin/env bash -x\necho cow"
prop_checkShebangParameters2 = verifyNotTree checkShebangParameters "#! /bin/sh  -l "
checkShebangParameters _ (T_Script id sb _) =
    [Note id ErrorC 2096 "On most OS, shebangs can only specify a single parameter." | length (words sb) > 2]

prop_checkShebang1 = verifyNotTree checkShebang "#!/usr/bin/env bash -x\necho cow"
prop_checkShebang2 = verifyNotTree checkShebang "#! /bin/sh  -l "
prop_checkShebang3 = verifyTree checkShebang "ls -l"
checkShebang params (T_Script id sb _) =
    [Note id ErrorC 2148 "Tips depend on target shell and yours is unknown. Add a shebang."
        | not (shellTypeSpecified params) && sb == "" ]

prop_checkBashisms = verify checkBashisms "while read a; do :; done < <(a)"
prop_checkBashisms2 = verify checkBashisms "[ foo -nt bar ]"
prop_checkBashisms3 = verify checkBashisms "echo $((i++))"
prop_checkBashisms4 = verify checkBashisms "rm !(*.hs)"
prop_checkBashisms5 = verify checkBashisms "source file"
prop_checkBashisms6 = verify checkBashisms "[ \"$a\" == 42 ]"
prop_checkBashisms7 = verify checkBashisms "echo ${var[1]}"
prop_checkBashisms8 = verify checkBashisms "echo ${!var[@]}"
prop_checkBashisms9 = verify checkBashisms "echo ${!var*}"
prop_checkBashisms10= verify checkBashisms "echo ${var:4:12}"
prop_checkBashisms11= verifyNot checkBashisms "echo ${var:-4}"
prop_checkBashisms12= verify checkBashisms "echo ${var//foo/bar}"
prop_checkBashisms13= verify checkBashisms "exec -c env"
prop_checkBashisms14= verify checkBashisms "echo -n \"Foo: \""
prop_checkBashisms15= verify checkBashisms "let n++"
prop_checkBashisms16= verify checkBashisms "echo $RANDOM"
prop_checkBashisms17= verify checkBashisms "echo $((RANDOM%6+1))"
prop_checkBashisms18= verify checkBashisms "foo &> /dev/null"
prop_checkBashisms19= verify checkBashisms "foo > file*.txt"
prop_checkBashisms20= verify checkBashisms "read -ra foo"
prop_checkBashisms21= verify checkBashisms "[ -a foo ]"
prop_checkBashisms22= verifyNot checkBashisms "[ foo -a bar ]"
prop_checkBashisms23= verify checkBashisms "trap mything err int"
prop_checkBashisms24= verifyNot checkBashisms "trap mything int term"
prop_checkBashisms25= verify checkBashisms "cat < /dev/tcp/host/123"
prop_checkBashisms26= verify checkBashisms "trap mything ERR SIGTERM"
checkBashisms _ = bashism
  where
    errMsg id s = err id 2040 $ "In sh, " ++ s ++ " not supported, even when sh is actually bash."
    warnMsg id s = warn id 2039 $ "In POSIX sh, " ++ s ++ " not supported."
    bashism (T_ProcSub id _ _) = errMsg id "process substitution is"
    bashism (T_Extglob id _ _) = warnMsg id "extglob is"
    bashism (T_DollarSingleQuoted id _) = warnMsg id "$'..' is"
    bashism (T_DollarDoubleQuoted id _) = warnMsg id "$\"..\" is"
    bashism (T_ForArithmetic id _ _ _ _) = warnMsg id "arithmetic for loops are"
    bashism (T_Arithmetic id _) = warnMsg id "standalone ((..)) is"
    bashism (T_DollarBracket id _) = warnMsg id "$[..] in place of $((..)) is"
    bashism (T_SelectIn id _ _ _) = warnMsg id "select loops are"
    bashism (T_BraceExpansion id _) = warnMsg id "brace expansion is"
    bashism (T_Condition id DoubleBracket _) = warnMsg id "[[ ]] is"
    bashism (T_HereString id _) = warnMsg id "here-strings are"
    bashism (TC_Binary id SingleBracket op _ _)
        | op `elem` [ "-nt", "-ef", "\\<", "\\>", "==" ] =
            warnMsg id $ op ++ " is"
    bashism (TC_Unary id _ "-a" _) =
            warnMsg id "unary -a in place of -e is"
    bashism (TA_Unary id op _)
        | op `elem` [ "|++", "|--", "++|", "--|"] =
            warnMsg id $ filter (/= '|') op ++ " is"
    bashism (TA_Binary id "**" _ _) = warnMsg id "exponentials are"
    bashism (T_FdRedirect id "&" (T_IoFile _ (T_Greater _) _)) = warnMsg id "&> is"
    bashism (T_IoFile id _ word) | isNetworked =
            warnMsg id "/dev/{tcp,udp} is"
        where
            file = onlyLiteralString word
            isNetworked = any (`isPrefixOf` file) ["/dev/tcp", "/dev/udp"]

    bashism t@(TA_Expansion id _) | isBashism =
        warnMsg id $ fromJust str ++ " is"
      where
        str = getLiteralString t
        isBashism = isJust str && fromJust str `elem` bashVars
    bashism t@(T_DollarBraced id token) = do
        mapM_ check expansion
        when (var `elem` bashVars) $ warnMsg id $ var ++ " is"
      where
        str = concat $ deadSimple token
        var = getBracedReference (bracedString token)
        check (regex, feature) =
            when (isJust $ matchRegex regex str) $ warnMsg id feature

    bashism t@(T_Pipe id "|&") =
        warnMsg id "|& in place of 2>&1 | is"
    bashism (T_Array id _) =
        warnMsg id "arrays are"
    bashism (T_IoFile id _ t) | isGlob t =
        warnMsg id "redirecting to/from globs is"
    bashism (T_CoProc id _ _) =
        warnMsg id "coproc is"

    bashism t@(T_SimpleCommand _ _ (cmd:arg:_))
        | t `isCommand` "echo" && "-" `isPrefixOf` argString =
            unless ("--" `isPrefixOf` argString) $ -- echo "-------"
                warnMsg (getId arg) "echo flags are"
      where argString = concat $ deadSimple arg
    bashism t@(T_SimpleCommand _ _ (cmd:arg:_))
        | t `isCommand` "exec" && "-" `isPrefixOf` concat (deadSimple arg) =
            warnMsg (getId arg) "exec flags are"
    bashism t@(T_SimpleCommand id _ _)
        | t `isCommand` "let" = warnMsg id "'let' is"

    bashism t@(T_SimpleCommand id _ (cmd:rest)) =
        let name = fromMaybe "" $ getCommandName t
            flags = getLeadingFlags t
        in do
            when (name `elem` bashCommands) $ warnMsg id $ "'" ++ name ++ "' is"
            potentially $ do
                allowed <- Map.lookup name allowedFlags
                (word, flag) <- listToMaybe $ filter (\x -> snd x `notElem` allowed) flags
                return . warnMsg (getId word) $ name ++ " -" ++ flag ++ " is"

            when (name == "source") $ warnMsg id "'source' in place of '.' is"
            when (name == "trap") $
                let
                    check token = potentially $ do
                        word <- liftM (map toLower) $ getLiteralString token
                        guard $ word `elem` ["err", "debug", "return"]
                        return $ warnMsg (getId token) $ "trapping " ++ word ++ " is"
                in
                    mapM_ check (reverse rest)
      where
        bashCommands = [
            "let", "caller", "builtin", "complete", "compgen", "declare", "dirs", "disown",
            "enable", "mapfile", "readarray", "pushd", "popd", "shopt", "suspend", "type",
            "typeset"
            ]
        allowedFlags = Map.fromList [
            ("read", ["r"]),
            ("ulimit", ["f"]),
            ("echo", []),
            ("exec", [])
            ]

    bashism _ = return ()

    varChars="_0-9a-zA-Z"
    expansion = let re = mkRegex in [
        (re $ "^[" ++ varChars ++ "]+\\[.*\\]$", "array references are"),
        (re $ "^![" ++ varChars ++ "]+\\[[*@]]$", "array key expansion is"),
        (re $ "^![" ++ varChars ++ "]+[*@]$", "name matching prefixes are"),
        (re $ "^[" ++ varChars ++ "]+:[^-=?+]", "string indexing is"),
        (re $ "^[" ++ varChars ++ "]+(\\[.*\\])?/", "string replacement is")
        ]
    bashVars = [
        "RANDOM", "LINENO", "OSTYPE", "MACHTYPE", "HOSTTYPE", "HOSTNAME",
        "DIRSTACK", "EUID", "UID", "SECONDS", "SHLVL", "PIPESTATUS", "SHELLOPTS"
        ]

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
            || (liftM wouldHaveBeenGlob (getLiteralString word) == Just True)) $
        err id 2066 "Since you double quoted this, it will not word split, and the loop will only run once."
checkForInQuoted _ (T_ForIn _ f [T_NormalWord _ [T_SingleQuoted id s]] _) =
    warn id 2041 $ "This is a literal string. To run as a command, use $(" ++ s ++ ")."
checkForInQuoted _ (T_ForIn _ f [T_NormalWord _ [T_Literal id s]] _) =
    if ',' `elem` s
      then unless ('{' `elem` s) $
            warn id 2042 "Use spaces, not commas, to separate loop elements."
      else warn id 2043 $ "This loop will only run once, with " ++ f ++ "='" ++ s ++ "'."
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
    case deadSimple x of
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
checkUnquotedExpansions params =
    check
  where
    check t@(T_DollarExpansion _ _) = examine t
    check t@(T_Backticked _ _) = examine t
    check _ = return ()
    tree = parentMap params
    examine t =
        unless (isQuoteFree tree t || usedAsCommandName tree t) $
            warn (getId t) 2046 "Quote this to prevent word splitting."


prop_checkRedirectToSame = verify checkRedirectToSame "cat foo > foo"
prop_checkRedirectToSame2 = verify checkRedirectToSame "cat lol | sed -e 's/a/b/g' > lol"
prop_checkRedirectToSame3 = verifyNot checkRedirectToSame "cat lol | sed -e 's/a/b/g' > foo.bar && mv foo.bar lol"
prop_checkRedirectToSame4 = verifyNot checkRedirectToSame "foo /dev/null > /dev/null"
prop_checkRedirectToSame5 = verifyNot checkRedirectToSame "foo > bar 2> bar"
checkRedirectToSame params s@(T_Pipeline _ _ list) =
    mapM_ (\l -> (mapM_ (\x -> doAnalysis (checkOccurrences x) l) (getAllRedirs list))) list
  where
    note x = Note x InfoC 2094
                "Make sure not to read and write the same file in the same pipeline."
    checkOccurrences t@(T_NormalWord exceptId x) u@(T_NormalWord newId y) =
        when (exceptId /= newId
                && x == y
                && not (isOutput t && isOutput u)
                && not (special t)) $ do
            addNote $ note newId
            addNote $ note exceptId
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
    special x = "/dev/" `isPrefixOf` concat (deadSimple x)
    isOutput t =
        case drop 1 $ getPath (parentMap params) t of
            T_IoFile _ op _:_ ->
                case op of
                    T_Greater _  -> True
                    T_DGREAT _ -> True
                    _ -> False
            _ -> False
checkRedirectToSame _ _ = return ()


prop_checkShorthandIf  = verify checkShorthandIf "[[ ! -z file ]] && scp file host || rm file"
prop_checkShorthandIf2 = verifyNot checkShorthandIf "[[ ! -z file ]] && { scp file host || echo 'Eek'; }"
prop_checkShorthandIf3 = verifyNot checkShorthandIf "foo && bar || echo baz"
prop_checkShorthandIf4 = verifyNot checkShorthandIf "foo && a=b || a=c"
checkShorthandIf _ (T_AndIf id _ (T_OrIf _ _ (T_Pipeline _ _ t)))
        | not $ isOk t =
    info id 2015 "Note that A && B || C is not if-then-else. C may run when A is true."
  where
    isOk [t] = isAssignment t || fromMaybe False (do
        name <- getCommandBasename t
        return $ name `elem` ["echo", "exit", "return"])
    isOk _ = False
checkShorthandIf _ _ = return ()


prop_checkDollarStar = verify checkDollarStar "for f in $*; do ..; done"
prop_checkDollarStar2 = verifyNot checkDollarStar "a=$*"
checkDollarStar p t@(T_NormalWord _ [T_DollarBraced id l])
      | bracedString l == "*"  =
    unless isAssigned $
        warn id 2048 "Use \"$@\" (with quotes) to prevent whitespace problems."
  where
    path = getPath (parentMap p) t
    isAssigned = any isAssignment . take 2 $ path
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
checkUnquotedDollarAt p word@(T_NormalWord _ parts) | not $ isStrictlyQuoteFree (parentMap p) word =
    forM_ (take 1 $ filter isArrayExpansion parts) $ \x ->
        err (getId x) 2068
            "Double quote array expansions, otherwise they're like $* and break on spaces."
checkUnquotedDollarAt _ _ = return ()

prop_checkConcatenatedDollarAt1 = verify checkConcatenatedDollarAt "echo \"foo$@\""
prop_checkConcatenatedDollarAt2 = verify checkConcatenatedDollarAt "echo ${arr[@]}lol"
prop_checkConcatenatedDollarAt3 = verify checkConcatenatedDollarAt "echo $a$@"
prop_checkConcatenatedDollarAt4 = verifyNot checkConcatenatedDollarAt "echo $@"
prop_checkConcatenatedDollarAt5 = verifyNot checkConcatenatedDollarAt "echo \"${arr[@]}\""
checkConcatenatedDollarAt p word@(T_NormalWord {})
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
checkArrayWithoutIndex params _ =
    concat $ doVariableFlowAnalysis readF writeF Map.empty (variableFlow params)
  where
    readF _ (T_DollarBraced id token) _ = do
        map <- get
        return . maybeToList $ do
            name <- getLiteralString token
            assignment <- Map.lookup name map
            return [Note id WarningC 2128
                    "Expanding an array without an index only gives the first element."]
    readF _ _ _ = return []

    writeF _ t name (DataArray _) = do
        modify (Map.insert name t)
        return []
    writeF _ _ name _ = do
        modify (Map.delete name)
        return []

prop_checkStderrRedirect = verify checkStderrRedirect "test 2>&1 > cow"
prop_checkStderrRedirect2 = verifyNot checkStderrRedirect "test > cow 2>&1"
checkStderrRedirect _ (T_Redirecting _ [
    T_FdRedirect id "2" (T_IoFile _ (T_GREATAND _) (T_NormalWord _ [T_Literal _ "1"])),
    T_FdRedirect _ _ (T_IoFile _ op _)
    ] _) = case op of
            T_Greater _ -> error
            T_DGREAT _ -> error
            _ -> return ()
         where error = err id 2069 "The order of the 2>&1 and the redirect matters. The 2>&1 has to be last."
checkStderrRedirect _ _ = return ()

lt x = trace ("FAILURE " ++ show x) x
ltt t = trace ("FAILURE " ++ show t)


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
        if name == "find"
            then return $ getFindCommand cmd
            else return name

    isProbablyOk =
            any isOkAssignment (take 3 $ getPath parents t)
            || commandName `elem` [
                "trap"
                ,"sh"
                ,"bash"
                ,"ksh"
                ,"zsh"
                ,"ssh"
                ,"xprop"
                ,"alias"
                ]
            || "awk" `isSuffixOf` commandName
            || "perl" `isPrefixOf` commandName

    commonlyQuoted = ["PS1", "PS2", "PS3", "PS4", "PROMPT_COMMAND"]
    isOkAssignment t =
        case t of
            T_Assignment _ _ name _ _ -> name `elem` commonlyQuoted
            otherwise -> False

    re = mkRegex "\\$[{(0-9a-zA-Z_]|`.*`"
    sedContra = mkRegex "\\$[dpsaic]($|[^a-zA-Z])"

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
checkUnquotedN _ (T_Condition _ SingleBracket (TC_Unary _ SingleBracket "-n" (T_NormalWord id [t]))) | willSplit t =
       err id 2070 "Always true because you failed to quote. Use [[ ]] instead."
checkUnquotedN _ _ = return ()

prop_checkNumberComparisons1 = verify checkNumberComparisons "[[ $foo < 3 ]]"
prop_checkNumberComparisons2 = verify checkNumberComparisons "[[ 0 >= $(cmd) ]]"
prop_checkNumberComparisons3 = verifyNot checkNumberComparisons "[[ $foo ]] > 3"
prop_checkNumberComparisons4 = verify checkNumberComparisons "[[ $foo > 2.72 ]]"
prop_checkNumberComparisons5 = verify checkNumberComparisons "[[ $foo -le 2.72 ]]"
prop_checkNumberComparisons6 = verify checkNumberComparisons "[[ 3.14 -eq $foo ]]"
prop_checkNumberComparisons7 = verifyNot checkNumberComparisons "[[ 3.14 == $foo ]]"
prop_checkNumberComparisons8 = verify checkNumberComparisons "[[ foo <= bar ]]"
prop_checkNumberComparisons9 = verify checkNumberComparisons "[ foo \\>= bar ]"
prop_checkNumberComparisons11= verify checkNumberComparisons "[[ $foo -eq 'N' ]]"
prop_checkNumberComparisons12= verify checkNumberComparisons "[ x$foo -gt x${N} ]"
checkNumberComparisons params (TC_Binary id typ op lhs rhs) = do
    if isNum lhs && not (isNonNum rhs)
       || isNum rhs && not (isNonNum lhs)
      then do
        when (isLtGt op) $
          err id 2071 $
            op ++ " is for string comparisons. Use " ++ eqv op ++ " instead."
        when (isLeGe op) $
            err id 2071 $ op ++ " is not a valid operator. " ++
              "Use " ++ eqv op ++ " ."
      else do
        when (isLeGe op || isLtGt op) $
            mapM_ checkDecimals [lhs, rhs]

        when (isLeGe op) $
            err id 2122 $ op ++ " is not a valid operator. " ++
                "Use '! a " ++ invert op ++ " b' instead."

    when (op `elem` ["-lt", "-gt", "-le", "-ge", "-eq"]) $ do
        mapM_ checkDecimals [lhs, rhs]
        checkStrings [lhs, rhs]

  where
      isLtGt = flip elem ["<", "\\<", ">", "\\>"]
      isLeGe = flip elem ["<=", "\\<=", ">=", "\\>="]

      supportsDecimals = (shellType params) == Ksh
      checkDecimals hs =
        when (isFraction hs && not supportsDecimals) $
            err (getId hs) 2072 decimalError
      decimalError = "Decimals are not supported. " ++
        "Either use integers only, or use bc or awk to compare."

      checkStrings hs =
        mapM_ stringError . take 1 . filter isNonNum $ hs

      isNonNum t = fromMaybe False $ do
        s <- getLiteralStringExt (const $ return "") t
        return . not . all numChar $ s
      numChar x = isDigit x || x `elem` "+-. "

      stringError t = err (getId t) 2130 $
        op ++ " is for integer comparisons. Use " ++ seqv op ++ " instead."

      isNum t =
        case deadSimple t of
            [v] -> all isDigit v
            _ -> False
      isFraction t =
        case deadSimple t of
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

      floatRegex = mkRegex "^[0-9]+\\.[0-9]+$"
checkNumberComparisons _ _ = return ()

prop_checkSingleBracketOperators1 = verify checkSingleBracketOperators "[ test =~ foo ]"
prop_checkSingleBracketOperators2 = verify checkSingleBracketOperators "[ $foo > $bar ]"
prop_checkSingleBracketOperators3 = verifyNot checkSingleBracketOperators "[[ foo < bar ]]"
prop_checkSingleBracketOperators5 = verify checkSingleBracketOperators "until [ $n <= $z ]; do echo foo; done"
checkSingleBracketOperators _ (TC_Binary id typ op lhs rhs)
    | typ == SingleBracket && op `elem` ["<", ">", "<=", ">="] =
        err id 2073 $ "Can't use " ++ op ++" in [ ]. Escape it or use [[..]]."
checkSingleBracketOperators _ (TC_Binary id typ op lhs rhs)
    | typ == SingleBracket && op == "=~" =
        err id 2074 $ "Can't use " ++ op ++" in [ ]. Use [[..]] instead."
checkSingleBracketOperators _ _ = return ()

prop_checkDoubleBracketOperators1 = verify checkDoubleBracketOperators "[[ 3 \\< 4 ]]"
prop_checkDoubleBracketOperators3 = verifyNot checkDoubleBracketOperators "[[ foo < bar ]]"
checkDoubleBracketOperators _ x@(TC_Binary id typ op lhs rhs)
    | typ == DoubleBracket && op `elem` ["\\<", "\\>", "\\<=", "\\>="] =
        err id 2075 $ "Escaping " ++ op ++" is required in [..], but invalid in [[..]]"
checkDoubleBracketOperators _ _ = return ()

prop_checkConditionalAndOrs1 = verify checkConditionalAndOrs "[ foo && bar ]"
prop_checkConditionalAndOrs2 = verify checkConditionalAndOrs "[[ foo -o bar ]]"
prop_checkConditionalAndOrs3 = verifyNot checkConditionalAndOrs "[[ foo || bar ]]"
checkConditionalAndOrs _ (TC_And id SingleBracket "&&" _ _) =
    err id 2107 "You can't use && inside [..]. Use -a instead."
checkConditionalAndOrs _ (TC_And id DoubleBracket "-a" _ _) =
    err id 2108 "In [[..]], use && instead of -a."
checkConditionalAndOrs _ (TC_Or id SingleBracket "||" _ _) =
    err id 2109 "You can't use || inside [..]. Use -o instead."
checkConditionalAndOrs _ (TC_Or id DoubleBracket "-o" _ _) =
    err id 2110 "In [[..]], use || instead of -o."
checkConditionalAndOrs _ _ = return ()

prop_checkQuotedCondRegex1 = verify checkQuotedCondRegex "[[ $foo =~ \"bar\" ]]"
prop_checkQuotedCondRegex2 = verify checkQuotedCondRegex "[[ $foo =~ 'cow' ]]"
prop_checkQuotedCondRegex3 = verifyNot checkQuotedCondRegex "[[ $foo =~ $foo ]]"
checkQuotedCondRegex _ (TC_Binary _ _ "=~" _ rhs) =
    case rhs of
        T_NormalWord id [T_DoubleQuoted _ _] -> error id
        T_NormalWord id [T_SingleQuoted _ _] -> error id
        _ -> return ()
  where
    error id = err id 2076 "Don't quote rhs of =~, it'll match literally rather than as a regex."
checkQuotedCondRegex _ _ = return ()

prop_checkGlobbedRegex1 = verify checkGlobbedRegex "[[ $foo =~ *foo* ]]"
prop_checkGlobbedRegex2 = verify checkGlobbedRegex "[[ $foo =~ f* ]]"
prop_checkGlobbedRegex2a = verify checkGlobbedRegex "[[ $foo =~ \\#* ]]"
prop_checkGlobbedRegex3 = verifyNot checkGlobbedRegex "[[ $foo =~ $foo ]]"
prop_checkGlobbedRegex4 = verifyNot checkGlobbedRegex "[[ $foo =~ ^c.* ]]"
checkGlobbedRegex _ (TC_Binary _ DoubleBracket "=~" _ rhs) =
    let s = concat $ deadSimple rhs in
        when (isConfusedGlobRegex s) $
            warn (getId rhs) 2049 "=~ is for regex. Use == for globs."
checkGlobbedRegex _ _ = return ()


prop_checkConstantIfs1 = verify checkConstantIfs "[[ foo != bar ]]"
prop_checkConstantIfs2 = verify checkConstantIfs "[[ n -le 4 ]]"
prop_checkConstantIfs3 = verify checkConstantIfs "[[ $n -le 4 && n -ge 2 ]]"
prop_checkConstantIfs4 = verifyNot checkConstantIfs "[[ $n -le 3 ]]"
prop_checkConstantIfs5 = verifyNot checkConstantIfs "[[ $n -le $n ]]"
checkConstantIfs _ (TC_Binary id typ op lhs rhs)
    | op `elem` [ "==", "!=", "<=", ">=", "-eq", "-ne", "-lt", "-le", "-gt", "-ge", "=~", ">", "<", "="] =
        when (isJust lLit && isJust rLit) $ warn id 2050 "This expression is constant. Did you forget the $ on a variable?"
    where
        lLit = getLiteralString lhs
        rLit = getLiteralString rhs
checkConstantIfs _ _ = return ()

prop_checkNoaryWasBinary = verify checkNoaryWasBinary "[[ a==$foo ]]"
prop_checkNoaryWasBinary2 = verify checkNoaryWasBinary "[ $foo=3 ]"
prop_checkNoaryWasBinary3 = verify checkNoaryWasBinary "[ $foo!=3 ]"
checkNoaryWasBinary _ (TC_Noary _ _ t@(T_NormalWord id l)) | not $ isConstant t = do
    let str = concat $ deadSimple t
    when ('=' `elem` str) $ err id 2077 "You need spaces around the comparison operator."
checkNoaryWasBinary _ _ = return ()

prop_checkConstantNoary = verify checkConstantNoary "[[ '$(foo)' ]]"
prop_checkConstantNoary2 = verify checkConstantNoary "[ \"-f lol\" ]"
prop_checkConstantNoary3 = verify checkConstantNoary "[[ cmd ]]"
prop_checkConstantNoary4 = verify checkConstantNoary "[[ ! cmd ]]"
checkConstantNoary _ (TC_Noary _ _ t@(T_NormalWord id _)) | isConstant t =
    err id 2078 "This expression is constant. Did you forget a $ somewhere?"
checkConstantNoary _ _ = return ()

prop_checkBraceExpansionVars1 = verify checkBraceExpansionVars "echo {1..$n}"
prop_checkBraceExpansionVars2 = verifyNot checkBraceExpansionVars "echo {1,3,$n}"
checkBraceExpansionVars _ (T_BraceExpansion id s) | "..$" `isInfixOf` s =
    warn id 2051 "Bash doesn't support variables in brace range expansions."
checkBraceExpansionVars _ _ = return ()

prop_checkForDecimals = verify checkForDecimals "((3.14*c))"
checkForDecimals _ t@(TA_Expansion id _) = potentially $ do
    str <- getLiteralString t
    first <- str !!! 0
    guard $ isDigit first && '.' `elem` str
    return $ err id 2079 "(( )) doesn't support decimals. Use bc or awk."
checkForDecimals _ _ = return ()

prop_checkDivBeforeMult = verify checkDivBeforeMult "echo $((c/n*100))"
prop_checkDivBeforeMult2 = verifyNot checkDivBeforeMult "echo $((c*100/n))"
checkDivBeforeMult _ (TA_Binary _ "*" (TA_Binary id "/" _ _) _) =
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
checkArithmeticDeref params t@(TA_Expansion _ [T_DollarBraced id l]) =
    unless (isException $ bracedString l) getWarning
  where
    isException [] = True
    isException s = any (`elem` "/.:#%?*@$") s || isDigit (head s)
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
checkComparisonAgainstGlob _ (TC_Binary _ DoubleBracket op _ (T_NormalWord id [T_DollarBraced _ _])) | op == "=" || op == "==" =
    warn id 2053 "Quote the rhs of = in [[ ]] to prevent glob interpretation."
checkComparisonAgainstGlob _ (TC_Binary _ SingleBracket op _ word)
        | (op == "=" || op == "==") && isGlob word =
    err (getId word) 2081 "[ .. ] can't match globs. Use [[ .. ]] or grep."
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
-- This only catches the most idiomatic cases. Fixme?
checkOrNeq _ (TC_Or id typ op (TC_Binary _ _ op1 word1 _) (TC_Binary _ _ op2 word2 _))
    | word1 == word2 && (op1 == op2 && (op1 == "-ne" || op1 == "!=")) =
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
    | s `notElem` ["-nt", "-ot", "-ef", "==", "!=", "<=", ">=", "-eq", "-ne", "-lt", "-le", "-gt", "-ge", "=~", ">", "<", "=", "\\<", "\\>", "\\<=", "\\>="] =
        warn id 2057 "Unknown binary operator."
checkValidCondOps _ (TC_Unary id _ s _)
    | s `notElem`  [ "!", "-a", "-b", "-c", "-d", "-e", "-f", "-g", "-h", "-L", "-k", "-p", "-r", "-s", "-S", "-t", "-u", "-w", "-x", "-O", "-G", "-N", "-z", "-n", "-o", "-v", "-R"] =
        warn id 2058 "Unknown unary operator."
checkValidCondOps _ _ = return ()

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
            T_HereString {} -> return True
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

parents params = getPath (parentMap params)

--- Command specific checks

checkCommand str f t@(T_SimpleCommand id _ (cmd:rest)) =
    when (t `isCommand` str) $ f cmd rest
checkCommand _ _ _ = return ()

checkUnqualifiedCommand str f t@(T_SimpleCommand id _ (cmd:rest)) =
    when (t `isUnqualifiedCommand` str) $ f cmd rest
checkUnqualifiedCommand _ _ _ = return ()

getLiteralString = getLiteralStringExt (const Nothing)

getGlobOrLiteralString = getLiteralStringExt f
  where
    f (T_Glob _ str) = return str
    f _ = Nothing

getLiteralStringExt more = g
  where
    allInList = liftM concat . mapM g
    g (T_DoubleQuoted _ l) = allInList l
    g (T_DollarDoubleQuoted _ l) = allInList l
    g (T_NormalWord _ l) = allInList l
    g (TA_Expansion _ l) = allInList l
    g (T_SingleQuoted _ s) = return s
    g (T_Literal _ s) = return s
    g x = more x

isLiteral t = isJust $ getLiteralString t

-- Get a literal string ignoring all non-literals
onlyLiteralString :: Token -> String
onlyLiteralString = fromJust . getLiteralStringExt (const $ return "")

-- Turn a NormalWord like foo="bar $baz" into a series of constituent elements like [foo=,bar ,$baz]
getWordParts (T_NormalWord _ l)   = concatMap getWordParts l
getWordParts (T_DoubleQuoted _ l) = l
getWordParts other                = [other]

getUnquotedLiteral (T_NormalWord _ list) =
    liftM concat $ mapM str list
  where
    str (T_Literal _ s) = return s
    str _ = Nothing
getUnquotedLiteral _ = Nothing

isCommand token str = isCommandMatch token (\cmd -> cmd  == str || ('/' : str) `isSuffixOf` cmd)
isUnqualifiedCommand token str = isCommandMatch token (== str)

isCommandMatch token matcher = fromMaybe False $ do
    cmd <- getCommandName token
    return $ matcher cmd

getCommandName (T_Redirecting _ _ w) =
    getCommandName w
getCommandName (T_SimpleCommand _ _ (w:_)) =
    getLiteralString w
getCommandName (T_Annotation _ _ t) = getCommandName t
getCommandName _ = Nothing

getCommandBasename = liftM basename . getCommandName
basename = reverse . takeWhile (/= '/') . reverse

isAssignment (T_Annotation _ _ w) = isAssignment w
isAssignment (T_Redirecting _ _ w) = isAssignment w
isAssignment (T_SimpleCommand _ (w:_) []) = True
isAssignment (T_Assignment {}) = True
isAssignment _ = False

prop_checkPrintfVar1 = verify checkPrintfVar "printf \"Lol: $s\""
prop_checkPrintfVar2 = verifyNot checkPrintfVar "printf 'Lol: $s'"
prop_checkPrintfVar3 = verify checkPrintfVar "printf -v cow $(cmd)"
prop_checkPrintfVar4 = verifyNot checkPrintfVar "printf \"%${count}s\" var"
checkPrintfVar _ = checkUnqualifiedCommand "printf" (const f) where
    f (dashv:var:rest) | getLiteralString dashv == Just "-v" = f rest
    f (format:params) = check format
    f _ = return ()
    check format =
        unless ('%' `elem` concat (deadSimple format) || isLiteral format) $
          warn (getId format) 2059
              "Don't use variables in the printf format string. Use printf \"..%s..\" \"$foo\"."

prop_checkUuoeCmd1 = verify checkUuoeCmd "echo $(date)"
prop_checkUuoeCmd2 = verify checkUuoeCmd "echo `date`"
prop_checkUuoeCmd3 = verify checkUuoeCmd "echo \"$(date)\""
prop_checkUuoeCmd4 = verify checkUuoeCmd "echo \"`date`\""
prop_checkUuoeCmd5 = verifyNot checkUuoeCmd "echo \"The time is $(date)\""
checkUuoeCmd _ = checkUnqualifiedCommand "echo" (const f) where
    msg id = style id 2005 "Useless echo? Instead of 'echo $(cmd)', just use 'cmd'."
    f [token] = when (tokenIsJustCommandOutput token) $ msg (getId token)
    f _ = return ()

-- Check whether a word is entirely output from a single command
tokenIsJustCommandOutput t = case t of
    T_NormalWord id [T_DollarExpansion _ _] -> True
    T_NormalWord id [T_DoubleQuoted _ [T_DollarExpansion _ _]] -> True
    T_NormalWord id [T_Backticked _ _] -> True
    T_NormalWord id [T_DoubleQuoted _ [T_Backticked _ _]] -> True
    _ -> False

prop_checkUuoeVar1 = verify checkUuoeVar "for f in $(echo $tmp); do echo lol; done"
prop_checkUuoeVar2 = verify checkUuoeVar "date +`echo \"$format\"`"
prop_checkUuoeVar3 = verifyNot checkUuoeVar "foo \"$(echo -e '\r')\""
prop_checkUuoeVar4 = verifyNot checkUuoeVar "echo $tmp"
prop_checkUuoeVar5 = verify checkUuoeVar "foo \"$(echo \"$(date) value:\" $value)\""
prop_checkUuoeVar6 = verifyNot checkUuoeVar "foo \"$(echo files: *.png)\""
prop_checkUuoeVar7 = verifyNot checkUuoeVar "foo $(echo $(bar))" -- covered by 2005
prop_checkUuoeVar8 = verifyNot checkUuoeVar "#!/bin/sh\nz=$(echo)"
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
          otherwise -> return ()

prop_checkTr1 = verify checkTr "tr [a-f] [A-F]"
prop_checkTr2 = verify checkTr "tr 'a-z' 'A-Z'"
prop_checkTr2a= verify checkTr "tr '[a-z]' '[A-Z]'"
prop_checkTr3 = verifyNot checkTr "tr -d '[:lower:]'"
prop_checkTr3a= verifyNot checkTr "tr -d '[:upper:]'"
prop_checkTr3b= verifyNot checkTr "tr -d '|/_[:upper:]'"
prop_checkTr4 = verifyNot checkTr "ls [a-z]"
prop_checkTr5 = verify checkTr "tr foo bar"
prop_checkTr6 = verify checkTr "tr 'hello' 'world'"
prop_checkTr8 = verifyNot checkTr "tr aeiou _____"
prop_checkTr9 = verifyNot checkTr "a-z n-za-m"
prop_checkTr10= verifyNot checkTr "tr --squeeze-repeats rl lr"
prop_checkTr11= verifyNot checkTr "tr abc '[d*]'"
checkTr _ = checkCommand "tr" (const $ mapM_ f)
  where
    f w | isGlob w = -- The user will go [ab] -> '[ab]' -> 'ab'. Fixme?
        warn (getId w) 2060 "Quote parameters to tr to prevent glob expansion."
    f word =
      case getLiteralString word of
        Just "a-z" -> info (getId word) 2018 "Use '[:lower:]' to support accents and foreign alphabets."
        Just "A-Z" -> info (getId word) 2019 "Use '[:upper:]' to support accents and foreign alphabets."
        Just s -> do  -- Eliminate false positives by only looking for dupes in SET2?
          when (not ("-" `isPrefixOf` s || "[:" `isInfixOf` s) && duplicated s) $
            info (getId word) 2020 "tr replaces sets of chars, not words (mentioned due to duplicates)."
          unless ("[:" `isPrefixOf` s) $
            when ("[" `isPrefixOf` s && "]" `isSuffixOf` s && (length s > 2) && ('*' `notElem` s)) $
              info (getId word) 2021 "Don't use [] around ranges in tr, it replaces literal square brackets."
        Nothing -> return ()

    duplicated s =
        let relevant = filter isAlpha s
        in relevant /= nub relevant


prop_checkFindNameGlob1 = verify checkFindNameGlob "find / -name *.php"
prop_checkFindNameGlob2 = verify checkFindNameGlob "find / -type f -ipath *(foo)"
prop_checkFindNameGlob3 = verifyNot checkFindNameGlob "find * -name '*.php'"
checkFindNameGlob _ = checkCommand "find" (const f) where
    acceptsGlob (Just s) = s `elem` [ "-ilname", "-iname", "-ipath", "-iregex", "-iwholename", "-lname", "-name", "-path", "-regex", "-wholename" ]
    acceptsGlob _ = False
    f [] = return ()
    f [x] = return ()
    f (a:b:r) = do
        when (acceptsGlob (getLiteralString a) && isGlob b) $ do
            let (Just s) = getLiteralString a
            warn (getId b) 2061 $ "Quote the parameter to " ++ s ++ " so the shell won't interpret it."
        f (b:r)


prop_checkGrepRe1 = verify checkGrepRe "cat foo | grep *.mp3"
prop_checkGrepRe2 = verify checkGrepRe "grep -Ev cow*test *.mp3"
prop_checkGrepRe3 = verify checkGrepRe "grep --regex=*.mp3 file"
prop_checkGrepRe4 = verifyNot checkGrepRe "grep foo *.mp3"
prop_checkGrepRe5 = verifyNot checkGrepRe "grep-v  --regex=moo *"
prop_checkGrepRe6 = verifyNot checkGrepRe "grep foo \\*.mp3"
prop_checkGrepRe7 = verify checkGrepRe "grep *foo* file"
prop_checkGrepRe8 = verify checkGrepRe "ls | grep foo*.jpg"
prop_checkGrepRe9 = verifyNot checkGrepRe "grep '[0-9]*' file"
prop_checkGrepRe10= verifyNot checkGrepRe "grep '^aa*' file"
prop_checkGrepRe11= verifyNot checkGrepRe "grep --include=*.png foo"

checkGrepRe _ = checkCommand "grep" (const f) where
    -- --regex=*(extglob) doesn't work. Fixme?
    skippable (Just s) = not ("--regex=" `isPrefixOf` s) && "-" `isPrefixOf` s
    skippable _ = False
    f [] = return ()
    f (x:r) | skippable (getLiteralStringExt (const $ return "_") x) = f r
    f (re:_) = do
        when (isGlob re) $
            warn (getId re) 2062 "Quote the grep pattern so the shell won't interpret it."
        let string = concat $ deadSimple re
        if isConfusedGlobRegex string then
            warn (getId re) 2063 "Grep uses regex, but this looks like a glob."
          else potentially $ do
            char <- getSuspiciousRegexWildcard string
            return $ info (getId re) 2022 $
                "Note that unlike globs, " ++ [char] ++ "* here matches '" ++ [char, char, char] ++ "' but not '" ++ wordStartingWith char ++ "'."

wordStartingWith c =
    head . filter ([c] `isPrefixOf`) $ candidates
  where
    candidates =
        sampleWords ++ map (\(x:r) -> toUpper x : r) sampleWords ++ [c:"test"]

prop_checkTrapQuotes1 = verify checkTrapQuotes "trap \"echo $num\" INT"
prop_checkTrapQuotes1a= verify checkTrapQuotes "trap \"echo `ls`\" INT"
prop_checkTrapQuotes2 = verifyNot checkTrapQuotes "trap 'echo $num' INT"
prop_checkTrapQuotes3 = verify checkTrapQuotes "trap \"echo $((1+num))\" EXIT DEBUG"
checkTrapQuotes _ = checkCommand "trap" (const f) where
    f (x:_) = checkTrap x
    f _ = return ()
    checkTrap (T_NormalWord _ [T_DoubleQuoted _ rs]) = mapM_ checkExpansions rs
    checkTrap _ = return ()
    warning id = warn id 2064 "Use single quotes, otherwise this expands now rather than when signalled."
    checkExpansions (T_DollarExpansion id _) = warning id
    checkExpansions (T_Backticked id _) = warning id
    checkExpansions (T_DollarBraced id _) = warning id
    checkExpansions (T_DollarArithmetic id _) = warning id
    checkExpansions _ = return ()

prop_checkTimeParameters1 = verify checkTimeParameters "time -f lol sleep 10"
prop_checkTimeParameters2 = verifyNot checkTimeParameters "time sleep 10"
prop_checkTimeParameters3 = verifyNot checkTimeParameters "time -p foo"
checkTimeParameters _ = checkUnqualifiedCommand "time" f where
    f cmd (x:_) = let s = concat $ deadSimple x in
                when ("-" `isPrefixOf` s && s /= "-p") $
                    info (getId cmd) 2023 "The shell may override 'time' as seen in man time(1). Use 'command time ..' for that one."
    f _ _ = return ()

prop_checkTestRedirects1 = verify checkTestRedirects "test 3 > 1"
prop_checkTestRedirects2 = verifyNot checkTestRedirects "test 3 \\> 1"
prop_checkTestRedirects3 = verify checkTestRedirects "/usr/bin/test $var > $foo"
checkTestRedirects _ (T_Redirecting id redirs@(redir:_) cmd) | cmd `isCommand` "test" =
    warn (getId redir) 2065 "This is interpretted as a shell file redirection, not a comparison."
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
    special file = concat (deadSimple file) == "/dev/null"
checkSudoRedirect _ _ = return ()

prop_checkReturn1 = verifyNot checkReturn "return"
prop_checkReturn2 = verifyNot checkReturn "return 1"
prop_checkReturn3 = verifyNot checkReturn "return $var"
prop_checkReturn4 = verifyNot checkReturn "return $((a|b))"
prop_checkReturn5 = verify checkReturn "return -1"
prop_checkReturn6 = verify checkReturn "return 1000"
prop_checkReturn7 = verify checkReturn "return 'hello world'"
checkReturn _ = checkCommand "return" (const f)
  where
    f (first:second:_) =
        err (getId second) 2151
            "Only one integer 0-255 can be returned. Use stdout for other data."
    f [value] =
        when (isInvalid $ literal value) $
            err (getId value) 2152
                "Can only return 0-255. Other data should be written to stdout."
    f _ = return ()

    isInvalid s = s == "" || any (not . isDigit) s || length s > 5
        || let value = (read s :: Integer) in value > 255

    literal token = fromJust $ getLiteralStringExt lit token
    lit (T_DollarBraced {}) = return "0"
    lit (T_DollarArithmetic {}) = return "0"
    lit (T_DollarExpansion {}) = return "0"
    lit (T_Backticked {}) = return "0"
    lit _ = return "WTF"


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
        let contents = concat $ deadSimple word in
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
checkBackticks _ (T_Backticked id _) =
    style id 2006 "Use $(..) instead of legacy `..`."
checkBackticks _ _ = return ()

prop_checkIndirectExpansion1 = verify checkIndirectExpansion "${foo$n}"
prop_checkIndirectExpansion2 = verifyNot checkIndirectExpansion "${foo//$n/lol}"
prop_checkIndirectExpansion3 = verify checkIndirectExpansion "${$#}"
prop_checkIndirectExpansion4 = verify checkIndirectExpansion "${var${n}_$((i%2))}"
prop_checkIndirectExpansion5 = verifyNot checkIndirectExpansion "${bar}"
checkIndirectExpansion _ (T_DollarBraced i (T_NormalWord _ contents)) =
    when (isIndirection contents) $
        err i 2082 "To expand via indirection, use name=\"foo$n\"; echo \"${!name}\"."
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
checkInexplicablyUnquoted _ (T_NormalWord id tokens) = mapM_ check (tails tokens)
  where
    check (T_SingleQuoted _ _:T_Literal id str:_)
        | all isAlphaNum str =
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
        warn id 2140 "The double quotes around this do nothing. Remove or escape them."
checkInexplicablyUnquoted _ _ = return ()

prop_checkTildeInQuotes1 = verify checkTildeInQuotes "var=\"~/out.txt\""
prop_checkTildeInQuotes2 = verify checkTildeInQuotes "foo > '~/dir'"
prop_checkTildeInQuotes4 = verifyNot checkTildeInQuotes "~/file"
prop_checkTildeInQuotes5 = verifyNot checkTildeInQuotes "echo '/~foo/cow'"
prop_checkTildeInQuotes6 = verifyNot checkTildeInQuotes "awk '$0 ~ /foo/'"
checkTildeInQuotes _ = check
  where
    verify id ('~':'/':_) = warn id 2088 "Note that ~ does not expand in quotes."
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


prop_checkUnusedEchoEscapes1 = verify checkUnusedEchoEscapes "echo 'foo\\nbar\\n'"
prop_checkUnusedEchoEscapes2 = verifyNot checkUnusedEchoEscapes "echo -e 'foi\\nbar'"
prop_checkUnusedEchoEscapes3 = verify checkUnusedEchoEscapes "echo \"n:\\t42\""
prop_checkUnusedEchoEscapes4 = verifyNot checkUnusedEchoEscapes "echo lol"
prop_checkUnusedEchoEscapes5 = verifyNot checkUnusedEchoEscapes "echo -n -e '\n'"
checkUnusedEchoEscapes _ = checkCommand "echo" (const f)
  where
    isDashE = mkRegex "^-.*e"
    hasEscapes = mkRegex "\\\\[rnt]"
    f args | concat (concatMap deadSimple allButLast) `matches` isDashE =
        return ()
      where allButLast = reverse . drop 1 . reverse $ args
    f args = mapM_ checkEscapes args

    checkEscapes (T_NormalWord _ args) =
        mapM_ checkEscapes args
    checkEscapes (T_DoubleQuoted id args) =
        mapM_ checkEscapes args
    checkEscapes (T_Literal id str) = examine id str
    checkEscapes (T_SingleQuoted id str) = examine id str
    checkEscapes _ = return ()

    examine id str =
        when (str `matches` hasEscapes) $
            info id 2028 "echo won't expand escape sequences. Consider printf."


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

-- This is hard to get right without properly parsing ssh args
prop_checkSshCmdStr1 = verify checkSshCommandString "ssh host \"echo $PS1\""
prop_checkSshCmdStr2 = verifyNot checkSshCommandString "ssh host \"ls foo\""
prop_checkSshCmdStr3 = verifyNot checkSshCommandString "ssh \"$host\""
checkSshCommandString _ = checkCommand "ssh" (const f)
  where
    nonOptions =
        filter (\x -> not $ "-" `isPrefixOf` concat (deadSimple x))
    f args =
        case nonOptions args of
            (hostport:r@(_:_)) -> checkArg $ last r
            _ -> return ()
    checkArg (T_NormalWord _ [T_DoubleQuoted id parts]) =
        case filter (not . isConstant) parts of
            [] -> return ()
            (x:_) -> info (getId x) 2029
                "Note that, unescaped, this expands on the client side."
    checkArg _ = return ()


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
subshellAssignmentCheck params t =
    let flow = variableFlow params
        check = findSubshelled flow [("oops",[])] Map.empty
    in snd $ runWriter check


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

data DataSource = SourceFrom [Token] | SourceExternal | SourceDeclaration
  deriving (Show)

data VariableState = Dead Token String | Alive deriving (Show)

dataTypeFrom defaultType v = (case v of T_Array {} -> DataArray; _ -> defaultType) $ SourceFrom [v]

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
        TA_Binary _ op lhs rhs -> maybeToList $ do
            guard $ op `elem` ["=", "*=", "/=", "%=", "+=", "-=", "<<=", ">>=", "&=", "^=", "|="]
            name <- getLiteralString lhs
            return (t, t, name, DataString $ SourceFrom [rhs])

        t@(T_CoProc _ name _) ->
            [(t, t, fromMaybe "COPROC" name, DataArray SourceExternal)]

        --Points to 'for' rather than variable
        T_ForIn id str words _ -> [(t, t, str, DataString $ SourceFrom words)]
        T_SelectIn id str words _ -> [(t, t, str, DataString $ SourceFrom words)]
        _ -> []

-- Consider 'export/declare -x' a reference, since it makes the var available
getReferencedVariableCommand base@(T_SimpleCommand _ _ (T_NormalWord _ (T_Literal _ x:_):rest)) =
    case x of
        "export" -> if "f" `elem` flags
            then []
            else concatMap getReference rest
        "declare" -> if "x" `elem` flags
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

        "declare" -> declaredVars
        "typeset" -> declaredVars

        "local" -> concatMap getModifierParamString rest
        "readonly" -> concatMap getModifierParamString rest
        "set" -> maybeToList $ do
            params <- getSetParams rest
            return (base, base, "@", DataString $ SourceFrom params)

        "printf" -> maybeToList $ getPrintfVariable rest

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
        where var = takeWhile isVariableChar $ dropWhile (`elem` "+-") $ concat $ deadSimple token

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

getModifiedVariableCommand _ = []

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
getBracedReference s = fromMaybe s $
    takeName noPrefix `mplus` getSpecial noPrefix `mplus` getSpecial s
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

getIndexReferences s = fromMaybe [] $ do
    match <- matchRegex re s
    index <- match !!! 0
    return $ matchAllStrings variableNameRegex index
  where
    re = mkRegex "(\\[.*\\])"

getReferencedVariables t =
    case t of
        T_DollarBraced id l -> let str = bracedString l in
            (t, t, getBracedReference str) :
                map (\x -> (l, l, x)) (getIndexReferences str)
        TA_Expansion id _ -> getIfReference t t
        T_Assignment id mode str _ word ->
            [(t, t, str) | mode == Append] ++ specialReferences str t word

        TC_Unary id _ "-v" token -> getIfReference t token
        TC_Unary id _ "-R" token -> getIfReference t token
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

-- Try to get referenced variables from a literal string like "$foo"
-- Ignores tons of cases like arithmetic evaluation and array indices.
prop_getVariablesFromLiteral1 =
    getVariablesFromLiteral "$foo${bar//a/b}$BAZ" == ["foo", "bar", "BAZ"]
getVariablesFromLiteral string =
    map (!! 0) $ matchAllSubgroups variableRegex string
  where
    variableRegex = mkRegex "\\$\\{?([A-Za-z0-9_]+)"

getVariablesFromLiteralToken token =
    getVariablesFromLiteral (fromJust $ getLiteralStringExt (const $ return " ") token)

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
        let read    = getReferencedVariables t
        in mapM_ (\v -> modify (Reference v:)) read

    setWritten t =
        let written = getModifiedVariables t
        in mapM_ (\v -> modify (Assignment v:)) written

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
prop_checkSpacefulness24= verifyNotTree checkSpacefulness "a='a b'; cat <<< $a"
prop_checkSpacefulness25= verifyTree checkSpacefulness "a='s/[0-9]//g'; sed $a"

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
        spaced <- hasSpaces name
        return [Note (getId token) InfoC 2086 warning |
                  spaced
                  && not (isArrayExpansion token) -- There's another warning for this
                  && not (isCounting token)
                  && not (isQuoteFree parents token)
                  && not (usedAsCommandName parents token)]
      where
        warning = "Double quote to prevent globbing and word splitting."

    writeF _ _ name (DataString SourceExternal) = do
        setSpaces name True
        return []

    writeF _ _ name (DataString (SourceFrom vals)) = do
        map <- get
        setSpaces name
            (isSpacefulWord (\x -> Map.findWithDefault True x map) vals)
        return []

    writeF _ _ _ _ = return []

    parents = parentMap params

    isCounting (T_DollarBraced id token) =
        case concat $ deadSimple token of
            '#':_ -> True
            _ -> False
    isCounting _ = False

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
          T_DollarBraced _ l -> spacefulF $ getBracedReference $ bracedString l
          T_NormalWord _ w   -> isSpacefulWord spacefulF w
          T_DoubleQuoted _ w -> isSpacefulWord spacefulF w
          _ -> False
      where
        globspace = "*?[] \t\n"
        containsAny s = any (`elem` s)

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
checkQuotesInLiterals params t =
    doVariableFlowAnalysis readF writeF Map.empty (variableFlow params)
  where
    getQuotes name = liftM (Map.lookup name) get
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
        Map.lookup (concat . deadSimple $ t) map
    forToken quoteMap (T_DoubleQuoted id tokens) =
        msum $ map (forToken quoteMap) tokens
    forToken quoteMap (T_NormalWord id tokens) =
        msum $ map (forToken quoteMap) tokens
    forToken _ t =
        if containsQuotes (concat $ deadSimple t)
        then return $ getId t
        else Nothing

    readF _ expr name = do
        assignment <- getQuotes name
        return
          (if isJust assignment
              && not (isParamTo parents "eval" expr)
              && not (isQuoteFree parents expr)
              then [
                  Note (fromJust assignment)WarningC 2089
                      "Quotes/backslashes will be treated literally. Use an array.",
                  Note (getId expr) WarningC 2090
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
        let string = concat $ deadSimple arg
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
checkUnassignedReferences params t = warnings
  where
    (readMap, writeMap) = execState (mapM tally $ variableFlow params) (Map.empty, Map.empty)
    defaultAssigned = Map.fromList $ map (\a -> (a, ())) $ filter (not . null) internalVariables

    tally (Assignment (_, _, name, _))  =
        modify (\(read, written) -> (read, Map.insert name () written))
    tally (Reference (_, place, name)) =
        modify (\(read, written) -> (Map.insertWith' (const id) name place read, written))
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
        isArray (T_Array {}) = True
        isArray (T_DollarBraced _ l) | var /= getBracedReference (bracedString l) = True
        isArray _ = False

    isGuarded (T_DollarBraced _ v) =
        any (`isPrefixOf` rest) ["-", ":-", "?", ":?"]
      where
        name = concat $ deadSimple v
        rest = dropWhile isVariableChar $ dropWhile (`elem` "#!") $ name
    isGuarded _ = False

    match var candidate =
        if var /= candidate && (map toLower var) == (map toLower candidate)
        then 1
        else dist var candidate


prop_checkGlobsAsOptions1 = verify checkGlobsAsOptions "rm *.txt"
prop_checkGlobsAsOptions2 = verify checkGlobsAsOptions "ls ??.*"
prop_checkGlobsAsOptions3 = verifyNot checkGlobsAsOptions "rm -- *.txt"
checkGlobsAsOptions _ (T_SimpleCommand _ _ args) =
    mapM_ check $ takeWhile (not . isEndOfArgs) args
  where
    check v@(T_NormalWord _ (T_Glob id s:_)) | s == "*" || s == "?" =
        info id 2035 $
            "Use ./" ++ concat (deadSimple v)
                ++ " so names with dashes won't become options."
    check _ = return ()

    isEndOfArgs t =
        case concat $ deadSimple t of
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

checkWhileReadPitfalls _ (T_WhileExpression id [command] contents)
        | isStdinReadCommand command =
    mapM_ checkMuncher contents
  where
    munchers = [ "ssh", "ffmpeg", "mplayer" ]

    isStdinReadCommand (T_Pipeline _ _ [T_Redirecting id redirs cmd]) =
        let plaintext = deadSimple cmd
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
    name = getBracedReference $ bracedString value
    path = getPath (parentMap params) t
    idPath = map getId path

    check [] = return ()
    check (t:rest) =
        case t of
            T_SimpleCommand _ vars (_:_) -> mapM_ checkVar vars
            otherwise -> check rest
    checkVar (T_Assignment aId mode aName Nothing value) |
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
checkCdAndBack params = doLists
  where
    shell = shellType params
    doLists (T_ForIn _ _ _ cmds) = doList cmds
    doLists (T_ForArithmetic _ _ _ _ cmds) = doList cmds
    doLists (T_WhileExpression _ _ cmds) = doList cmds
    doLists (T_UntilExpression _ _ cmds) = doList cmds
    doLists (T_IfExpression _ thens elses) = do
        mapM_ (\(_, l) -> doList l) thens
        doList elses
    doLists _ = return ()

    isCdRevert t =
        case deadSimple t of
            ["cd", p] -> p `elem` ["..", "-"]
            _ -> False

    getCmd (T_Annotation id _ x) = getCmd x
    getCmd (T_Pipeline id _ [x]) = getCommandName x
    getCmd _ = Nothing

    doList list =
        let cds = filter ((== Just "cd") . getCmd) list in
            when (length cds >= 2 && isCdRevert (last cds)) $
               warn (getId $ head cds) 2103 message

    message =
        if shell == Bash
        then "Consider using ( subshell ), 'cd foo||exit', or pushd/popd instead."
        else "Consider using ( subshell ) or 'cd foo||exit' instead."

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
    subshellType t = case leadType (shellType params) (parentMap params) t of
        NoneScope -> Nothing
        SubshellScope str -> return str
    isFunction t = case t of T_Function {} -> True; _ -> False
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
        Sh -> do
            when (hasKeyword && hasParens) $
                warn id 2112 "'function' keyword is non-standard. Delete it."
            when (hasKeyword && not hasParens) $
                warn id 2113 "'function' keyword is non-standard. Use 'foo()' instead of 'function foo'."
checkFunctionDeclarations _ _ = return ()


prop_checkCatastrophicRm1 = verify checkCatastrophicRm "rm -r $1/$2"
prop_checkCatastrophicRm2 = verify checkCatastrophicRm "rm -r /home/$foo"
prop_checkCatastrophicRm3 = verifyNot checkCatastrophicRm "rm -r /home/${USER:?}/*"
prop_checkCatastrophicRm4 = verify checkCatastrophicRm "rm -fr /home/$(whoami)/*"
prop_checkCatastrophicRm5 = verifyNot checkCatastrophicRm "rm -r /home/${USER:-thing}/*"
prop_checkCatastrophicRm6 = verify checkCatastrophicRm "rm --recursive /etc/*$config*"
prop_checkCatastrophicRm8 = verify checkCatastrophicRm "rm -rf /home"
prop_checkCatastrophicRm9 = verifyNot checkCatastrophicRm "rm -rf -- /home"
prop_checkCatastrophicRmA = verify checkCatastrophicRm "rm -rf /usr /lib/nvidia-current/xorg/xorg"
prop_checkCatastrophicRmB = verify checkCatastrophicRm "rm -rf \"$STEAMROOT/\"*"
checkCatastrophicRm params t@(T_SimpleCommand id _ tokens) | t `isCommand` "rm" =
    when (any isRecursiveFlag simpleArgs) $
        mapM_ checkWord tokens
  where
    simpleArgs = deadSimple t

    checkWord token =
        case getLiteralString token of
            Just str ->
                when (notElem "--" simpleArgs && (fixPath str `elem` importantPaths)) $
                    warn (getId token) 2114 "Warning: deletes a system directory. Use 'rm --' to disable this message."
            Nothing ->
                checkWord' token

    checkWord' token = fromMaybe (return ()) $ do
        filename <- getPotentialPath token
        let path = fixPath filename
        return . when (path `elem` importantPaths) $
            warn (getId token) 2115 $ "Use \"${var:?}\" to ensure this never expands to " ++ path ++ " ."

    fixPath filename =
        let normalized = skipRepeating '/' . skipRepeating '*' $ filename in
            if normalized == "/" then normalized else stripTrailing '/' normalized

    getPotentialPath = getLiteralStringExt f
      where
        f (T_Glob _ str) = return str
        f (T_DollarBraced _ word) =
            let var = onlyLiteralString word in
                if any (flip isInfixOf var) [":?", ":-", ":="]
                then Nothing
                else return ""
        f _ = return ""

    isRecursiveFlag "--recursive" = True
    isRecursiveFlag ('-':'-':_) = False
    isRecursiveFlag ('-':str) = 'r' `elem` str || 'R' `elem` str
    isRecursiveFlag _ = False

    stripTrailing c = reverse . dropWhile (== c) . reverse
    skipRepeating c (a:b:rest) | a == b && b == c = skipRepeating c (b:rest)
    skipRepeating c (a:r) = a:skipRepeating c r
    skipRepeating _ [] = []

    paths = [
        "", "/bin", "/etc", "/home", "/mnt", "/usr", "/usr/share", "/usr/local",
        "/var", "/lib"
        ]
    importantPaths = filter (not . null) $
        ["", "/", "/*", "/*/*"] >>= (\x -> map (++x) paths)
checkCatastrophicRm _ _ = return ()


prop_checkInteractiveSu1 = verify checkInteractiveSu "su; rm file; su $USER"
prop_checkInteractiveSu2 = verify checkInteractiveSu "su foo; something; exit"
prop_checkInteractiveSu3 = verifyNot checkInteractiveSu "echo rm | su foo"
prop_checkInteractiveSu4 = verifyNot checkInteractiveSu "su root < script"
checkInteractiveSu params = checkCommand "su" f
  where
    f cmd l = when (length l <= 1) $
        when (all undirected $ getPath (parentMap params) cmd) $
            info (getId cmd) 2117
                "To run commands as another user, use su -c or sudo."

    undirected (T_Pipeline _ _ l) = length l <= 1
    -- This should really just be modifications to stdin, but meh
    undirected (T_Redirecting _ list _) = null list
    undirected _ = True


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
checkUnpassedInFunctions params root =
    execWriter $ mapM_ warnForGroup referenceGroups
  where
    functionMap :: Map.Map String Token
    functionMap = Map.fromList $
        map (\t@(T_Function _ _ _ name _) -> (name,t)) functions
    functions = execWriter $ doAnalysis (tell . maybeToList . findFunction) root

    findFunction t@(T_Function id _ _ name body) =
        let flow = getVariableFlow (shellType params) (parentMap params) body
        in
          if any isPositionalReference flow && not (any isPositionalAssignment flow)
            then return t
            else Nothing
    findFunction _ = Nothing

    isPositionalAssignment x =
        case x of
            Assignment (_, _, str, _) -> isPositional str
            _ -> False
    isPositionalReference x =
        case x of
            Reference (_, _, str) -> isPositional str
            _ -> False

    referenceList :: [(String, Bool, Token)]
    referenceList = execWriter $
        doAnalysis (fromMaybe (return ()) . checkCommand) root
    checkCommand :: Token -> Maybe (Writer [(String, Bool, Token)] ())
    checkCommand t@(T_SimpleCommand _ _ (cmd:args)) = do
        str <- getLiteralString cmd
        unless (Map.member str functionMap) $ fail "irrelevant"
        return $ tell [(str, null args, t)]
    checkCommand _ = Nothing

    isPositional str = str == "*" || str == "@"
        || (all isDigit str && str /= "0")

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


prop_checkSetAssignment1 = verify checkSetAssignment "set foo 42"
prop_checkSetAssignment2 = verify checkSetAssignment "set foo = 42"
prop_checkSetAssignment3 = verify checkSetAssignment "set foo=42"
prop_checkSetAssignment4 = verifyNot checkSetAssignment "set -- if=/dev/null"
prop_checkSetAssignment5 = verifyNot checkSetAssignment "set 'a=5'"
prop_checkSetAssignment6 = verifyNot checkSetAssignment "set"
checkSetAssignment params = checkUnqualifiedCommand "set" f
  where
    f cmd (var:value:rest) =
        let str = literal var in
            when (isVariableName str || isAssignment str) $
                msg (getId var)
    f cmd (var:_) =
        when (isAssignment $ literal var) $
            msg (getId var)
    f _ _ = return ()

    msg id = warn id 2121 "To assign a variable, use just 'var=value', no 'set ..'."

    isAssignment str = '=' `elem` str
    literal (T_NormalWord _ l) = concatMap literal l
    literal (T_Literal _ str) = str
    literal _ = "*"


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
    checkVar (T_Assignment id Assign "PATH" Nothing word) =
        let string = concat $ deadSimple word
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
    checkVar (T_Assignment id Assign "PATH" Nothing (T_NormalWord _ parts)) =
        when (any (\x -> isQuoted x && hasTilde x) parts) $
            warn id 2147 "Literal tilde in PATH works poorly across programs."
    checkVar _ = return ()

    hasTilde t = fromMaybe False (liftM2 elem (return '~') (getLiteralStringExt (const $ return "") t))
    isQuoted (T_DoubleQuoted {}) = True
    isQuoted (T_SingleQuoted {}) = True
    isQuoted _ = False
checkTildeInPath _ _ = return ()

prop_checkUnsupported3 = verify checkUnsupported "#!/bin/sh\ncase foo in bar) baz ;& esac"
prop_checkUnsupported4 = verify checkUnsupported "#!/bin/ksh\ncase foo in bar) baz ;;& esac"
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
    otherwise -> ("", [])
  where
    forCase seps | CaseContinue `elem` seps = ("cases with ;;&", [Bash])
    forCase seps | CaseFallThrough `elem` seps = ("cases with ;&", [Bash, Ksh])
    forCase _ = ("", [])


getCommandSequences (T_Script _ _ cmds) = [cmds]
getCommandSequences (T_BraceGroup _ cmds) = [cmds]
getCommandSequences (T_Subshell _ cmds) = [cmds]
getCommandSequences (T_WhileExpression _ _ cmds) = [cmds]
getCommandSequences (T_UntilExpression _ _ cmds) = [cmds]
getCommandSequences (T_ForIn _ _ _ cmds) = [cmds]
getCommandSequences (T_ForArithmetic _ _ _ _ cmds) = [cmds]
getCommandSequences (T_IfExpression _ thens elses) = map snd thens ++ [elses]
getCommandSequences _ = []

groupWith f = groupBy ((==) `on` f)

prop_checkMultipleAppends1 = verify checkMultipleAppends "foo >> file; bar >> file; baz >> file;"
prop_checkMultipleAppends2 = verify checkMultipleAppends "foo >> file; bar | grep f >> file; baz >> file;"
prop_checkMultipleAppends3 = verifyNot checkMultipleAppends "foo < file; bar < file; baz < file;"
checkMultipleAppends params t =
    mapM_ checkList $ getCommandSequences t
  where
    checkList list =
        mapM_ checkGroup (groupWith (liftM fst) $ map getTarget list)
    checkGroup (f:_:_:_) | isJust f =
        style (snd $ fromJust f) 2129
            "Consider using { cmd1; cmd2; } >> file instead of individual redirects."
    checkGroup _ = return ()
    getTarget (T_Pipeline _ _ args@(_:_)) = getTarget (last args)
    getTarget (T_Redirecting id list _) = do
        file <- mapMaybe getAppend list !!! 0
        return (file, id)
    getTarget _ = Nothing
    getAppend (T_FdRedirect _ _ (T_IoFile _ (T_DGREAT {}) f)) = return f
    getAppend _ = Nothing


prop_checkAliasesExpandEarly1 = verify checkAliasesExpandEarly "alias foo=\"echo $PWD\""
prop_checkAliasesExpandEarly2 = verifyNot checkAliasesExpandEarly "alias -p"
prop_checkAliasesExpandEarly3 = verifyNot checkAliasesExpandEarly "alias foo='echo {1..10}'"
checkAliasesExpandEarly params =
    checkUnqualifiedCommand "alias" (const f)
  where
    f = mapM_ checkArg
    checkArg arg | '=' `elem` concat (deadSimple arg) =
        forM_ (take 1 $ filter (not . isLiteral) $ getWordParts arg) $
            \x -> warn (getId x) 2139 "This expands when defined, not when used. Consider escaping."
    checkArg _ = return ()

prop_checkSuspiciousIFS1 = verify checkSuspiciousIFS "IFS=\"\\n\""
prop_checkSuspiciousIFS2 = verifyNot checkSuspiciousIFS "IFS=$'\\t'"
checkSuspiciousIFS params (T_Assignment id Assign "IFS" Nothing value) =
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

prop_checkAliasesUsesArgs1 = verify checkAliasesUsesArgs "alias a='cp $1 /a'"
prop_checkAliasesUsesArgs2 = verifyNot checkAliasesUsesArgs "alias $1='foo'"
prop_checkAliasesUsesArgs3 = verify checkAliasesUsesArgs "alias a=\"echo \\${@}\""
checkAliasesUsesArgs params =
    checkUnqualifiedCommand "alias" (const f)
  where
    re = mkRegex "\\$\\{?[0-9*@]"
    f = mapM_ checkArg
    checkArg arg =
        let string = fromJust $ getLiteralStringExt (const $ return "_") arg in
            when ('=' `elem` string && string `matches` re) $
                err (getId arg) 2142
                    "Aliases can't use positional parameters. Use a function."

prop_checkGrepQ1= verify checkShouldUseGrepQ "[[ $(foo | grep bar) ]]"
prop_checkGrepQ2= verify checkShouldUseGrepQ "[ -z $(fgrep lol) ]"
prop_checkGrepQ3= verify checkShouldUseGrepQ "[ -n \"$(foo | zgrep lol)\" ]"
prop_checkGrepQ4= verifyNot checkShouldUseGrepQ "[ -z $(grep bar | cmd) ]"
prop_checkGrepQ5= verifyNot checkShouldUseGrepQ "rm $(ls | grep file)"
prop_checkGrepQ6= verifyNot checkShouldUseGrepQ "[[ -n $(pgrep foo) ]]"
checkShouldUseGrepQ params t =
    potentially $ case t of
        TC_Noary id _ token -> check id True token
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

prop_checkTestGlobs1 = verify checkTestGlobs "[ -e *.mp3 ]"
prop_checkTestGlobs2 = verifyNot checkTestGlobs "[[ $a == *b* ]]"
checkTestGlobs params (TC_Unary _ _ op token) | isGlob token =
    err (getId token) 2144 $
       op ++ " doesn't work with globs. Use a for loop."
checkTestGlobs _ _ = return ()

prop_checkFindActionPrecedence1 = verify checkFindActionPrecedence "find . -name '*.wav' -o -name '*.au' -exec rm {} +"
prop_checkFindActionPrecedence2 = verifyNot checkFindActionPrecedence "find . -name '*.wav' -o \\( -name '*.au' -exec rm {} + \\)"
prop_checkFindActionPrecedence3 = verifyNot checkFindActionPrecedence "find . -name '*.wav' -o -name '*.au'"
checkFindActionPrecedence params = checkCommand "find" (const f)
  where
    pattern = [isMatch, const True, isParam ["-o", "-or"], isMatch, const True, isAction]
    f list | length list < length pattern = return ()
    f list@(_:rest) =
        if all id (zipWith ($) pattern list)
        then warnFor (list !! (length pattern - 1))
        else f rest
    isMatch = isParam [ "-name", "-regex", "-iname", "-iregex", "-wholename", "-iwholename" ]
    isAction = isParam [ "-exec", "-execdir", "-delete", "-print", "-print0" ]
    isParam strs t = fromMaybe False $ do
        param <- getLiteralString t
        return $ param `elem` strs
    warnFor t = warn (getId t) 2146 "This action ignores everything before the -o. Use \\( \\) to group."


prop_checkFindExecWithSingleArgument1 = verify checkFindExecWithSingleArgument "find . -exec 'cat {} | wc -l' \\;"
prop_checkFindExecWithSingleArgument2 = verify checkFindExecWithSingleArgument "find . -execdir 'cat {} | wc -l' +"
prop_checkFindExecWithSingleArgument3 = verifyNot checkFindExecWithSingleArgument "find . -exec wc -l {} \\;"
checkFindExecWithSingleArgument _ = checkCommand "find" (const f)
  where
    f = void . sequence . mapMaybe check . tails
    check (exec:arg:term:_) = do
        execS <- getLiteralString exec
        termS <- getLiteralString term
        cmdS <- getLiteralStringExt (const $ return " ") arg

        guard $ execS `elem` ["-exec", "-execdir"] && termS `elem` [";", "+"]
        guard $ cmdS `matches` commandRegex
        return $ warn (getId exec) 2150 "-exec does not invoke a shell. Rewrite or use -exec sh -c .. ."
    check _ = Nothing
    commandRegex = mkRegex "[ |;]"


prop_checkMaskedReturns1 = verify checkMaskedReturns "f() { local a=$(false); }"
prop_checkMaskedReturns2 = verify checkMaskedReturns "declare a=$(false)"
prop_checkMaskedReturns3 = verify checkMaskedReturns "declare a=\"`false`\""
prop_checkMaskedReturns4 = verifyNot checkMaskedReturns "declare a; a=$(false)"
prop_checkMaskedReturns5 = verifyNot checkMaskedReturns "f() { local -r a=$(false); }"
checkMaskedReturns _ t@(T_SimpleCommand id _ (cmd:rest)) = potentially $ do
    name <- getCommandName t
    guard $ name `elem` ["declare", "export"]
        || name == "local" && "r" `notElem` (map snd $ getAllFlags t)
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


return []
runTests =  $( [| $(forAllProperties) (quickCheckWithResult (stdArgs { maxSuccess = 1 }) ) |])
