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
module ShellCheck.Analytics where

import ShellCheck.AST
import ShellCheck.Parser
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map
import Data.Char
import Data.List
import Debug.Trace
import Text.Regex
import Data.Maybe

data Shell = Ksh | Zsh | Sh | Bash
    deriving (Show, Eq)

genericChecks = [
    runBasicAnalysis (\x -> mapM_ (flip ($) x) basicChecks)
    ,runBasicTreeAnalysis treeChecks
    ,subshellAssignmentCheck
    ,checkSpacefulness
    ,checkQuotesInLiterals
    ,checkShebang
    ,checkFunctionsUsedExternally
    ]

checksFor Sh = map runBasicAnalysis [
    checkBashisms
    ,checkTimeParameters
    ]
checksFor Ksh = [ ]
checksFor Zsh = map runBasicAnalysis [ checkTimeParameters ]
checksFor Bash = map runBasicAnalysis [
    checkTimeParameters
    ,checkBraceExpansionVars
    ]

runAllAnalytics root m = addToMap notes m
    where shell = determineShell root
          unsupported = (getId root, Note ErrorC "ShellCheck only handles Bourne based shells, sorry!")
          notes = case shell of
                    Nothing -> [ unsupported ]
                    Just sh -> checkList ((checksFor sh) ++ genericChecks) root

checkList l t = concatMap (\f -> f t) l
addToMap list map = foldr (\(id,note) m -> Map.adjust (\(Metadata pos notes) -> Metadata pos (note:notes)) id m) map list

prop_determineShell0 = determineShell (T_Script (Id 0) "#!/bin/sh" []) == Just Sh
prop_determineShell1 = determineShell (T_Script (Id 0) "#!/usr/bin/env ksh" []) == Just Ksh
prop_determineShell2 = determineShell (T_Script (Id 0) "" []) == Just Bash
determineShell (T_Script _ shebang _) = normalize $ shellFor shebang
    where shellFor s | "/env " `isInfixOf` s = head ((drop 1 $ words s)++[""])
          shellFor s = reverse . takeWhile (/= '/') . reverse $ s
          normalize "csh" = Nothing
          normalize "tcsh" = Nothing
          normalize "sh" = return Sh
          normalize "ksh" = return Ksh
          normalize "zsh" = return Zsh
          normalize "bash" = return Bash
          normalize _ = return Bash

runBasicAnalysis f t = snd $ runState (doAnalysis f t) []
basicChecks = [
    checkUuoc
    ,checkPipePitfalls
    ,checkForInQuoted
    ,checkForInLs
    ,checkRedirectToSame
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
    ,checkForDecimals
    ,checkDivBeforeMult
    ,checkArithmeticDeref
    ,checkArithmeticBadOctal
    ,checkComparisonAgainstGlob
    ,checkPrintfVar
    ,checkCommarrays
    ,checkOrNeq
    ,checkEcho
    ,checkConstantIfs
    ,checkTr
    ,checkPipedAssignment
    ,checkAssignAteCommand
    ,checkUuoe
    ,checkFindNameGlob
    ,checkGrepRe
    ,checkDollarArithmeticCommand
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
    ]
treeChecks = [
    checkUnquotedExpansions
    ,checkSingleQuotedVariables
    ]

runBasicTreeAnalysis checks token =
    checkList (map runTree checks) token
  where
    parentTree = getParentTree token
    runTree f t = runBasicAnalysis (flip f $ parentTree) t

addNoteFor id note = modify ((id, note):)
warn id note = addNoteFor id $ Note WarningC $ note
err id note = addNoteFor id $ Note ErrorC $ note
info id note = addNoteFor id $ Note InfoC $ note
style id note = addNoteFor id $ Note StyleC $ note

isVariableChar x = x == '_' || x >= 'a' && x <= 'z' || x >= 'A' && x <= 'Z' || x >= '0' && x <= '9'

willSplit x =
  case x of
    T_DollarBraced _ _ -> True
    T_DollarExpansion _ _ -> True
    T_Backticked _ _ -> True
    T_BraceExpansion _ s -> True
    T_Glob _ _ -> True
    T_Extglob _ _ _ -> True
    T_NormalWord _ l -> any willSplit l
    _ -> False

isGlob (T_Extglob _ _ _) = True
isGlob (T_Glob _ _) = True
isGlob (T_NormalWord _ l) = any isGlob l
isGlob _ = False

wouldHaveBeenGlob s = '*' `elem` s

isConfusedGlobRegex ('*':_) = True
isConfusedGlobRegex [x,'*'] | x /= '\\' = True
isConfusedGlobRegex _ = False

isPotentiallyConfusedGlobRegex =
    let re = mkRegex "[a-z1-9]\\*" in
        isJust . matchRegex re

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
makeSimple t = t
simplify = doTransform makeSimple

deadSimple (T_NormalWord _ l) = [concat (concatMap (deadSimple) l)]
deadSimple (T_DoubleQuoted _ l) = [(concat (concatMap (deadSimple) l))]
deadSimple (T_SingleQuoted _ s) = [s]
deadSimple (T_DollarBraced _ _) = ["${VAR}"]
deadSimple (T_DollarArithmetic _ _) = ["${VAR}"]
deadSimple (T_DollarExpansion _ _) = ["${VAR}"]
deadSimple (T_Backticked _ _) = ["${VAR}"]
deadSimple (T_Glob _ s) = [s]
deadSimple (T_Pipeline _ [x]) = deadSimple x
deadSimple (T_Literal _ x) = [x]
deadSimple (T_SimpleCommand _ vars words) = concatMap (deadSimple) words
deadSimple (T_Redirecting _ _ foo) = deadSimple foo
deadSimple (T_DollarSingleQuoted _ s) = [s]
deadSimple _ = []

verify f s = checkBasic f s == Just True
verifyNot f s = checkBasic f s == Just False
verifyFull f s = checkFull f s == Just True
verifyNotFull f s = checkFull f s == Just False
verifyTree f s = checkTree f s == Just True
verifyNotTree f s = checkTree f s == Just False

checkBasic f s = checkFull (runBasicAnalysis f) s
checkTree f s = checkFull (runBasicTreeAnalysis [f]) s
checkFull f s = case parseShell "-" s of
        (ParseResult (Just (t, m)) _) -> Just . not . null $ f t
        _ -> Nothing


prop_checkEcho1 = verify checkEcho "FOO=$(echo \"$cow\" | sed 's/foo/bar/g')"
prop_checkEcho2 = verify checkEcho "rm $(echo $cow | sed -e 's,foo,bar,')"
prop_checkEcho3 = verify checkEcho "n=$(echo $foo | wc -c)"
checkEcho (T_Pipeline id [a, b]) =
    when (acmd == ["echo", "${VAR}"]) $
        case bcmd of
            ["sed", v] -> checkIn v
            ["sed", "-e", v] -> checkIn v
            ["wc", "-c"] -> countMsg
            ["wc", "-m"] -> countMsg
            _ -> return ()
  where
    acmd = deadSimple a
    bcmd = deadSimple b
    checkIn s =
        case matchRegex checkEchoSedRe s of
                Just _ -> style id $ "See if you can use ${variable//search/replace} instead."
                _        -> return ()
    countMsg = style id $ "See if you can use ${#variable} instead."
checkEcho _ = return ()
checkEchoSedRe = mkRegex "^s(.)(.*)\\1(.*)\\1g?$"


prop_checkPipedAssignment1 = verify checkPipedAssignment "A=ls | grep foo"
prop_checkPipedAssignment2 = verifyNot checkPipedAssignment "A=foo cmd | grep foo"
prop_checkPipedAssignment3 = verifyNot checkPipedAssignment "A=foo"
checkPipedAssignment (T_Pipeline _ (T_Redirecting _ _ (T_SimpleCommand id (_:_) []):_:_)) =
    warn id "If you wanted to assign the output of the pipeline, use a=$(b | c) ."
checkPipedAssignment _ = return ()

prop_checkAssignAteCommand1 = verify checkAssignAteCommand "A=ls -l"
prop_checkAssignAteCommand2 = verify checkAssignAteCommand "A=ls --sort=$foo"
prop_checkAssignAteCommand3 = verify checkAssignAteCommand "A=cat foo | grep bar"
prop_checkAssignAteCommand4 = verifyNot checkAssignAteCommand "A=foo ls -l"
prop_checkAssignAteCommand5 = verifyNot checkAssignAteCommand "PAGER=cat grep bar"
checkAssignAteCommand (T_SimpleCommand id ((T_Assignment _ _ assignmentTerm):[]) (firstWord:_)) =
    when ("-" `isPrefixOf` (concat $ deadSimple firstWord) ||
        (isCommonCommand (getLiteralString assignmentTerm) && not (isCommonCommand (getLiteralString firstWord)))) $
            warn id "To assign the output of a command, use var=$(cmd) ."
  where
    isCommonCommand (Just s) = s `elem` commonCommands
    isCommonCommand _ = False
checkAssignAteCommand _ = return ()


prop_checkUuoc = verify checkUuoc "cat foo | grep bar"
checkUuoc (T_Pipeline _ (T_Redirecting _ _ f@(T_SimpleCommand id _ _):_:_)) =
    case deadSimple f of ["cat", _] -> style id "Useless cat. Consider 'cmd < file | ..' or 'cmd file | ..' instead."
                         _ -> return ()
checkUuoc _ = return ()

prop_checkNeedlessCommands = verify checkNeedlessCommands "foo=$(expr 3 + 2)"
checkNeedlessCommands (T_SimpleCommand id _ (w:_)) | w `isCommand` "expr" =
    style id "Use $((..)), ${} or [[ ]] in place of antiquated expr."
checkNeedlessCommands (T_SimpleCommand id _ (w:_)) | w `isCommand` "dirname" =
    style id "Use parameter expansion instead, such as ${var%/*}."
checkNeedlessCommands (T_SimpleCommand id _ (w:_)) | w `isCommand` "basename" =
    style id "Use parameter expansion instead, such as ${var##*/}."
checkNeedlessCommands _ = return ()

prop_checkPipePitfalls1 = verify checkPipePitfalls "foo | grep foo | awk bar"
prop_checkPipePitfalls2 = verifyNot checkPipePitfalls "foo | awk bar | grep foo"
prop_checkPipePitfalls3 = verify checkPipePitfalls "ls | grep -v mp3"
checkPipePitfalls (T_Pipeline id commands) = do
    for [["grep"], ["sed"]] $  \id -> style id "You don't need grep | sed, sed can filter lines by itself."
    for [["grep"], ["awk"]] $  \id -> style id "You don't need grep | awk, awk can filter lines by itself."
    for [["ls"], ["?"]] $      \id -> warn id "Don't parse ls output; it mangles filenames."
    for [["ls"], ["grep"]] $   \id -> warn id "Don't use ls | grep. Use a glob or a for loop with a condition."
    for [["ls"], ["xargs"]] $  \id -> warn id "Don't use ls | xargs. Use find -exec .. +"
    for [["find"], ["xargs"]]$ \id -> warn id "Don't use find | xargs cmd. find -exec cmd {} + handles whitespace."
    for [["?"], ["echo"]] $    \id -> info id "echo doesn't read from stdin, are you sure you should be piping to it?"
  where
    for l f =
        let indices = indexOfSublists l (map (take 1 . deadSimple) commands)
        in mapM_ f (map (\n -> getId $ commands !! n) indices)
checkPipePitfalls _ = return ()

indexOfSublists sub all = f 0 all
  where
    f _ [] = []
    f n a@(r:rest) =
        let others = f (n+1) rest in
            if match sub (take (length sub) a)
              then n:others
              else others
    match [] [] = True
    match (["?"]:r1) (_:r2) = match r1 r2
    match (x1:r1) (x2:r2) | x1 == x2 = match r1 r2
    match _ _ = False


bracedString l = concat $ deadSimple l
isMagicInQuotes (T_DollarBraced _ l) | '@' `elem` (bracedString l) = True
isMagicInQuotes _ = False

prop_checkShebang1 = verifyFull checkShebang "#!/usr/bin/env bash -x\necho cow"
prop_checkShebang2 = verifyNotFull checkShebang "#! /bin/sh  -l "
checkShebang (T_Script id sb _) =
    if (length $ words sb) > 2 then
        let note = Note ErrorC $ "On most OS, shebangs can only specify a single parameter."
        in [(id, note)]
    else []

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
checkBashisms = bashism
  where
    errMsg id s = err id $ "#!/bin/sh was specified, so " ++ s ++ " is not supported, even when sh is actually bash."
    warnMsg id s = warn id $ "#!/bin/sh was specified, but " ++ s ++ " is not standard."
    bashism (T_ProcSub id _ _) = errMsg id "process substitution"
    bashism (T_Extglob id _ _) = warnMsg id "extglob"
    bashism (T_DollarSingleQuoted id _) = warnMsg id "$'..'"
    bashism (T_DollarDoubleQuoted id _) = warnMsg id "$\"..\""
    bashism (T_ForArithmetic id _ _ _ _) = warnMsg id "arithmetic for loop"
    bashism (T_Arithmetic id _) = warnMsg id "standalone ((..))"
    bashism (T_SelectIn id _ _ _) = warnMsg id "select loop"
    bashism (T_BraceExpansion id _) = warnMsg id "brace expansion"
    bashism (T_Condition id DoubleBracket _) = warnMsg id "[[ ]]"
    bashism (T_HereString id _) = warnMsg id "here-string"
    bashism (TC_Binary id SingleBracket op _ _)
        | op `elem` [ "-nt", "-ef", "\\<", "\\>", "==" ] =
            warnMsg id op
    bashism (TA_Unary id op _)
        | op `elem` [ "|++", "|--", "++|", "--|"] =
            warnMsg id (filter (/= '|') op)
    bashism t@(T_SimpleCommand id _ _)
        | t `isCommand` "source" =
            warnMsg id "'source' in place of '.'"
    bashism (T_DollarBraced id token) =
        mapM_ check expansion
      where
        str = concat $ deadSimple token
        check (regex, feature) =
            when (isJust $ matchRegex regex str) $ warnMsg id feature

    bashism t@(T_SimpleCommand _ _ (cmd:arg:_))
        | t `isCommand` "echo" && "-" `isPrefixOf` (concat $ deadSimple arg) =
            warnMsg (getId arg) "echo flag"
    bashism t@(T_SimpleCommand _ _ (cmd:arg:_))
        | t `isCommand` "exec" && "-" `isPrefixOf` (concat $ deadSimple arg) =
            warnMsg (getId arg) "exec flag"
    bashism t@(T_SimpleCommand id _ _)
        | t `isCommand` "let" = warnMsg id "'let'"
    bashism t@(TA_Variable id "RANDOM") =
        warnMsg id "RANDOM"

    bashism _ = return()

    varChars="_0-9a-zA-Z"
    expansion = let re = mkRegex in [
        (re $ "^[" ++ varChars ++ "]+\\[.*\\]$", "array references"),
        (re $ "^![" ++ varChars ++ "]+\\[[*@]]$", "array key expansion"),
        (re $ "^![" ++ varChars ++ "]+[*@]$", "name matching prefix"),
        (re $ "^[" ++ varChars ++ "]+:[^-=?+]", "string indexing"),
        (re $ "^[" ++ varChars ++ "]+(\\[.*\\])?/", "string replacement"),
        (re $ "^RANDOM$", "$RANDOM")
        ]

prop_checkForInQuoted = verify checkForInQuoted "for f in \"$(ls)\"; do echo foo; done"
prop_checkForInQuoted2 = verifyNot checkForInQuoted "for f in \"$@\"; do echo foo; done"
prop_checkForInQuoted2a = verifyNot checkForInQuoted "for f in *.mp3; do echo foo; done"
prop_checkForInQuoted2b = verify checkForInQuoted "for f in \"*.mp3\"; do echo foo; done"
prop_checkForInQuoted3 = verify checkForInQuoted "for f in 'find /'; do true; done"
prop_checkForInQuoted4 = verify checkForInQuoted "for f in 1,2,3; do true; done"
prop_checkForInQuoted5 = verify checkForInQuoted "for f in ls; do true; done"
checkForInQuoted (T_ForIn _ f [T_NormalWord _ [word@(T_DoubleQuoted id list)]] _) =
    when (any (\x -> willSplit x && not (isMagicInQuotes x)) list
            || (getLiteralString word >>= (return . wouldHaveBeenGlob)) == Just True) $
        err id $ "Since you double quoted this, it will not word split, and the loop will only run once."
checkForInQuoted (T_ForIn _ f [T_NormalWord _ [T_SingleQuoted id s]] _) =
    warn id $ "This is a literal string. To run as a command, use $(" ++ s ++ ")."
checkForInQuoted (T_ForIn _ f [T_NormalWord _ [T_Literal id s]] _) =
    if ',' `elem` s
      then warn id $ "Use spaces, not commas, to separate loop elements."
      else warn id $ "This loop will only run once, with " ++ f ++ "='" ++ s ++ "'."
checkForInQuoted _ = return ()

prop_checkForInCat1 = verify checkForInCat "for f in $(cat foo); do stuff; done"
prop_checkForInCat1a= verify checkForInCat "for f in `cat foo`; do stuff; done"
prop_checkForInCat2 = verify checkForInCat "for f in $(cat foo | grep lol); do stuff; done"
prop_checkForInCat2a= verify checkForInCat "for f in `cat foo | grep lol`; do stuff; done"
prop_checkForInCat3 = verifyNot checkForInCat "for f in $(cat foo | grep bar | wc -l); do stuff; done"
checkForInCat (T_ForIn _ f [T_NormalWord _ w] _) = mapM_ checkF w
  where
    checkF (T_DollarExpansion id [T_Pipeline _ r])
        | all isLineBased r =
            info id $ "To read lines rather than words, pipe/redirect to a 'while read' loop."
    checkF (T_Backticked id cmds) = checkF (T_DollarExpansion id cmds)
    checkF _ = return ()
    isLineBased cmd = any (cmd `isCommand`) ["grep", "sed", "cat"]
checkForInCat _ = return ()

prop_checkForInLs = verify checkForInLs "for f in $(ls *.mp3); do mplayer \"$f\"; done"
prop_checkForInLs2 = verify checkForInLs "for f in `ls *.mp3`; do mplayer \"$f\"; done"
checkForInLs t = try t
  where
   try (T_ForIn _ f [T_NormalWord _ [T_DollarExpansion id [x]]] _) =
        check id f x
   try (T_ForIn _ f [T_NormalWord _ [T_Backticked id [x]]] _) =
        check id f x
   try _ = return ()
   check id f x =
    case deadSimple x of
      ("ls":n) ->
        let args = (if n == [] then ["*"] else n) in
          err id $ "Don't use 'for "++f++" in $(ls " ++ (intercalate " " n)
            ++ ")'. Use 'for "++f++" in "++ (intercalate " " args) ++ "'."
      _ -> return ()


prop_checkFindExec1 = verify checkFindExec "find / -name '*.php' -exec rm {};"
prop_checkFindExec2 = verify checkFindExec "find / -exec touch {} && ls {} \\;"
prop_checkFindExec3 = verify checkFindExec "find / -execdir cat {} | grep lol +"
prop_checkFindExec4 = verifyNot checkFindExec "find / -name '*.php' -exec foo {} +"
prop_checkFindExec5 = verifyNot checkFindExec "find / -execdir bash -c 'a && b' \\;"
prop_checkFindExec6 = verify checkFindExec "find / -type d -execdir rm *.jpg \\;"
checkFindExec (T_SimpleCommand _ _ t@(h:r)) | h `isCommand` "find" = do
    c <- broken r False
    when c $ do
        let wordId = getId $ last t in
            err wordId "Missing ';' or + terminating -exec. You can't use |/||/&&, and ';' has to be a separate, quoted argument."

  where
    broken [] v = return v
    broken (w:r) v = do
        when v $ (mapM_ warnFor $ fromWord w)
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
        T_Extglob _ _ _ -> True
        _ -> False

    warnFor x =
        if shouldWarn x
        then info (getId x) "This will expand once before find runs, not per file found."
        else return ()

    fromWord (T_NormalWord _ l) = l
    fromWord _ = []
checkFindExec _ = return ()


prop_checkUnquotedExpansions1 = verifyTree checkUnquotedExpansions "rm $(ls)"
prop_checkUnquotedExpansions1a= verifyTree checkUnquotedExpansions "rm `ls`"
prop_checkUnquotedExpansions2 = verifyTree checkUnquotedExpansions "rm foo$(date)"
prop_checkUnquotedExpansions3 = verifyTree checkUnquotedExpansions "[ $(foo) == cow ]"
prop_checkUnquotedExpansions3a= verifyTree checkUnquotedExpansions "[ ! $(foo) ]"
prop_checkUnquotedExpansions4 = verifyNotTree checkUnquotedExpansions "[[ $(foo) == cow ]]"
prop_checkUnquotedExpansions5 = verifyNotTree checkUnquotedExpansions "for f in $(cmd); do echo $f; done"
checkUnquotedExpansions t tree =
    check t
  where
    msg id = warn id "Quote this to prevent word splitting."
    check (T_NormalWord _ l) = mapM_ check' l
    check _ = return ()

    check' t@(T_DollarExpansion id _) = unless (inUnquotableContext tree t) $ msg id
    check' t@(T_Backticked id _) = unless (inUnquotableContext tree t) $ msg id
    check' _ = return ()

prop_checkRedirectToSame = verify checkRedirectToSame "cat foo > foo"
prop_checkRedirectToSame2 = verify checkRedirectToSame "cat lol | sed -e 's/a/b/g' > lol"
prop_checkRedirectToSame3 = verifyNot checkRedirectToSame "cat lol | sed -e 's/a/b/g' > foo.bar && mv foo.bar lol"
checkRedirectToSame s@(T_Pipeline _ list) =
    mapM_ (\l -> (mapM_ (\x -> doAnalysis (checkOccurences x) l) (getAllRedirs list))) list
  where checkOccurences (T_NormalWord exceptId x) (T_NormalWord newId y) =
            when (x == y && exceptId /= newId) (do
                let note = Note InfoC $ "Make sure not to read and write the same file in the same pipeline."
                addNoteFor newId $ note
                addNoteFor exceptId $ note)
        checkOccurences _ _ = return ()
        getAllRedirs l = concatMap (\(T_Redirecting _ ls _) -> concatMap getRedirs ls) l
        getRedirs (T_FdRedirect _ _ (T_IoFile _ op file)) =
                case op of T_Greater _ -> [file]
                           T_Less _    -> [file]
                           T_DGREAT _  -> [file]
                           _ -> []
        getRedirs _ = []
checkRedirectToSame _ = return ()


prop_checkShorthandIf  = verify checkShorthandIf "[[ ! -z file ]] && scp file host || rm file"
prop_checkShorthandIf2 = verifyNot checkShorthandIf "[[ ! -z file ]] && { scp file host || echo 'Eek'; }"
checkShorthandIf (T_AndIf id _ (T_OrIf _ _ _)) =
    info id "Note that A && B || C is not if-then-else. C may run when A is true."
checkShorthandIf _ = return ()


prop_checkDollarStar = verify checkDollarStar "for f in $*; do ..; done"
checkDollarStar (T_NormalWord _ [(T_DollarBraced id l)]) | (bracedString l) == "*"  =
    warn id $ "Use \"$@\" (with quotes) to prevent whitespace problems."
checkDollarStar _ = return ()


prop_checkUnquotedDollarAt = verify checkUnquotedDollarAt "ls $@"
prop_checkUnquotedDollarAt1= verifyNot checkUnquotedDollarAt "ls ${#@}"
prop_checkUnquotedDollarAt2 = verify checkUnquotedDollarAt "ls ${foo[@]}"
prop_checkUnquotedDollarAt3 = verifyNot checkUnquotedDollarAt "ls ${#foo[@]}"
prop_checkUnquotedDollarAt4 = verifyNot checkUnquotedDollarAt "ls \"$@\""
prop_checkUnquotedDollarAt5 = verifyNot checkUnquotedDollarAt "ls ${foo/@/ at }"
checkUnquotedDollarAt (T_NormalWord _ [T_DollarBraced id l]) =
    let string = bracedString l
        failing = err id $ "Add double quotes around ${" ++ string ++ "}, otherwise it's just like $* and breaks on spaces."
        in do
            when ("@" `isPrefixOf` string) failing
            when (not ("#" `isPrefixOf` string) && "[@]" `isInfixOf` string) failing
checkUnquotedDollarAt _ = return ()

prop_checkStderrRedirect = verify checkStderrRedirect "test 2>&1 > cow"
prop_checkStderrRedirect2 = verifyNot checkStderrRedirect "test > cow 2>&1"
checkStderrRedirect (T_Redirecting _ [
    T_FdRedirect id "2" (T_IoFile _ (T_GREATAND _) (T_NormalWord _ [T_Literal _ "1"])),
    T_FdRedirect _ _ (T_IoFile _ op _)
    ] _) = case op of
            T_Greater _ -> error
            T_DGREAT _ -> error
            _ -> return ()
         where error = err id $ "The order of the 2>&1 and the redirect matters. The 2>&1 has to be last."
checkStderrRedirect _ = return ()

lt x = trace ("FAILURE " ++ (show x)) x
ltt t x = trace ("FAILURE " ++ (show t)) x


prop_checkSingleQuotedVariables  = verifyTree checkSingleQuotedVariables "echo '$foo'"
prop_checkSingleQuotedVariables2 = verifyTree checkSingleQuotedVariables "echo 'lol$1.jpg'"
prop_checkSingleQuotedVariables3 = verifyNotTree checkSingleQuotedVariables "sed 's/foo$/bar/'"
prop_checkSingleQuotedVariables3a= verifyTree checkSingleQuotedVariables "sed 's/${foo}/bar/'"
prop_checkSingleQuotedVariables3b= verifyTree checkSingleQuotedVariables "sed 's/$(echo cow)/bar/'"
prop_checkSingleQuotedVariables3c= verifyTree checkSingleQuotedVariables "sed 's/$((1+foo))/bar/'"
prop_checkSingleQuotedVariables4 = verifyNotTree checkSingleQuotedVariables "awk '{print $1}'"
checkSingleQuotedVariables t@(T_SingleQuoted id s) parents =
            case matchRegex checkSingleQuotedVariablesRe s of
                Just [] -> unless (probablyOk t) $ info id $ "Expressions don't expand in single quotes, use double quotes for that."
                _          -> return ()
  where
    probablyOk t = isParamTo parents "awk" t
checkSingleQuotedVariables _ _ = return ()
checkSingleQuotedVariablesRe = mkRegex "\\$[{(0-9a-zA-Z_]"


prop_checkUnquotedN = verify checkUnquotedN "if [ -n $foo ]; then echo cow; fi"
prop_checkUnquotedN2 = verify checkUnquotedN "[ -n $cow ]"
prop_checkUnquotedN3 = verifyNot checkUnquotedN "[[ -n $foo ]] && echo cow"
checkUnquotedN (T_Condition _ SingleBracket (TC_Unary _ SingleBracket "-n" (T_NormalWord id [t]))) | willSplit t =
       err id "Always true because you failed to quote. Use [[ ]] instead."
checkUnquotedN _ = return ()

prop_checkNumberComparisons1 = verify checkNumberComparisons "[[ $foo < 3 ]]"
prop_checkNumberComparisons2 = verify checkNumberComparisons "[[ 0 >= $(cmd) ]]"
prop_checkNumberComparisons3 = verifyNot checkNumberComparisons "[[ $foo ]] > 3"
prop_checkNumberComparisons4 = verify checkNumberComparisons "[[ $foo > 2.72 ]]"
prop_checkNumberComparisons5 = verify checkNumberComparisons "[[ $foo -le 2.72 ]]"
prop_checkNumberComparisons6 = verify checkNumberComparisons "[[ 3.14 = $foo ]]"
checkNumberComparisons (TC_Binary id typ op lhs rhs) = do
    when (op `elem` ["<", ">", "<=", ">=", "\\<", "\\>", "\\<=", "\\>="]) $ do
        when (isNum lhs || isNum rhs) $ err id $ "\"" ++ op ++ "\" is for string comparisons. Use " ++ (eqv op) ++" ."
        mapM_ checkDecimals [lhs, rhs]

    when (op `elem` ["-lt", "-gt", "-le", "-ge", "-eq", "=", "=="]) $ do
        mapM_ checkDecimals [lhs, rhs]

  where
      checkDecimals hs = when (isFraction hs) $ err (getId hs) $ decimalError
      decimalError = "Decimals are not supported. Either use integers only, or use bc or awk to compare."
      isNum t = case deadSimple t of [v] -> all isDigit v
                                     _ -> False
      isFraction t = case deadSimple t of [v] -> isJust $ matchRegex floatRegex v
                                          _ -> False
      eqv ('\\':s) = eqv s
      eqv "<" = "-lt"
      eqv ">" = "-gt"
      eqv "<=" = "-le"
      eqv ">=" = "-ge"
      eqv _ = "the numerical equivalent"
      floatRegex = mkRegex "^[0-9]+\\.[0-9]+$"
checkNumberComparisons _ = return ()

prop_checkSingleBracketOperators1 = verify checkSingleBracketOperators "[ test =~ foo ]"
prop_checkSingleBracketOperators2 = verify checkSingleBracketOperators "[ $foo > $bar ]"
prop_checkSingleBracketOperators3 = verifyNot checkSingleBracketOperators "[[ foo < bar ]]"
prop_checkSingleBracketOperators5 = verify checkSingleBracketOperators "until [ $n <= $z ]; do echo foo; done"
checkSingleBracketOperators (TC_Binary id typ op lhs rhs)
    | typ == SingleBracket && op `elem` ["<", ">", "<=", ">="] =
        err id $ "Can't use " ++ op ++" in [ ]. Escape it or use [[..]]."
checkSingleBracketOperators (TC_Binary id typ op lhs rhs)
    | typ == SingleBracket && op == "=~" =
        err id $ "Can't use " ++ op ++" in [ ]. Use [[..]] instead."
checkSingleBracketOperators _ = return ()

prop_checkDoubleBracketOperators1 = verify checkDoubleBracketOperators "[[ 3 \\< 4 ]]"
prop_checkDoubleBracketOperators3 = verifyNot checkDoubleBracketOperators "[[ foo < bar ]]"
checkDoubleBracketOperators x@(TC_Binary id typ op lhs rhs)
    | typ == DoubleBracket && op `elem` ["\\<", "\\>", "\\<=", "\\>="] =
        err id $ "Escaping " ++ op ++" is required in [..], but invalid in [[..]]"
checkDoubleBracketOperators _ = return ()

prop_checkQuotedCondRegex1 = verify checkQuotedCondRegex "[[ $foo =~ \"bar\" ]]"
prop_checkQuotedCondRegex2 = verify checkQuotedCondRegex "[[ $foo =~ 'cow' ]]"
prop_checkQuotedCondRegex3 = verifyNot checkQuotedCondRegex "[[ $foo =~ $foo ]]"
checkQuotedCondRegex (TC_Binary _ _ "=~" _ rhs) =
    case rhs of
        T_NormalWord id [T_DoubleQuoted _ _] -> error id
        T_NormalWord id [T_SingleQuoted _ _] -> error id
        _ -> return ()
  where
    error id = err id $ "Don't quote rhs of =~, it'll match literally rather than as a regex."
checkQuotedCondRegex _ = return ()

prop_checkGlobbedRegex1 = verify checkGlobbedRegex "[[ $foo =~ *foo* ]]"
prop_checkGlobbedRegex2 = verify checkGlobbedRegex "[[ $foo =~ f* ]]"
prop_checkGlobbedRegex2a = verify checkGlobbedRegex "[[ $foo =~ \\#* ]]"
prop_checkGlobbedRegex3 = verifyNot checkGlobbedRegex "[[ $foo =~ $foo ]]"
prop_checkGlobbedRegex4 = verifyNot checkGlobbedRegex "[[ $foo =~ ^c.* ]]"
checkGlobbedRegex (TC_Binary _ DoubleBracket "=~" _ rhs) =
    let s = concat $ deadSimple rhs in
        if isConfusedGlobRegex s
            then warn (getId rhs) $ "=~ is for regex. Use == for globs."
            else return ()
checkGlobbedRegex _ = return ()


prop_checkConstantIfs1 = verify checkConstantIfs "[[ foo != bar ]]"
prop_checkConstantIfs2 = verify checkConstantIfs "[[ n -le 4 ]]"
prop_checkConstantIfs3 = verify checkConstantIfs "[[ $n -le 4 && n -ge 2 ]]"
prop_checkConstantIfs4 = verifyNot checkConstantIfs "[[ $n -le 3 ]]"
prop_checkConstantIfs5 = verifyNot checkConstantIfs "[[ $n -le $n ]]"
checkConstantIfs (TC_Binary id typ op lhs rhs)
    | op `elem` [ "==", "!=", "<=", ">=", "-eq", "-ne", "-lt", "-le", "-gt", "-ge", "=~", ">", "<", "="] = do
        when (isJust lLit && isJust rLit) $ warn id $ "This expression is constant. Did you forget the $ on a variable?"
    where
        lLit = getLiteralString lhs
        rLit = getLiteralString rhs
checkConstantIfs _ = return ()

prop_checkNoaryWasBinary = verify checkNoaryWasBinary "[[ a==$foo ]]"
prop_checkNoaryWasBinary2 = verify checkNoaryWasBinary "[ $foo=3 ]"
prop_checkNoaryWasBinary3 = verify checkNoaryWasBinary "[ foo!=3 ]"
checkNoaryWasBinary (TC_Noary _ _ t@(T_NormalWord id l)) = do
    let str = concat $ deadSimple t
    when ('=' `elem` str) $ err id $ "Always true because you didn't put spaces around the operator."
checkNoaryWasBinary _ = return ()

prop_checkConstantNoary = verify checkConstantNoary "[[ '$(foo)' ]]"
prop_checkConstantNoary2 = verify checkConstantNoary "[ \"-f lol\" ]"
prop_checkConstantNoary3 = verify checkConstantNoary "[[ cmd ]]"
prop_checkConstantNoary4 = verify checkConstantNoary "[[ ! cmd ]]"
checkConstantNoary (TC_Noary _ _ t@(T_NormalWord id _)) | isConstant t = do
    err id $ if isEmpty t then "Always false, just checks the non-emptyness of the literal word."
                          else "Always true, just checks the non-emptyness of the literal word."
checkConstantNoary _ = return ()

prop_checkBraceExpansionVars = verify checkBraceExpansionVars "echo {1..$n}"
checkBraceExpansionVars (T_BraceExpansion id s) | '$' `elem` s =
    warn id $ "Bash doesn't support variables in brace expansions."
checkBraceExpansionVars _ = return ()

prop_checkForDecimals = verify checkForDecimals "((3.14*c))"
checkForDecimals (TA_Literal id s) | any (== '.') s = do
    err id $ "(( )) doesn't support decimals. Use bc or awk."
checkForDecimals _ = return ()

prop_checkDivBeforeMult = verify checkDivBeforeMult "echo $((c/n*100))"
prop_checkDivBeforeMult2 = verifyNot checkDivBeforeMult "echo $((c*100/n))"
checkDivBeforeMult (TA_Binary _ "*" (TA_Binary id "/" _ _) _) = do
    info id $ "Increase precision by replacing a/b*c with a*c/b."
checkDivBeforeMult _ = return ()

prop_checkArithmeticDeref = verify checkArithmeticDeref "echo $((3+$foo))"
prop_checkArithmeticDeref2 = verify checkArithmeticDeref "cow=14; (( s+= $cow ))"
prop_checkArithmeticDeref3 = verifyNot checkArithmeticDeref "cow=1/40; (( s+= ${cow%%/*} ))"
prop_checkArithmeticDeref4 = verifyNot checkArithmeticDeref "(( ! $? ))"
prop_checkArithmeticDeref5 = verifyNot checkArithmeticDeref "(($1))"
checkArithmeticDeref (TA_Expansion _ (T_DollarBraced id l)) | not . excepting $ bracedString l =
    style id $ "Don't use $ on variables in (( ))."
  where
    excepting [] = True
    excepting s = (any (`elem` "/.:#%?*@") s) || (isDigit $ head s)
checkArithmeticDeref _ = return ()

prop_checkArithmeticBadOctal1 = verify checkArithmeticBadOctal "(( 0192 ))"
prop_checkArithmeticBadOctal2 = verifyNot checkArithmeticBadOctal "(( 0x192 ))"
prop_checkArithmeticBadOctal3 = verifyNot checkArithmeticBadOctal "(( 1 ^ 0777 ))"
checkArithmeticBadOctal (TA_Base id "0" (TA_Literal _ str)) | '9' `elem` str || '8' `elem` str =
    err id $ "Numbers with leading 0 are considered octal."
checkArithmeticBadOctal _ = return ()

prop_checkComparisonAgainstGlob = verify checkComparisonAgainstGlob "[[ $cow == $bar ]]"
prop_checkComparisonAgainstGlob2 = verifyNot checkComparisonAgainstGlob "[[ $cow == \"$bar\" ]]"
checkComparisonAgainstGlob (TC_Binary _ DoubleBracket op _ (T_NormalWord id [T_DollarBraced _ _])) | op == "=" || op == "==" =
    warn id $ "Quote the rhs of = in [[ ]] to prevent glob interpretation."
checkComparisonAgainstGlob _ = return ()

prop_checkCommarrays1 = verify checkCommarrays "a=(1, 2)"
prop_checkCommarrays2 = verify checkCommarrays "a+=(1,2,3)"
prop_checkCommarrays3 = verifyNot checkCommarrays "cow=(1 \"foo,bar\" 3)"
checkCommarrays (T_Array id l) =
    if any ("," `isSuffixOf`) (concatMap deadSimple l) || (length $ filter (==',') (concat $ concatMap deadSimple l)) > 1
    then warn id "Use spaces, not commas, to separate array elements."
    else return ()
checkCommarrays _ = return ()

prop_checkOrNeq1 = verify checkOrNeq "if [[ $lol -ne cow || $lol -ne foo ]]; then echo foo; fi"
prop_checkOrNeq2 = verify checkOrNeq "(( a!=lol || a!=foo ))"
prop_checkOrNeq3 = verify checkOrNeq "[ \"$a\" != lol || \"$a\" != foo ]"
prop_checkOrNeq4 = verifyNot checkOrNeq "[ a != $cow || b != $foo ]"
-- This only catches the most idiomatic cases. Fixme?
checkOrNeq (TC_Or id typ op (TC_Binary _ _ op1 word1 _) (TC_Binary _ _ op2 word2 _))
    | word1 == word2 && (op1 == op2 && (op1 == "-ne" || op1 == "!=")) =
        warn id $ "You probably wanted " ++ (if typ == SingleBracket then "-a" else "&&") ++ " here."
checkOrNeq (TA_Binary id "||" (TA_Binary _ "!=" word1 _) (TA_Binary _ "!=" word2 _))
    | word1 == word2 =
        warn id "You probably wanted && here."
checkOrNeq _ = return ()


prop_checkDollarArithmeticCommand1 = verify checkDollarArithmeticCommand "while $((n>10)); do echo foo; done"
prop_checkDollarArithmeticCommand2 = verify checkDollarArithmeticCommand "$(( n++ ))"
prop_checkDollarArithmeticCommand3 = verifyNot checkDollarArithmeticCommand "if (( n > 10 )); then echo foo; fi"
prop_checkDollarArithmeticCommand4 = verifyNot checkDollarArithmeticCommand "echo $((n+3))"
checkDollarArithmeticCommand (T_SimpleCommand _ [] [T_NormalWord _ [T_DollarArithmetic id _]]) =
    err id "Use ((..)) instead of $((..)) when running as a command."
checkDollarArithmeticCommand _ = return ()

allModifiedVariables t = snd $ runState (doAnalysis (\x -> modify $ (++) (getModifiedVariables x)) t) []

prop_checkValidCondOps1 = verify checkValidCondOps "[[ a -xz b ]]"
prop_checkValidCondOps2 = verify checkValidCondOps "[ -M a ]"
prop_checkValidCondOps2a= verifyNot checkValidCondOps "[ 3 \\> 2 ]"
prop_checkValidCondOps3 = verifyNot checkValidCondOps "[ 1 = 2 -a 3 -ge 4 ]"
prop_checkValidCondOps4 = verifyNot checkValidCondOps "[[ ! -v foo ]]"
checkValidCondOps (TC_Binary id _ s _ _)
    | not (s `elem` ["-nt", "-ot", "-ef", "==", "!=", "<=", ">=", "-eq", "-ne", "-lt", "-le", "-gt", "-ge", "=~", ">", "<", "=", "\\<", "\\>", "\\<=", "\\>="]) =
        warn id "Unknown binary operator."
checkValidCondOps (TC_Unary id _ s _)
    | not (s `elem`  [ "!", "-a", "-b", "-c", "-d", "-e", "-f", "-g", "-h", "-L", "-k", "-p", "-r", "-s", "-S", "-t", "-u", "-w", "-x", "-O", "-G", "-N", "-z", "-n", "-o", "-v", "-R"]) =
        warn id "Unknown unary operator."
checkValidCondOps _ = return ()

--- Context seeking

getParentTree t =
    snd . snd $ runState (doStackAnalysis pre post t) ([], Map.empty)
  where
    pre t = modify (\(l, m) -> (t:l, m))
    post t = do
        ((_:rest), map) <- get
        case rest of [] -> put (rest, map)
                     (x:_) -> put (rest, Map.insert (getId t) x map)

getTokenMap t =
    snd $ runState (doAnalysis f t) (Map.empty)
  where
    f t = modify (Map.insert (getId t) t)


inUnquotableContext tree t =
    case t of
        TC_Noary _ DoubleBracket _ -> True
        TC_Unary _ DoubleBracket _ _ -> True
        TC_Binary _ DoubleBracket _ _ _ -> True
        TA_Unary _ _ _ -> True
        TA_Binary _ _ _ _ -> True
        TA_Trinary _ _ _ _ -> True
        TA_Expansion _ _ -> True
        T_Assignment _ _ _ -> True
        T_Redirecting _ _ _ -> or $ map (isCommand t) ["local", "declare"]
        T_DoubleQuoted _ _ -> True
        T_CaseExpression _ _ _ -> True
        T_ForIn _ _ _ _ -> True -- Pragmatically assume it's desirable here
        x -> case Map.lookup (getId x) tree of
                Nothing -> False
                Just parent -> inUnquotableContext tree parent

isParamTo tree cmd t =
    go t
  where
    go x = case Map.lookup (getId x) tree of
                Nothing -> False
                Just parent -> check parent
    check t =
        case t of
            T_SingleQuoted _ _ -> go t
            T_DoubleQuoted _ _ -> go t
            T_NormalWord _ _ -> go t
            T_SimpleCommand _ _ _ -> isCommand t cmd
            T_Redirecting _ _ _ -> isCommand t cmd
            _ -> False

--- Command specific checks

checkCommand str f (T_SimpleCommand id _ cmd) =
    case cmd of
        (w:rest) -> if w `isCommand` str then f rest else return ()
        _ -> return ()
checkCommand _ _ _ = return ()

checkUnqualifiedCommand str f (T_SimpleCommand id _ cmd) =
    case cmd of
        (w:rest) -> if w `isUnqualifiedCommand` str then f rest else return ()
        _ -> return ()
checkUnqualifiedCommand _ _ _ = return ()

getLiteralString t = g t
  where
    allInList l = let foo = map g l in if all isJust foo then return $ concat (catMaybes foo) else Nothing
    g s@(T_DoubleQuoted _ l) = allInList l
    g s@(T_DollarDoubleQuoted _ l) = allInList l
    g s@(T_NormalWord _ l) = allInList l
    g (T_SingleQuoted _ s) = return s
    g (T_Literal _ s) = return s
    g _ = Nothing

isLiteral t = isJust $ getLiteralString t

isCommand token str = isCommandMatch token (\cmd -> cmd  == str || ("/" ++ str) `isSuffixOf` cmd)
isUnqualifiedCommand token str = isCommandMatch token (\cmd -> cmd  == str)

isCommandMatch (T_Redirecting _ _ w) matcher =
    isCommandMatch w matcher
isCommandMatch (T_SimpleCommand _ _ (w:_)) matcher =
    isCommandMatch w matcher
isCommandMatch token matcher =
    case getLiteralString token of
        Just cmd -> matcher cmd
        Nothing -> False

getCommandFor word =
    case getLiteralString word of
        Just str -> reverse . (takeWhile (/= '/')) . reverse $ str
        Nothing -> ""

prop_checkPrintfVar1 = verify checkPrintfVar "printf \"Lol: $s\""
prop_checkPrintfVar2 = verifyNot checkPrintfVar "printf 'Lol: $s'"
prop_checkPrintfVar3 = verify checkPrintfVar "printf -v cow $(cmd)"
checkPrintfVar = checkUnqualifiedCommand "printf" f where
    f (dashv:var:rest) | getLiteralString dashv == (Just "-v") = f rest
    f (format:params) = check format
    f _ = return ()
    check format =
        if not $ isLiteral format
          then warn (getId format) $ "Don't use variables in the printf format string. Use printf \"%s\" \"$foo\"."
          else return ()

prop_checkUuoe1 = verify checkUuoe "echo $(date)"
prop_checkUuoe1a= verify checkUuoe "echo `date`"
prop_checkUuoe2 = verify checkUuoe "echo \"$(date)\""
prop_checkUuoe2a= verify checkUuoe "echo \"`date`\""
prop_checkUuoe3 = verifyNot checkUuoe "echo \"The time is $(date)\""
checkUuoe = checkUnqualifiedCommand "echo" f where
    msg id = style id "Useless echo? Instead of 'echo $(cmd)', just use 'cmd'."
    f [T_NormalWord id [(T_DollarExpansion _ _)]] = msg id
    f [T_NormalWord id [T_DoubleQuoted _ [(T_DollarExpansion _ _)]]] = msg id
    f [T_NormalWord id [(T_Backticked _ _)]] = msg id
    f [T_NormalWord id [T_DoubleQuoted _ [(T_Backticked _ _)]]] = msg id
    f _ = return ()

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
checkTr = checkCommand "tr" (mapM_ f)
  where
    f w | isGlob w = do -- The user will go [ab] -> '[ab]' -> 'ab'. Fixme?
        warn (getId w) $ "Quote parameters to tr to prevent glob expansion."
    f word = case getLiteralString word of
                Just "a-z" -> info (getId word) "Use '[:lower:]' to support accents and foreign alphabets."
                Just "A-Z" -> info (getId word) "Use '[:upper:]' to support accents and foreign alphabets."

                Just s -> do  -- Eliminate false positives by only looking for dupes in SET2?
                            when ((not $ "-" `isPrefixOf` s || "[:" `isInfixOf` s) && duplicated s) $
                                info (getId word) "tr replaces sets of chars, not words (mentioned due to duplicates)."

                            unless ("[:" `isPrefixOf` s) $
                                when ("[" `isPrefixOf` s && "]" `isSuffixOf` s && (length s > 2) && (not $ '*' `elem` s)) $
                                    info (getId word) "Don't use [] around ranges in tr, it replaces literal square brackets."
                Nothing -> return ()

    duplicated s =
        let relevant = filter isAlpha s
        in not $ relevant == nub relevant


prop_checkFindNameGlob1 = verify checkFindNameGlob "find / -name *.php"
prop_checkFindNameGlob2 = verify checkFindNameGlob "find / -type f -ipath *(foo)"
prop_checkFindNameGlob3 = verifyNot checkFindNameGlob "find * -name '*.php'"
checkFindNameGlob = checkCommand "find" f where
    acceptsGlob (Just s) = s `elem` [ "-ilname", "-iname", "-ipath", "-iregex", "-iwholename", "-lname", "-name", "-path", "-regex", "-wholename" ]
    acceptsGlob _ = False
    f [] = return ()
    f [x] = return ()
    f (a:b:r) = do
        when (acceptsGlob (getLiteralString a) && isGlob b) $ do
            let (Just s) = getLiteralString a
            warn (getId b) $ "Quote the parameter to " ++ s ++ " so the shell won't interpret it."
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

checkGrepRe = checkCommand "grep" f where
    -- --regex=*(extglob) doesn't work. Fixme?
    skippable (Just s) = not ("--regex=" `isPrefixOf` s) && "-" `isPrefixOf` s
    skippable _ = False
    f [] = return ()
    f (x:r) | skippable (getLiteralString x) = f r
    f (re:_) = do
        when (isGlob re) $ do
            warn (getId re) $ "Quote the grep pattern so the shell won't interpret it."
        let string = concat $ deadSimple re
        if isConfusedGlobRegex string then
            warn (getId re) $ "Grep uses regex, but this looks like a glob."
          else
            if (isPotentiallyConfusedGlobRegex string)
            then info (getId re) "Note that c* does not mean \"c followed by anything\" in regex."
            else return ()


prop_checkTrapQuotes1 = verify checkTrapQuotes "trap \"echo $num\" INT"
prop_checkTrapQuotes1a= verify checkTrapQuotes "trap \"echo `ls`\" INT"
prop_checkTrapQuotes2 = verifyNot checkTrapQuotes "trap 'echo $num' INT"
prop_checkTrapQuotes3 = verify checkTrapQuotes "trap \"echo $((1+num))\" EXIT DEBUG"
checkTrapQuotes = checkCommand "trap" f where
    f (x:_) = checkTrap x
    f _ = return ()
    checkTrap (T_NormalWord _ [T_DoubleQuoted _ rs]) = mapM_ checkExpansions rs
    checkTrap _ = return ()
    warning id = warn id $ "Use single quotes, otherwise this expands now rather than when signalled."
    checkExpansions (T_DollarExpansion id _) = warning id
    checkExpansions (T_Backticked id _) = warning id
    checkExpansions (T_DollarBraced id _) = warning id
    checkExpansions (T_DollarArithmetic id _) = warning id
    checkExpansions _ = return ()

prop_checkTimeParameters1 = verify checkTimeParameters "time -f lol sleep 10"
prop_checkTimeParameters2 = verifyNot checkTimeParameters "time sleep 10"
prop_checkTimeParameters3 = verifyNot checkTimeParameters "time -p foo"
checkTimeParameters = checkUnqualifiedCommand "time" f where
    f (x:_) = let s = concat $ deadSimple x in
                if "-" `isPrefixOf` s && s /= "-p" then
                    info (getId x) "The shell may override 'time' as seen in man time(1). Use 'command time ..' for that one."
                  else return ()
    f _ = return ()

prop_checkTestRedirects1 = verify checkTestRedirects "test 3 > 1"
prop_checkTestRedirects2 = verifyNot checkTestRedirects "test 3 \\> 1"
prop_checkTestRedirects3 = verify checkTestRedirects "/usr/bin/test $var > $foo"
checkTestRedirects (T_Redirecting id redirs@(redir:_) cmd) | cmd `isCommand` "test" =
    warn (getId redir) $ "This is interpretted as a shell file redirection, not a comparison."
checkTestRedirects _ = return ()

prop_checkSudoRedirect1 = verify checkSudoRedirect "sudo echo 3 > /proc/file"
prop_checkSudoRedirect2 = verify checkSudoRedirect "sudo cmd < input"
prop_checkSudoRedirect3 = verify checkSudoRedirect "sudo cmd >> file"
prop_checkSudoRedirect4 = verify checkSudoRedirect "sudo cmd &> file"
prop_checkSudoRedirect5 = verifyNot checkSudoRedirect "sudo cmd 2>&1"
prop_checkSudoRedirect6 = verifyNot checkSudoRedirect "sudo cmd 2> log"
checkSudoRedirect (T_Redirecting _ redirs cmd) | cmd `isCommand` "sudo" =
    mapM_ warnAbout redirs
  where
    warnAbout (T_FdRedirect _ s (T_IoFile id op file))
        | s == "" || s == "&" =
        case op of
            T_Less _ ->
              info (getId op) $
                "sudo doesn't affect redirects. Use sudo cat file | .."
            T_Greater _ ->
              warn (getId op) $
                "sudo doesn't affect redirects. Use ..| sudo tee file"
            T_DGREAT _ ->
              warn (getId op) $
                "sudo doesn't affect redirects. Use .. | sudo tee -a file"
            _ -> return ()
    warnAbout _ = return ()
checkSudoRedirect _ = return ()

prop_checkPS11 = verify checkPS1Assignments "PS1='\\033[1;35m\\$ '"
prop_checkPS11a= verify checkPS1Assignments "export PS1='\\033[1;35m\\$ '"
prop_checkPSf2 = verify checkPS1Assignments "PS1='\\h \\e[0m\\$ '"
prop_checkPS13 = verify checkPS1Assignments "PS1=$'\\x1b[c '"
prop_checkPS14 = verify checkPS1Assignments "PS1=$'\\e[3m; '"
prop_checkPS14a= verify checkPS1Assignments "export PS1=$'\\e[3m; '"
prop_checkPS15 = verifyNot checkPS1Assignments "PS1='\\[\\033[1;35m\\]\\$ '"
prop_checkPS16 = verifyNot checkPS1Assignments "PS1='\\[\\e1m\\e[1m\\]\\$ '"
checkPS1Assignments t =
    case t of
        (T_Assignment _ "PS1" word) -> warnFor [word]
        (T_SimpleCommand id _ tokens) ->
            when (t `isCommand` "export") $
                warnFor (filter isPS1Token tokens)
        _ -> return ()
  where
    isPS1Token t = "PS1" `isPrefixOf` (concat $ deadSimple t)
    warnFor words =
        let contents = concat $ concatMap deadSimple words in
            when (not (null words) && containsUnescaped contents) $
                info (getId $ head words) "Make sure all escape sequences are enclosed in \\[..\\] to prevent line wrapping issues"
    containsUnescaped s =
        let unenclosed = subRegex enclosedRegex s "" in
           isJust $ matchRegex escapeRegex unenclosed
    enclosedRegex = mkRegex "\\\\\\[.*\\\\\\]" -- FIXME: shouldn't be eager
    escapeRegex = mkRegex "\\x1[Bb]|\\e|\x1B|\\033"

prop_checkBackticks1 = verify checkBackticks "echo `foo`"
prop_checkBackticks2 = verifyNot checkBackticks "echo $(foo)"
checkBackticks (T_Backticked id _) =
    style id "Use $(..) instead of deprecated `..`"
checkBackticks _ = return ()

prop_checkIndirectExpansion1 = verify checkIndirectExpansion "${foo$n}"
prop_checkIndirectExpansion2 = verifyNot checkIndirectExpansion "${foo//$n/lol}"
checkIndirectExpansion (T_DollarBraced id (T_NormalWord _ ((T_Literal _ s):attempt:_))) =
    case attempt of T_DollarExpansion _ _ -> doit
                    T_Backticked _ _ -> doit
                    T_DollarBraced _ _ -> doit
                    T_DollarArithmetic _ _ -> doit
                    _ -> return ()
  where
    doit = if all isVariableChar s
            then err id "To expand via indirection, use name=\"foo$n\"; echo \"${!name}\""
            else return ()
checkIndirectExpansion _ = return ()

prop_checkInexplicablyUnquoted1 = verify checkInexplicablyUnquoted "echo 'var='value';'"
prop_checkInexplicablyUnquoted2 = verifyNot checkInexplicablyUnquoted "'foo'*"
prop_checkInexplicablyUnquoted3 = verifyNot checkInexplicablyUnquoted "wget --user-agent='something'"
checkInexplicablyUnquoted (T_NormalWord id tokens) = mapM_ check (tails tokens)
  where
    check ((T_SingleQuoted _ _):(T_Literal id str):_)
        | all isAlphaNum str =
        info id $ "This word is outside of quotes. Did you intend to 'nest '\"'single quotes'\"' instead'? "
    check _ = return ()
checkInexplicablyUnquoted _ = return ()

prop_checkTildeInQuotes1 = verify checkTildeInQuotes "var=\"~/out.txt\""
prop_checkTildeInQuotes2 = verify checkTildeInQuotes "foo > '~/dir'"
prop_checkTildeInQuotes3 = verify checkTildeInQuotes "args='-s ~/dir'"
prop_checkTildeInQuotes4 = verifyNot checkTildeInQuotes "~/file"
prop_checkTildeInQuotes5 = verifyNot checkTildeInQuotes "echo '/~foo/cow'"
checkTildeInQuotes = check
  where
    post f = f "Note that ~ does not expand in quotes"
    verify id ('~':_) = post (warn id)
    verify id str =
        when (isJust $ matchRegex re str) $ post (info id)
    re = mkRegex "\\s~"
    check (T_NormalWord _ ((T_SingleQuoted id str):_)) =
        verify id str
    check (T_NormalWord _ ((T_DoubleQuoted _ ((T_Literal id str):_)):_)) =
        verify id str
    check _ = return ()

prop_checkLonelyDotDash1 = verify checkLonelyDotDash "./ file"
prop_checkLonelyDotDash2 = verifyNot checkLonelyDotDash "./file"
checkLonelyDotDash t@(T_Redirecting id _ _)
    | isUnqualifiedCommand t "./" =
        err id "Don't add spaces after the slash in './file'."
checkLonelyDotDash _ = return ()


prop_checkSpuriousExec1 = verify checkSpuriousExec "exec foo; true"
prop_checkSpuriousExec2 = verify checkSpuriousExec "if a; then exec b; exec c; fi"
prop_checkSpuriousExec3 = verifyNot checkSpuriousExec "echo cow; exec foo"
prop_checkSpuriousExec4 = verifyNot checkSpuriousExec "if a; then exec b; fi"
prop_checkSpuriousExec5 = verifyNot checkSpuriousExec "exec > file; cmd"
prop_checkSpuriousExec6 = verify checkSpuriousExec "exec foo > file; cmd"
checkSpuriousExec = doLists
  where
    doLists (T_Script _ _ cmds) = doList cmds
    doLists (T_BraceGroup _ cmds) = doList cmds
    doLists (T_WhileExpression _ _ cmds) = doList cmds
    doLists (T_UntilExpression _ _ cmds) = doList cmds
    doLists (T_ForIn _ _ _ cmds) = doList cmds
    doLists (T_IfExpression _ thens elses) = do
        mapM_ (\(_, l) -> doList l) thens
        doList elses
    doLists _ = return ()

    doList t@(current:following:_) = do
        commentIfExec current
        doList (tail t)
    doList _ = return ()

    commentIfExec (T_Pipeline id list) =
      mapM_ commentIfExec list
    commentIfExec (T_Redirecting _ _ f@(
      T_SimpleCommand id _ (cmd:arg:_))) =
        when (f `isUnqualifiedCommand` "exec") $
          warn (id) $
            "Remove \"exec \" if script should continue after this command."
    commentIfExec _ = return ()


--- Subshell detection

prop_subshellAssignmentCheck = verifyFull     subshellAssignmentCheck "cat foo | while read bar; do a=$bar; done; echo \"$a\""
prop_subshellAssignmentCheck2 = verifyNotFull subshellAssignmentCheck "while read bar; do a=$bar; done < file; echo \"$a\""
prop_subshellAssignmentCheck3 = verifyFull    subshellAssignmentCheck "( A=foo; ); rm $A"
prop_subshellAssignmentCheck4 = verifyNotFull subshellAssignmentCheck "( A=foo; rm $A; )"
prop_subshellAssignmentCheck5 = verifyFull    subshellAssignmentCheck "cat foo | while read cow; do true; done; echo $cow;"
prop_subshellAssignmentCheck6 = verifyFull    subshellAssignmentCheck "( export lol=$(ls); ); echo $lol;"
prop_subshellAssignmentCheck7 = verifyFull    subshellAssignmentCheck "cmd | while read foo; do (( n++ )); done; echo \"$n lines\""
prop_subshellAssignmentCheck8 = verifyFull    subshellAssignmentCheck "n=3 & echo $((n++))"
prop_subshellAssignmentCheck9 = verifyFull    subshellAssignmentCheck "read n & n=foo$n"
prop_subshellAssignmentCheck10 = verifyFull    subshellAssignmentCheck "(( n <<= 3 )) & (( n |= 4 )) &"
prop_subshellAssignmentCheck11 = verifyFull subshellAssignmentCheck "cat /etc/passwd | while read line; do let n=n+1; done\necho $n"
prop_subshellAssignmentCheck12 = verifyFull subshellAssignmentCheck "cat /etc/passwd | while read line; do let ++n; done\necho $n"
subshellAssignmentCheck t =
    let flow = getVariableFlow t
        check = findSubshelled flow [("oops",[])] Map.empty
    in snd $ runState check []


data Scope = SubshellScope String | NoneScope deriving (Show, Eq)
data StackData =
    StackScope Scope
    | StackScopeEnd
    -- (Base expression, specific position, var name, assigned values)
    | Assignment (Token, Token, String, DataSource)
    | Reference (Token, Token, String)
  deriving (Show, Eq)
data DataSource = DataFrom [Token] | DataExternal
  deriving (Show, Eq)

data VariableState = Dead Token String | Alive deriving (Show, Eq)

leadType t =
    case t of
        T_DollarExpansion _ _  -> SubshellScope "$(..) expansion"
        T_Backticked _ _  -> SubshellScope "`..` expansion"
        T_Backgrounded _ _  -> SubshellScope "backgrounding &"
        T_Subshell _ _  -> SubshellScope "(..) group"
        -- This considers the whole pipeline one subshell. Consider fixing.
        T_Pipeline _ (_:_:[])  -> SubshellScope "pipeline"
        _ -> NoneScope


getModifiedVariables t =
    case t of
        T_SimpleCommand _ vars [] ->
            concatMap (\x -> case x of
                                T_Assignment id name w ->
                                    [(x, x, name, DataFrom [w])]
                                _ -> []
                      ) vars
        c@(T_SimpleCommand _ _ _) ->
            getModifiedVariableCommand c

        TA_Unary _ "++|" (TA_Variable id name) -> [(t, t, name, DataFrom [t])]
        TA_Unary _ "|++" (TA_Variable id name) -> [(t, t, name, DataFrom [t])]
        TA_Binary _ op (TA_Variable id name) rhs ->
            if any (==op) ["=", "*=", "/=", "%=", "+=", "-=", "<<=", ">>=", "&=", "^=", "|="]
                then [(t, t, name, DataFrom [rhs])]
                else []

        --Points to 'for' rather than variable
        T_ForIn id str words _ -> [(t, t, str, DataFrom words)]
        T_SelectIn id str words _ -> [(t, t, str, DataFrom words)]
        _ -> []

getModifiedVariableCommand base@(T_SimpleCommand _ _ ((T_NormalWord _ ((T_Literal _ x):_)):rest)) =
    case x of
        "read" -> concatMap getLiteral rest
        "export" -> concatMap exportParamToLiteral rest
        "let" -> concatMap letParamToLiteral rest
        _ -> []
  where
    stripEquals s = let rest = dropWhile (/= '=') s in
        if rest == "" then "" else tail rest
    stripEqualsFrom (T_NormalWord id1 ((T_Literal id2 s):rs)) =
        (T_NormalWord id1 ((T_Literal id2 (stripEquals s)):rs))
    stripEqualsFrom (T_NormalWord id1 [T_DoubleQuoted id2 [T_Literal id3 s]]) =
        (T_NormalWord id1 [T_DoubleQuoted id2 [T_Literal id3 (stripEquals s)]])
    stripEqualsFrom t = t

    getLiteral t@(T_NormalWord _ [T_Literal _ s]) =
        [(base, t, s, DataExternal)]
    getLiteral t@(T_NormalWord _ [T_DoubleQuoted _ [T_Literal id s]]) =
        [(base, t, s, DataExternal)]
    getLiteral x = []
    exportParamToLiteral t@(T_NormalWord _ ((T_Literal _ s):_)) =
        if '=' `elem` s
            then [(base, t, prefix, DataFrom [stripEqualsFrom t])]
            else []
      where prefix = takeWhile (/= '=') s
    exportParamToLiteral _ = []
    letParamToLiteral token =
          if var == ""
            then []
            else [(base, token, var, DataFrom [stripEqualsFrom token])]
        where var = takeWhile (isVariableChar) $ dropWhile (\x -> x `elem` "+-") $ concat $ deadSimple token
getModifiedVariableCommand _ = []

-- TODO:
getBracedReference s = takeWhile (\x -> not $ x `elem` ":[#%/^,") $ dropWhile (== '#') s

getReferencedVariables t =
    case t of
        T_DollarBraced id l -> map (\x -> (t, t, x)) $ [getBracedReference $ bracedString l]
        TA_Variable id str -> [(t, t, str)]
        x -> []

getVariableFlow t =
    let (_, stack) = runState (doStackAnalysis startScope endScope t) []
    in reverse stack
  where
    startScope t =
        let scopeType = leadType t
        in do
            when (scopeType /= NoneScope) $ modify ((StackScope scopeType):)
            if assignFirst t then setWritten t else return ()

    endScope t =
        let scopeType = leadType t
        in do
            setRead t
            if assignFirst t then return () else setWritten t
            when (scopeType /= NoneScope) $ modify ((StackScopeEnd):)

    assignFirst (T_ForIn _ _ _ _) = True
    assignFirst (T_SelectIn _ _ _ _) = True
    assignFirst _ = False

    setRead t =
        let read    = getReferencedVariables t
        in mapM_ (\v -> modify ((Reference v):)) read

    setWritten t =
        let written = getModifiedVariables t
        in mapM_ (\v -> modify ((Assignment v):)) written

findSubshelled [] _ _ = return ()
findSubshelled ((Assignment x@(_, _, str, _)):rest) ((reason,scope):lol) deadVars =
    findSubshelled rest ((reason, x:scope):lol) $ Map.insert str Alive deadVars
findSubshelled ((Reference (_, readToken, str)):rest) scopes deadVars = do
    case Map.findWithDefault Alive str deadVars of
        Alive -> return ()
        Dead writeToken reason -> do
                    info (getId writeToken) $ "Modification of " ++ str ++ " is local (to subshell caused by "++ reason ++")."
                    info (getId readToken) $ str ++ " was modified in a subshell. That change might be lost."
    findSubshelled rest scopes deadVars

findSubshelled ((StackScope (SubshellScope reason)):rest) scopes deadVars =
    findSubshelled rest ((reason,[]):scopes) deadVars

findSubshelled ((StackScopeEnd):rest) ((reason, scope):oldScopes) deadVars =
    findSubshelled rest oldScopes $
        foldl (\m (_, token, var, _) ->
            Map.insert var (Dead token reason) m) deadVars scope

doVariableFlowAnalysis readFunc writeFunc empty t = fst $ runState (
    foldM (\list x -> do { l <- doFlow x;  return $ l ++ list; }) [] flow
    ) empty
  where
    flow = getVariableFlow t
    doFlow (Reference (base, token, name)) =
        readFunc base token name
    doFlow (Assignment (base, token, name, values)) =
        writeFunc base token name values
    doFlow _ = return []

---- Spacefulness detection

prop_checkSpacefulness0 = verifyFull checkSpacefulness "for f in *.mp3; do echo $f; done"
prop_checkSpacefulness1 = verifyFull checkSpacefulness "a='cow moo'; echo $a"
prop_checkSpacefulness2 = verifyNotFull checkSpacefulness "a='cow moo'; [[ $a ]]"
prop_checkSpacefulness3 = verifyNotFull checkSpacefulness "a='cow*.mp3'; echo \"$a\""
prop_checkSpacefulness4 = verifyFull checkSpacefulness "for f in *.mp3; do echo $f; done"
prop_checkSpacefulness4a= verifyNotFull checkSpacefulness "foo=$(echo $foo)"
prop_checkSpacefulness5 = verifyFull checkSpacefulness "a='*'; b=$a; c=lol${b//foo/bar}; echo $c"
prop_checkSpacefulness6 = verifyFull checkSpacefulness "a=foo$(lol); echo $a"
prop_checkSpacefulness7 = verifyFull checkSpacefulness "a=foo\\ bar; rm $a"
prop_checkSpacefulness8 = verifyNotFull checkSpacefulness "a=foo\\ bar; a=foo; rm $a"
prop_checkSpacefulnessA = verifyFull checkSpacefulness "rm $1"
prop_checkSpacefulnessB = verifyFull checkSpacefulness "rm ${10//foo/bar}"
prop_checkSpacefulnessC = verifyNotFull checkSpacefulness "(( $1 + 3 ))"
prop_checkSpacefulnessD = verifyNotFull checkSpacefulness "if [[ $2 -gt 14 ]]; then true; fi"
prop_checkSpacefulnessE = verifyNotFull checkSpacefulness "foo=$3 env"
prop_checkSpacefulnessF = verifyNotFull checkSpacefulness "local foo=$1"
prop_checkSpacefulnessG = verifyNotFull checkSpacefulness "declare foo=$1"
prop_checkSpacefulnessH = verifyFull checkSpacefulness "echo foo=$1"

checkSpacefulness t =
    doVariableFlowAnalysis readF writeF (Map.fromList defaults) t
  where
    defaults = map (\x -> (show x, True)) [0..10]

    hasSpaces name = do
        map <- get
        return $ Map.findWithDefault False name map

    setSpaces name bool = do
        modify $ Map.insert name bool

    readF _ token name = do
        spaced <- hasSpaces name
        if spaced && (not $ inUnquotableContext parents token)
            then return [(getId token, Note InfoC warning)]
            else return []
      where
        warning = "Unquoted variable may contain spaces/globs, and will word split."

    writeF _ _ name DataExternal = do
        setSpaces name True
        return []

    writeF _ _ name (DataFrom vals) = do
        map <- get
        setSpaces name
            (isSpacefulWord (\x -> Map.findWithDefault False x map) vals)
        return []

    parents = getParentTree t
    isSpacefulWord :: (String -> Bool) -> [Token] -> Bool
    isSpacefulWord f words =
        any (isSpaceful f) words
    isSpaceful :: (String -> Bool) -> Token -> Bool
    isSpaceful spacefulF x =
        case x of
          T_DollarExpansion _ _ -> True
          T_Backticked _ _ -> True
          T_Glob _ _         -> True
          T_Extglob _ _ _    -> True
          T_Literal _ s      -> s `containsAny` globspace
          T_SingleQuoted _ s -> s `containsAny` globspace
          T_DollarBraced _ l -> spacefulF $ getBracedReference $ bracedString l
          T_NormalWord _ w   -> isSpacefulWord spacefulF w
          T_DoubleQuoted _ w -> isSpacefulWord spacefulF w
          _ -> False
      where
        globspace = "*? \t\n"
        containsAny s chars = any (\c -> c `elem` s) chars


prop_checkQuotesInLiterals1 = verifyFull checkQuotesInLiterals "param='--foo=\"bar\"'; app $param"
prop_checkQuotesInLiterals1a= verifyFull checkQuotesInLiterals "param=\"--foo='lolbar'\"; app $param"
prop_checkQuotesInLiterals2 = verifyNotFull checkQuotesInLiterals "param='--foo=\"bar\"'; app \"$param\""
prop_checkQuotesInLiterals3 =verifyNotFull checkQuotesInLiterals "param=('--foo='); app \"${param[@]}\""
prop_checkQuotesInLiterals4 = verifyNotFull checkQuotesInLiterals "param=\"don't bother with this one\"; app $param"
checkQuotesInLiterals t =
    doVariableFlowAnalysis readF writeF Map.empty t
  where
    getQuotes name = get >>= (return . Map.lookup name)
    setQuotes name ref = modify $ Map.insert name ref
    deleteQuotes = modify . Map.delete
    parents = getParentTree t
    quoteRegex = mkRegex "\"|([= ]|^)'|'( |$)"
    containsQuotes s = isJust $ matchRegex quoteRegex s

    -- Just catch the most blatant cases of foo='--cow="lol bert"'; cmd $foo, since that's 99%
    writeF _ _ name (DataFrom values) = do
        let quotedVars = filter (\v -> containsQuotes (concat $ deadSimple v)) values
        case quotedVars of
            [] -> deleteQuotes name
            x:_ -> setQuotes name (getId x)
        return []
    writeF _ _ _ _ = return []

    readF _ expr name = do
        assignment <- getQuotes name
        if isJust assignment && not (inUnquotableContext parents expr)
            then return [
                    (fromJust assignment,
                        Note WarningC "Word splitting will treat quotes as literals. Use an array."),
                    (getId expr,
                        Note WarningC "Embedded quotes in this variable will not be respected.")
                    ]
            else return []


prop_checkFunctionsUsedExternally1 =
  verifyFull checkFunctionsUsedExternally "foo() { :; }; sudo foo"
prop_checkFunctionsUsedExternally2 =
  verifyFull checkFunctionsUsedExternally "alias f='a'; xargs -n 1 f"
prop_checkFunctionsUsedExternally3 =
  verifyNotFull checkFunctionsUsedExternally "f() { :; }; echo f"
checkFunctionsUsedExternally t =
    runBasicAnalysis checkCommand t
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
    checkCommand t@(T_SimpleCommand _ _ (cmd:args)) =
        let name = getCommandFor cmd in
          when (name `elem` invokingCmds) $
            mapM_ (checkArg name) args
    checkCommand _ = return ()

    functions = Map.fromList $ runBasicAnalysis findFunctions t
    findFunctions (T_Function id name _) = modify ((name, id):)
    findFunctions t@(T_SimpleCommand id _ (_:args))
        | t `isUnqualifiedCommand` "alias" = mapM_ getAlias args
    findFunctions _ = return ()
    getAlias arg =
        let string = concat $ deadSimple arg
        in when ('=' `elem` string) $
            modify ((takeWhile (/= '=') string, getId arg):)
    checkArg cmd arg =
        case Map.lookup (concat $ deadSimple arg) functions of
            Nothing -> return ()
            Just id -> do
              warn (getId arg) $
                "Shell functions can't be passed to external commands."
              info id $
                "Use own script or sh -c '..' to run this from " ++ cmd ++ "."

