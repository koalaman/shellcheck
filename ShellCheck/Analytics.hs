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

checks = concat [
    map runBasicAnalysis basicChecks
    ,[subshellAssignmentCheck]
    ,[checkSpacefulness]
    ,[checkUnquotedExpansions]
    ]

runAllAnalytics = checkList checks
checkList l t m = foldl (\x f -> f t x) m l

runBasicAnalysis f t m = snd $ runState (doAnalysis f t) m
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
    ,checkSingleQuotedVariables
    ,checkUnquotedN
    ,checkNumberComparisons
    ,checkNoaryWasBinary
    ,checkBraceExpansionVars
    ,checkForDecimals
    ,checkDivBeforeMult
    ,checkArithmeticDeref
    ,checkComparisonAgainstGlob
    ,checkPrintfVar
    ,checkCommarrays
    ,checkOrNeq
    ,checkEcho
    ,checkConstantIfs
    ,checkTrAZ
    ,checkPipedAssignment
    ,checkAssignAteCommand
    ,checkUuoe
    ,checkFindNameGlob
    ,checkGrepRe
    ,checkDollarArithmeticCommand
    ,checkNeedlessCommands
    ,checkQuotedCondRegex 
    ,checkForInCat
    ]

modifyMap = modify
addNoteFor id note = modifyMap $ Map.adjust (\(Metadata pos notes) -> Metadata pos (note:notes)) id
warn id note = addNoteFor id $ Note WarningC $ note
err id note = addNoteFor id $ Note ErrorC $ note
info id note = addNoteFor id $ Note InfoC $ note
style id note = addNoteFor id $ Note StyleC $ note

willSplit x =
  case x of
    T_DollarBraced _ _ -> True
    T_DollarExpansion _ _ -> True
    T_Backticked _ _ -> True
    T_BraceExpansion _ s -> True
    T_Extglob _ _ _ -> True
    T_NormalWord _ l -> any willSplit l
    T_Literal _ s -> isGlobLiteral s
    _ -> False

isGlobLiteral str = any (`elem` str) "*?"

isGlob (T_Extglob _ _ _) = True
isGlob (T_Literal _ s) = isGlobLiteral s
isGlob (T_NormalWord _ l) = any isGlob l
isGlob _ = False

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
deadSimple (T_Pipeline _ [x]) = deadSimple x
deadSimple (T_Literal _ x) = [x]
deadSimple (T_SimpleCommand _ vars words) = concatMap (deadSimple) words
deadSimple (T_Redirecting _ _ foo) = deadSimple foo
deadSimple _ = []

verify f s = checkBasic f s == Just True
verifyNot f s = checkBasic f s == Just False
verifyFull f s = checkFull f s == Just True
verifyNotFull f s = checkFull f s == Just False

checkBasic f s = checkFull (runBasicAnalysis f) s
checkFull f s = case parseShell "-" s of
        (ParseResult (Just (t, m)) _) -> Just . not $ (notesFromMap $ f t m) == (notesFromMap m)
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
    for [["ls"], ["grep"]] $   \id -> warn id "Don't use ls | grep. Use a for loop with a condition in it."
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

prop_checkForInQuoted = verify checkForInQuoted "for f in \"$(ls)\"; do echo foo; done"
prop_checkForInQuoted2 = verifyNot checkForInQuoted "for f in \"$@\"; do echo foo; done"
prop_checkForInQuoted3 = verify checkForInQuoted "for f in 'find /'; do true; done"
checkForInQuoted (T_ForIn _ f [T_NormalWord _ [T_DoubleQuoted id list]] _) =
    when (any (\x -> willSplit x && not (isMagicInQuotes x)) list) $
        err id $ "Since you double quoted this, it will not word split, and the loop will only run once."
checkForInQuoted (T_ForIn _ f [T_NormalWord _ [T_SingleQuoted id s]] _) =
    warn id $ "This is a literal string. To run as a command, use $(" ++ s ++ ")."
checkForInQuoted _ = return ()

prop_checkForInCat1 = verify checkForInCat "for f in $(cat foo); do stuff; done"
prop_checkForInCat2 = verify checkForInCat "for f in $(cat foo | grep lol); do stuff; done"
prop_checkForInCat3 = verifyNot checkForInCat "for f in $(cat foo | grep bar | wc -l); do stuff; done"
checkForInCat (T_ForIn _ f [T_NormalWord _ w] _) = mapM_ checkF w
  where
    checkF (T_DollarExpansion id [T_Pipeline _ r])
        | all isLineBased r =
            info id $ "To read lines rather than words, pipe/redirect to a 'while read' loop."
    checkF _ = return ()
    isLineBased cmd = any (cmd `isCommand`) ["grep", "sed", "cat"]
checkForInCat _ = return ()

prop_checkForInLs = verify checkForInLs "for f in $(ls *.mp3); do mplayer \"$f\"; done"
checkForInLs (T_ForIn _ f [T_NormalWord _ [T_DollarExpansion id [x]]] _) =
    case deadSimple x of ("ls":n) -> let args = (if n == [] then ["*"] else n) in
                                        err id $ "Don't use 'for "++f++" in $(ls " ++ (intercalate " " n) ++ ")'. Use 'for "++f++" in "++ (intercalate " " args) ++ "' ."
                         _ -> return ()
checkForInLs _ = return ()


prop_checkUnquotedExpansions1 = verifyFull checkUnquotedExpansions "rm $(ls)"
prop_checkUnquotedExpansions2 = verifyFull checkUnquotedExpansions "rm foo$(date)"
prop_checkUnquotedExpansions3 = verifyFull checkUnquotedExpansions "[ $(foo) == cow ]"
prop_checkUnquotedExpansions3a= verifyFull checkUnquotedExpansions "[ ! $(foo) ]"
prop_checkUnquotedExpansions4 = verifyNotFull checkUnquotedExpansions "[[ $(foo) == cow ]]"
prop_checkUnquotedExpansions5 = verifyNotFull checkUnquotedExpansions "for f in $(cmd); do echo $f; done"
checkUnquotedExpansions t metaMap =
    runBasicAnalysis check t metaMap
  where
    tree = getParentTree t
    msg id = warn id "Quote this to prevent word splitting."
    check (T_NormalWord _ l) = mapM_ check' l
    check _ = return ()

    check' t@(T_DollarExpansion id _) = unless (inUnquotableContext tree t) $ msg id
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
prop_checkUnquotedDollarAt2 = verifyNot checkUnquotedDollarAt "ls \"$@\""
checkUnquotedDollarAt (T_NormalWord _ [T_DollarBraced id l]) | '@' `elem` (bracedString l) =
    err id $ "Add double quotes around ${" ++ (bracedString l) ++ "}, otherwise it's just like $* and breaks on spaces."
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


prop_checkSingleQuotedVariables = verify checkSingleQuotedVariables "echo '$foo'"
prop_checkSingleQuotedVariables2 = verify checkSingleQuotedVariables "echo 'lol$1.jpg'"
prop_checkSingleQuotedVariables3 = verifyNot checkSingleQuotedVariables "sed 's/foo$/bar/'"
checkSingleQuotedVariables (T_SingleQuoted id s) =
            case matchRegex checkSingleQuotedVariablesRe s of
                Just [var] -> info id $ var ++ " won't be expanded in single quotes."
                _          -> return ()
checkSingleQuotedVariables _ = return ()
checkSingleQuotedVariablesRe = mkRegex "(\\$[0-9a-zA-Z_]+)"


prop_checkUnquotedN = verify checkUnquotedN "if [ -n $foo ]; then echo cow; fi"
prop_checkUnquotedN2 = verify checkUnquotedN "[ -n $cow ]"
prop_checkUnquotedN3 = verifyNot checkUnquotedN "[[ -n $foo ]] && echo cow"
checkUnquotedN (T_Condition _ SingleBracket (TC_Unary _ SingleBracket "-n" (T_NormalWord id [t]))) | willSplit t =
       err id "Always true because you failed to quote. Use [[ ]] instead."
checkUnquotedN _ = return ()

prop_checkNumberComparisons1 = verify checkNumberComparisons "[[ $foo < 3 ]]"
prop_checkNumberComparisons2 = verify checkNumberComparisons "[[ 0 >= $(cmd) ]]"
prop_checkNumberComparisons3 = verifyNot checkNumberComparisons "[[ $foo ]] > 3"
prop_checkNumberComparisons4 = verify checkNumberComparisons "[ $foo > $bar ]"
prop_checkNumberComparisons5 = verify checkNumberComparisons "until [ $n <= $z ]; do echo foo; done"
checkNumberComparisons (TC_Binary id typ op lhs rhs)
    | op `elem` ["<", ">", "<=", ">="] = do
        when (isNum lhs || isNum rhs) $ err id $ "\"" ++ op ++ "\" is for string comparisons. Use " ++ (eqv op) ++" ."
        when (typ == SingleBracket) $ err id $ "Can't use " ++ op ++" in [ ]. Use [[ ]]."
    where
        isNum t = case deadSimple t of [v] -> all isDigit v
                                       _ -> False
        eqv "<" = "-lt"
        eqv ">" = "-gt"
        eqv "<=" = "-le"
        eqv ">=" = "-ge"
        eqv _ = "the numerical equivalent"
checkNumberComparisons _ = return ()

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
checkNoaryWasBinary (TC_Noary _ _ t@(T_NormalWord id l)) = do
    let str = concat $ deadSimple t
    when ('=' `elem` str) $ err id $ "Always true because you didn't put spaces around the = ."
checkNoaryWasBinary _ = return ()

prop_checkBraceExpansionVars = verify checkBraceExpansionVars "echo {1..$n}"
checkBraceExpansionVars (T_BraceExpansion id s) | '$' `elem` s =
    warn id $ "You can't use variables in brace expansions."
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
checkArithmeticDeref (TA_Expansion _ (T_DollarBraced id l)) | not $ any (`elem` "/.:#%?*@") $ bracedString l =
    style id $ "Don't use $ on variables in (( ))."
checkArithmeticDeref _ = return ()


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
        T_Redirecting _ _ _ -> False
        T_DoubleQuoted _ _ -> True
        T_CaseExpression _ _ _ -> True
        T_ForIn _ _ _ _ -> True -- Pragmatically assume it's desirable here
        x -> case Map.lookup (getId x) tree of
                Nothing -> False
                Just parent -> inUnquotableContext tree parent

--- Command specific checks

checkCommand str f (T_SimpleCommand id _ cmd) =
    case cmd of
        (w:rest) -> if w `isCommand` str then f rest else return ()
        _ -> return ()
checkCommand _ _ _ = return ()

getLiteralString t = g t
  where
    allInList l = let foo = map g l in if all isJust foo then return $ concat (catMaybes foo) else Nothing
    g s@(T_DoubleQuoted _ l) = allInList l
    g s@(T_NormalWord _ l) = allInList l
    g (T_SingleQuoted _ s) = return s
    g (T_Literal _ s) = return s
    g _ = Nothing

isLiteral t = isJust $ getLiteralString t

isCommand (T_Redirecting _ _ w) str = 
    isCommand w str
isCommand (T_SimpleCommand _ _ (w:_)) str = 
    isCommand w str
isCommand token str =
    case getLiteralString token of
        Just cmd -> cmd == str || ("/" ++ str) `isSuffixOf` cmd
        Nothing -> False

prop_checkPrintfVar1 = verify checkPrintfVar "printf \"Lol: $s\""
prop_checkPrintfVar2 = verifyNot checkPrintfVar "printf 'Lol: $s'"
prop_checkPrintfVar3 = verify checkPrintfVar "printf -v cow $(cmd)"
checkPrintfVar = checkCommand "printf" f where
    f (dashv:var:rest) | getLiteralString dashv == (Just "-v") = f rest
    f (format:params) = check format
    f _ = return ()
    check format =
        if not $ isLiteral format
          then warn (getId format) $ "Don't use variables in the printf format string. Use printf \"%s\" \"$foo\"."
          else return ()

prop_checkUuoe1 = verify checkUuoe "echo $(date)"
prop_checkUuoe2 = verify checkUuoe "echo \"$(date)\""
prop_checkUuoe3 = verifyNot checkUuoe "echo \"The time is $(date)\""
checkUuoe = checkCommand "echo" f where
    msg id = style id "Useless echo? Instead of 'echo $(cmd)', just use 'cmd'."
    f [T_NormalWord id [(T_DollarExpansion _ _)]] = msg id
    f [T_NormalWord id [T_DoubleQuoted _ [(T_DollarExpansion _ _)]]] = msg id
    f _ = return ()

prop_checkTrAZ1 = verify checkTrAZ "tr [a-f] [A-F]"
prop_checkTrAZ2 = verify checkTrAZ "tr 'a-z' 'A-Z'"
prop_checkTrAZ3 = verifyNot checkTrAZ "tr -d '[:lower:]'"
prop_checkTrAZ4 = verifyNot checkTrAZ "ls [a-z]"
checkTrAZ = checkCommand "tr" (mapM_ f)
  where
    f word = case getLiteralString word of
                Just "a-z" -> info (getId word) "Use '[:lower:]' to support accents and foreign alphabets."
                Just "A-Z" -> info (getId word) "Use '[:upper:]' to support accents and foreign alphabets."
                Just s -> unless ("[:" `isPrefixOf` s) $
                            when ("[" `isPrefixOf` s && "]" `isSuffixOf` s && (length s > 2)) $
                                info (getId word) "Don't use [] around ranges in tr, it replaces literal square brackets."
                Nothing -> return ()

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
checkGrepRe = checkCommand "grep" f where
    -- --regex=*(extglob) doesn't work. Fixme?
    skippable (Just s) = not ("--regex=" `isPrefixOf` s) && "-" `isPrefixOf` s
    skippable _ = False
    f [] = return ()
    f (x:r) | skippable (getLiteralString x) = f r
    f (re:_) = do
        when (isGlob re) $ do
            warn (getId re) $ "Quote the grep pattern so the shell won't interpret it."


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
subshellAssignmentCheck t map =
    let flow = getVariableFlow t
        check = findSubshelled flow [("oops",[])] Map.empty
    in snd $ runState check map


data Scope = SubshellScope String | NoneScope deriving (Show, Eq)
data StackData = StackScope Scope | StackScopeEnd | Assignment (Id, String) | Reference (Id, String) deriving (Show, Eq)
data VariableState = Dead Id String | Alive deriving (Show, Eq)
data VariableType = Spaceful | Spaceless deriving (Show, Eq)

leadType t =
    case t of
        T_DollarExpansion _ _  -> SubshellScope "$(..) expansion"
        T_Backgrounded _ _  -> SubshellScope "backgrounding &"
        T_Subshell _ _  -> SubshellScope "(..) group"
        -- This considers the whole pipeline one subshell. Consider fixing.
        T_Pipeline _ (_:_:[])  -> SubshellScope "pipeline"
        _ -> NoneScope


getModifiedVariables t =
    let l = getModifiedVariablesWithType (const False) t
    in map (\(id, name, typ) -> (id, name)) l

getModifiedVariablesWithType spacefulF t =
    case t of
        T_SimpleCommand _ vars [] ->
            concatMap (\x -> case x of
                                T_Assignment id name w -> [(id, name, if isSpaceful spacefulF w then Spaceful else Spaceless)]
                                _ -> []
                      ) vars
        c@(T_SimpleCommand _ _ _) ->
            getModifiedVariableCommand c

        TA_Unary _ "++|" (TA_Variable id name) -> [(id, name, Spaceless)]
        TA_Unary _ "|++" (TA_Variable id name) -> [(id, name, Spaceless)]
        TA_Binary _ op (TA_Variable id name) _ -> if any (==op) ["=", "*=", "/=", "%=", "+=", "-=", "<<=", ">>=", "&=", "^=", "|="]
                                                    then [(id,name, Spaceless)]
                                                    else []

        --Points to 'for' rather than variable
        T_ForIn id str words _ -> [(id, str, if any (isSpaceful spacefulF) words then Spaceful else Spaceless)]
        _ -> []

isSpaceful :: (String -> Bool) -> Token -> Bool
isSpaceful spacefulF x =
    case x of
      T_DollarExpansion  _ _ -> True
      T_Extglob _ _ _    -> True
      T_Literal _ s      -> s `containsAny` globspace
      T_SingleQuoted _ s -> s `containsAny` globspace
      T_DollarBraced _ l -> spacefulF $ getBracedReference $ bracedString l
      T_NormalWord _ w   -> isSpacefulWord spacefulF w
      T_DoubleQuoted _ w -> isSpacefulWord spacefulF w
      _ -> False
  where
    globspace = "* \t\n"
    containsAny s chars = any (\c -> c `elem` s) chars

isSpacefulWord :: (String -> Bool) -> [Token] -> Bool
isSpacefulWord f words =
    any (isSpaceful f) words

getModifiedVariableCommand (T_SimpleCommand _ _ ((T_NormalWord _ ((T_Literal _ x):_)):rest)) =
    case x of
        "read" -> concatMap getLiteral rest
        "export" -> concatMap exportParamToLiteral rest
        _ -> []
getModifiedVariableCommand _ = []

getLiteral (T_NormalWord _ [T_Literal id s]) = [(id,s, Spaceful)]
getLiteral (T_NormalWord _ [T_DoubleQuoted _ [T_Literal id s]]) = [(id,s,Spaceful)]
getLiteral x = []

exportParamToLiteral (T_NormalWord _ ((T_Literal id s):_)) =
    [(id,prefix,Spaceless)] -- Todo, make this determine spacefulness
    where prefix = takeWhile (/= '=') s
exportParamToLiteral _ = []

-- TODO:
getBracedReference s = takeWhile (\x -> not $ x `elem` ":[#%/^,") $ dropWhile (== '#') s

getReferencedVariables t =
    case t of
        T_DollarBraced id l -> map (\x -> (id, x)) $ [getBracedReference $ bracedString l]
        TA_Variable id str -> [(id,str)]
        x -> []

getVariableFlow t =
    let (_, stack) = runState (doStackAnalysis startScope endScope t) []
    in reverse stack
  where
    startScope t =
        let scopeType = leadType t
        in do
            when (scopeType /= NoneScope) $ modify ((StackScope scopeType):)

    endScope t =
        let scopeType = leadType t
            read    = getReferencedVariables t
            written = getModifiedVariables t
        in do
            when (scopeType /= NoneScope) $ modify ((StackScopeEnd):)
            mapM_ (\v -> modify ((Reference v):)) read
            mapM_ (\v -> modify ((Assignment v):)) written

findSubshelled :: [StackData] -> [(String, [(Id,String)])] -> (Map.Map String VariableState) -> State (Map.Map Id Metadata) ()
findSubshelled [] _ _ = return ()
findSubshelled ((Assignment x@(id, str)):rest) ((reason,scope):lol) deadVars =
    findSubshelled rest ((reason, x:scope):lol) $ Map.insert str Alive deadVars
findSubshelled ((Reference (readId, str)):rest) scopes deadVars = do
    case Map.findWithDefault Alive str deadVars of
        Alive -> return ()
        Dead writeId reason -> do
                    info writeId $ "Modification of " ++ str ++ " is local (to subshell caused by "++ reason ++")."
                    info readId $ str ++ " was modified in a subshell. That change might be lost."
    findSubshelled rest scopes deadVars

findSubshelled ((StackScope (SubshellScope reason)):rest) scopes deadVars =
    findSubshelled rest ((reason,[]):scopes) deadVars

findSubshelled ((StackScopeEnd):rest) ((reason, scope):oldScopes) deadVars =
    findSubshelled rest oldScopes $ foldl (\m (id, var) -> Map.insert var (Dead id reason) m) deadVars scope


---- Spacefulness detection

prop_checkSpacefulness1 = verifyFull checkSpacefulness "a='cow moo'; echo $a"
prop_checkSpacefulness2 = verifyNotFull checkSpacefulness "a='cow moo'; [[ $a ]]"
prop_checkSpacefulness3 = verifyNotFull checkSpacefulness "a='cow*.mp3'; echo \"$a\""
prop_checkSpacefulness4 = verifyFull checkSpacefulness "for f in *.mp3; do echo $f; done"
prop_checkSpacefulness5 = verifyFull checkSpacefulness "a='*'; b=$a; c=lol${b//foo/bar}; echo $c"
prop_checkSpacefulness6 = verifyFull checkSpacefulness "a=foo$(lol); echo $a"
prop_checkSpacefulness7 = verifyFull checkSpacefulness "a=foo\\ bar; rm $a"
prop_checkSpacefulness8 = verifyNotFull checkSpacefulness "a=foo\\ bar; a=foo; rm $a"
prop_checkSpacefulnessA = verifyFull checkSpacefulness "rm $1"
prop_checkSpacefulnessB = verifyFull checkSpacefulness "rm ${10//foo/bar}"
prop_checkSpacefulnessC = verifyNotFull checkSpacefulness "(( $1 + 3 ))"
prop_checkSpacefulnessD = verifyNotFull checkSpacefulness "if [[ $2 -gt 14 ]]; then true; fi"
prop_checkSpacefulnessE = verifyNotFull checkSpacefulness "foo=$3 env"

checkSpacefulness t metaMap =
    let (_, (newMetaMap, spaceMap)) = runState (doStackAnalysis startScope endScope t) (metaMap, Map.empty)
    in newMetaMap
  where
    isSpaceless m s = (not $ all isDigit s) && (Map.findWithDefault Spaceless s m) == Spaceless
    addInfo :: (Id, String) -> State (Map.Map Id Metadata, Map.Map String VariableType) ()
    addInfo (id, s) = do
        (metaMap, spaceMap) <- get
        when (not (inUnquotableContext parents (Map.findWithDefault undefined id items)) && not (isSpaceless spaceMap s)) $ do
            let note = Note InfoC "This variable may contain spaces/globs. Quote it unless you want splitting."
            let mm = Map.adjust (\(Metadata pos notes) -> Metadata pos (note:notes)) id metaMap
            put (mm, spaceMap)

    registerSpacing (id, s, typ) = do
        (metaMap, spaceMap) <- get
        put (metaMap, Map.insert s typ spaceMap)

    parents = getParentTree t
    items = getTokenMap t

    endScope _ = return ()

    startScope t = do
        (_, spaceMap) <- get
        let
            isSpaceful id = (Map.findWithDefault Spaceless id spaceMap) /= Spaceless
            read    = getReferencedVariables t
            written = getModifiedVariablesWithType isSpaceful t

        mapM_ addInfo read
        mapM_ registerSpacing written

