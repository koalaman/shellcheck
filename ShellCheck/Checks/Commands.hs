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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

-- This module contains checks that examine specific commands by name.
module ShellCheck.Checks.Commands (checker
    , ShellCheck.Checks.Commands.runTests
) where

import ShellCheck.AST
import ShellCheck.ASTLib
import ShellCheck.AnalyzerLib
import ShellCheck.Data
import ShellCheck.Interface
import ShellCheck.Parser
import ShellCheck.Regex

import Control.Monad
import Control.Monad.RWS
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map
import Test.QuickCheck.All (forAllProperties)
import Test.QuickCheck.Test (quickCheckWithResult, stdArgs, maxSuccess)

data CommandName = Exactly String | Basename String
    deriving (Eq, Ord)

data CommandCheck =
    CommandCheck CommandName (Token -> Analysis)


verify :: CommandCheck -> String -> Bool
verify f s = producesComments (getChecker [f]) s == Just True
verifyNot f s = producesComments (getChecker [f]) s == Just False

arguments (T_SimpleCommand _ _ (cmd:args)) = args

commandChecks :: [CommandCheck]
commandChecks = [
    checkTr
    ,checkFindNameGlob
    ,checkNeedlessExpr
    ,checkGrepRe
    ,checkTrapQuotes
    ,checkReturn
    ,checkFindExecWithSingleArgument
    ,checkUnusedEchoEscapes
    ,checkInjectableFindSh
    ,checkFindActionPrecedence
    ,checkMkdirDashPM
    ,checkNonportableSignals
    ,checkInteractiveSu
    ,checkSshCommandString
    ,checkPrintfVar
    ,checkUuoeCmd
    ,checkSetAssignment
    ,checkExportedExpansions
    ,checkAliasesUsesArgs
    ,checkAliasesExpandEarly
    ,checkUnsetGlobs
    ,checkFindWithoutPath
    ,checkTimeParameters
    ,checkTimedCommand
    ,checkLocalScope
    ,checkDeprecatedTempfile
    ,checkDeprecatedEgrep
    ,checkDeprecatedFgrep
    ,checkWhileGetoptsCase
    ,checkCatastrophicRm
    ,checkLetUsage
    ,checkMvArguments, checkCpArguments, checkLnArguments
    ]

buildCommandMap :: [CommandCheck] -> Map.Map CommandName (Token -> Analysis)
buildCommandMap = foldl' addCheck Map.empty
  where
    addCheck map (CommandCheck name function) =
        Map.insertWith composeAnalyzers name function map


checkCommand :: Map.Map CommandName (Token -> Analysis) -> Token -> Analysis
checkCommand map t@(T_SimpleCommand id _ (cmd:rest)) = fromMaybe (return ()) $ do
    name <- getLiteralString cmd
    return $
        if '/' `elem` name
        then
            Map.findWithDefault nullCheck (Basename $ basename name) map t
        else do
            Map.findWithDefault nullCheck (Exactly name) map t
            Map.findWithDefault nullCheck (Basename name) map t

  where
    basename = reverse . takeWhile (/= '/') . reverse
checkCommand _ _ = return ()

getChecker :: [CommandCheck] -> Checker
getChecker list = Checker {
    perScript = const $ return (),
    perToken = checkCommand map
    }
  where
    map = buildCommandMap list


checker :: Parameters -> Checker
checker params = getChecker commandChecks

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
checkTr = CommandCheck (Basename "tr") (mapM_ f . arguments)
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
              info (getId word) 2021 "Don't use [] around classes in tr, it replaces literal square brackets."
        Nothing -> return ()

    duplicated s =
        let relevant = filter isAlpha s
        in relevant /= nub relevant

prop_checkFindNameGlob1 = verify checkFindNameGlob "find / -name *.php"
prop_checkFindNameGlob2 = verify checkFindNameGlob "find / -type f -ipath *(foo)"
prop_checkFindNameGlob3 = verifyNot checkFindNameGlob "find * -name '*.php'"
checkFindNameGlob = CommandCheck (Basename "find") (f . arguments)  where
    acceptsGlob (Just s) = s `elem` [ "-ilname", "-iname", "-ipath", "-iregex", "-iwholename", "-lname", "-name", "-path", "-regex", "-wholename" ]
    acceptsGlob _ = False
    f [] = return ()
    f [x] = return ()
    f (a:b:r) = do
        when (acceptsGlob (getLiteralString a) && isGlob b) $ do
            let (Just s) = getLiteralString a
            warn (getId b) 2061 $ "Quote the parameter to " ++ s ++ " so the shell won't interpret it."
        f (b:r)


prop_checkNeedlessExpr = verify checkNeedlessExpr "foo=$(expr 3 + 2)"
prop_checkNeedlessExpr2 = verify checkNeedlessExpr "foo=`echo \\`expr 3 + 2\\``"
prop_checkNeedlessExpr3 = verifyNot checkNeedlessExpr "foo=$(expr foo : regex)"
prop_checkNeedlessExpr4 = verifyNot checkNeedlessExpr "foo=$(expr foo \\< regex)"
checkNeedlessExpr = CommandCheck (Basename "expr") f where
    f t =
        when (all (`notElem` exceptions) (words $ arguments t)) $
            style (getId t) 2003
                "expr is antiquated. Consider rewriting this using $((..)), ${} or [[ ]]."
    -- These operators are hard to replicate in POSIX
    exceptions = [ ":", "<", ">", "<=", ">=" ]
    words = mapMaybe getLiteralString


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
prop_checkGrepRe12= verifyNot checkGrepRe "grep -F 'Foo*' file"
prop_checkGrepRe13= verifyNot checkGrepRe "grep -- -foo bar*"
prop_checkGrepRe14= verifyNot checkGrepRe "grep -e -foo bar*"
prop_checkGrepRe15= verifyNot checkGrepRe "grep --regex -foo bar*"

checkGrepRe = CommandCheck (Basename "grep") check where
    check cmd = f cmd (arguments cmd)
    -- --regex=*(extglob) doesn't work. Fixme?
    skippable (Just s) = not ("--regex=" `isPrefixOf` s) && "-" `isPrefixOf` s
    skippable _ = False
    f _ [] = return ()
    f cmd (x:r) =
        let str = getLiteralStringExt (const $ return "_") x
        in
            if str `elem` [Just "--", Just "-e", Just "--regex"]
            then checkRE cmd r -- Regex is *after* this
            else
                if skippable str
                then f cmd r           -- Regex is elsewhere
                else checkRE cmd (x:r) -- Regex is this

    checkRE _ [] = return ()
    checkRE cmd (re:_) = do
        when (isGlob re) $
            warn (getId re) 2062 "Quote the grep pattern so the shell won't interpret it."

        unless (cmd `hasFlag` "F") $ do
            let string = concat $ oversimplify re
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


prop_checkTrapQuotes1 = verify checkTrapQuotes "trap \"echo $num\" INT"
prop_checkTrapQuotes1a= verify checkTrapQuotes "trap \"echo `ls`\" INT"
prop_checkTrapQuotes2 = verifyNot checkTrapQuotes "trap 'echo $num' INT"
prop_checkTrapQuotes3 = verify checkTrapQuotes "trap \"echo $((1+num))\" EXIT DEBUG"
checkTrapQuotes = CommandCheck (Exactly "trap") (f . arguments) where
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


prop_checkReturn1 = verifyNot checkReturn "return"
prop_checkReturn2 = verifyNot checkReturn "return 1"
prop_checkReturn3 = verifyNot checkReturn "return $var"
prop_checkReturn4 = verifyNot checkReturn "return $((a|b))"
prop_checkReturn5 = verify checkReturn "return -1"
prop_checkReturn6 = verify checkReturn "return 1000"
prop_checkReturn7 = verify checkReturn "return 'hello world'"
checkReturn = CommandCheck (Exactly "return") (f . arguments)
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


prop_checkFindExecWithSingleArgument1 = verify checkFindExecWithSingleArgument "find . -exec 'cat {} | wc -l' \\;"
prop_checkFindExecWithSingleArgument2 = verify checkFindExecWithSingleArgument "find . -execdir 'cat {} | wc -l' +"
prop_checkFindExecWithSingleArgument3 = verifyNot checkFindExecWithSingleArgument "find . -exec wc -l {} \\;"
checkFindExecWithSingleArgument = CommandCheck (Basename "find") (f . arguments)
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


prop_checkUnusedEchoEscapes1 = verify checkUnusedEchoEscapes "echo 'foo\\nbar\\n'"
prop_checkUnusedEchoEscapes2 = verifyNot checkUnusedEchoEscapes "echo -e 'foi\\nbar'"
prop_checkUnusedEchoEscapes3 = verify checkUnusedEchoEscapes "echo \"n:\\t42\""
prop_checkUnusedEchoEscapes4 = verifyNot checkUnusedEchoEscapes "echo lol"
prop_checkUnusedEchoEscapes5 = verifyNot checkUnusedEchoEscapes "echo -n -e '\n'"
checkUnusedEchoEscapes = CommandCheck (Basename "echo") (f . arguments)
  where
    isDashE = mkRegex "^-.*e"
    hasEscapes = mkRegex "\\\\[rnt]"
    f args | concat (concatMap oversimplify allButLast) `matches` isDashE =
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


prop_checkInjectableFindSh1 = verify checkInjectableFindSh "find . -exec sh -c 'echo {}' \\;"
prop_checkInjectableFindSh2 = verify checkInjectableFindSh "find . -execdir bash -c 'rm \"{}\"' ';'"
prop_checkInjectableFindSh3 = verifyNot checkInjectableFindSh "find . -exec sh -c 'rm \"$@\"' _ {} \\;"
checkInjectableFindSh = CommandCheck (Basename "find") (check . arguments)
  where
    check args = do
        let idStrings = map (\x -> (getId x, onlyLiteralString x)) args
        match pattern idStrings

    match _ [] = return ()
    match [] (next:_) = action next
    match (p:tests) ((id, arg):args) = do
        when (p arg) $ match tests args
        match (p:tests) args

    pattern = [
        (`elem` ["-exec", "-execdir"]),
        (`elem` ["sh", "bash", "dash", "ksh"]),
        (== "-c")
        ]
    action (id, arg) =
        when ("{}" `isInfixOf` arg) $
            warn id 2156 "Injecting filenames is fragile and insecure. Use parameters."


prop_checkFindActionPrecedence1 = verify checkFindActionPrecedence "find . -name '*.wav' -o -name '*.au' -exec rm {} +"
prop_checkFindActionPrecedence2 = verifyNot checkFindActionPrecedence "find . -name '*.wav' -o \\( -name '*.au' -exec rm {} + \\)"
prop_checkFindActionPrecedence3 = verifyNot checkFindActionPrecedence "find . -name '*.wav' -o -name '*.au'"
checkFindActionPrecedence = CommandCheck (Basename "find") (f . arguments)
  where
    pattern = [isMatch, const True, isParam ["-o", "-or"], isMatch, const True, isAction]
    f list | length list < length pattern = return ()
    f list@(_:rest) =
        if and (zipWith ($) pattern list)
        then warnFor (list !! (length pattern - 1))
        else f rest
    isMatch = isParam [ "-name", "-regex", "-iname", "-iregex", "-wholename", "-iwholename" ]
    isAction = isParam [ "-exec", "-execdir", "-delete", "-print", "-print0", "-fls", "-fprint", "-fprint0", "-fprintf", "-ls", "-ok", "-okdir", "-printf" ]
    isParam strs t = fromMaybe False $ do
        param <- getLiteralString t
        return $ param `elem` strs
    warnFor t = warn (getId t) 2146 "This action ignores everything before the -o. Use \\( \\) to group."


prop_checkMkdirDashPM0 = verify checkMkdirDashPM "mkdir -p -m 0755 a/b"
prop_checkMkdirDashPM1 = verify checkMkdirDashPM "mkdir -pm 0755 $dir"
prop_checkMkdirDashPM2 = verify checkMkdirDashPM "mkdir -vpm 0755 a/b"
prop_checkMkdirDashPM3 = verify checkMkdirDashPM "mkdir -pm 0755 -v a/b"
prop_checkMkdirDashPM4 = verify checkMkdirDashPM "mkdir --parents --mode=0755 a/b"
prop_checkMkdirDashPM5 = verify checkMkdirDashPM "mkdir --parents --mode 0755 a/b"
prop_checkMkdirDashPM6 = verify checkMkdirDashPM "mkdir -p --mode=0755 a/b"
prop_checkMkdirDashPM7 = verify checkMkdirDashPM "mkdir --parents -m 0755 a/b"
prop_checkMkdirDashPM8 = verifyNot checkMkdirDashPM "mkdir -p a/b"
prop_checkMkdirDashPM9 = verifyNot checkMkdirDashPM "mkdir -m 0755 a/b"
prop_checkMkdirDashPM10 = verifyNot checkMkdirDashPM "mkdir a/b"
prop_checkMkdirDashPM11 = verifyNot checkMkdirDashPM "mkdir --parents a/b"
prop_checkMkdirDashPM12 = verifyNot checkMkdirDashPM "mkdir --mode=0755 a/b"
prop_checkMkdirDashPM13 = verifyNot checkMkdirDashPM "mkdir_func -pm 0755 a/b"
prop_checkMkdirDashPM14 = verifyNot checkMkdirDashPM "mkdir -p -m 0755 singlelevel"
prop_checkMkdirDashPM15 = verifyNot checkMkdirDashPM "mkdir -p -m 0755 ../bin"
prop_checkMkdirDashPM16 = verify checkMkdirDashPM "mkdir -p -m 0755 ../bin/laden"
prop_checkMkdirDashPM17 = verifyNot checkMkdirDashPM "mkdir -p -m 0755 ./bin"
prop_checkMkdirDashPM18 = verify checkMkdirDashPM "mkdir -p -m 0755 ./bin/laden"
prop_checkMkdirDashPM19 = verifyNot checkMkdirDashPM "mkdir -p -m 0755 ./../bin"
prop_checkMkdirDashPM20 = verifyNot checkMkdirDashPM "mkdir -p -m 0755 .././bin"
prop_checkMkdirDashPM21 = verifyNot checkMkdirDashPM "mkdir -p -m 0755 ../../bin"
checkMkdirDashPM = CommandCheck (Basename "mkdir") check
  where
    check t = potentially $ do
        let flags = getAllFlags t
        dashP <- find ((\f -> f == "p" || f == "parents") . snd) flags
        dashM <- find ((\f -> f == "m" || f == "mode") . snd) flags
        -- mkdir -pm 0700 dir  is fine, so is ../dir, but dir/subdir is not.
        guard $ any couldHaveSubdirs (drop 1 $ arguments t)
        return $ warn (getId $ fst dashM) 2174 "When used with -p, -m only applies to the deepest directory."
    couldHaveSubdirs t = fromMaybe True $ do
        name <- getLiteralString t
        return $ '/' `elem` name && not (name `matches` re)
    re = mkRegex "^(\\.\\.?\\/)+[^/]+$"


prop_checkNonportableSignals1 = verify checkNonportableSignals "trap f 8"
prop_checkNonportableSignals2 = verifyNot checkNonportableSignals "trap f 0"
prop_checkNonportableSignals3 = verifyNot checkNonportableSignals "trap f 14"
prop_checkNonportableSignals4 = verify checkNonportableSignals "trap f SIGKILL"
prop_checkNonportableSignals5 = verify checkNonportableSignals "trap f 9"
prop_checkNonportableSignals6 = verify checkNonportableSignals "trap f stop"
prop_checkNonportableSignals7 = verifyNot checkNonportableSignals "trap 'stop' int"
checkNonportableSignals = CommandCheck (Exactly "trap") (f . arguments)
  where
    f args = case args of
        first:rest -> unless (isFlag first) $ mapM_ check rest
        _ -> return ()

    check param = potentially $ do
        str <- getLiteralString param
        let id = getId param
        return $ sequence_ $ mapMaybe (\f -> f id str) [
            checkNumeric,
            checkUntrappable
            ]

    checkNumeric id str = do
        guard $ not (null str)
        guard $ all isDigit str
        guard $ str /= "0" -- POSIX exit trap
        guard $ str `notElem` ["1", "2", "3", "6", "9", "14", "15" ] -- XSI
        return $ warn id 2172
            "Trapping signals by number is not well defined. Prefer signal names."

    checkUntrappable id str = do
        guard $ map toLower str `elem` ["kill", "9", "sigkill", "stop", "sigstop"]
        return $ err id 2173
            "SIGKILL/SIGSTOP can not be trapped."


prop_checkInteractiveSu1 = verify checkInteractiveSu "su; rm file; su $USER"
prop_checkInteractiveSu2 = verify checkInteractiveSu "su foo; something; exit"
prop_checkInteractiveSu3 = verifyNot checkInteractiveSu "echo rm | su foo"
prop_checkInteractiveSu4 = verifyNot checkInteractiveSu "su root < script"
checkInteractiveSu = CommandCheck (Basename "su") f
  where
    f cmd = when (length (arguments cmd) <= 1) $ do
        path <- pathTo cmd
        when (all undirected path) $
            info (getId cmd) 2117
                "To run commands as another user, use su -c or sudo."

    undirected (T_Pipeline _ _ l) = length l <= 1
    -- This should really just be modifications to stdin, but meh
    undirected (T_Redirecting _ list _) = null list
    undirected _ = True


-- This is hard to get right without properly parsing ssh args
prop_checkSshCmdStr1 = verify checkSshCommandString "ssh host \"echo $PS1\""
prop_checkSshCmdStr2 = verifyNot checkSshCommandString "ssh host \"ls foo\""
prop_checkSshCmdStr3 = verifyNot checkSshCommandString "ssh \"$host\""
checkSshCommandString = CommandCheck (Basename "ssh") (f . arguments)
  where
    nonOptions =
        filter (\x -> not $ "-" `isPrefixOf` concat (oversimplify x))
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


prop_checkPrintfVar1 = verify checkPrintfVar "printf \"Lol: $s\""
prop_checkPrintfVar2 = verifyNot checkPrintfVar "printf 'Lol: $s'"
prop_checkPrintfVar3 = verify checkPrintfVar "printf -v cow $(cmd)"
prop_checkPrintfVar4 = verifyNot checkPrintfVar "printf \"%${count}s\" var"
prop_checkPrintfVar5 = verify checkPrintfVar "printf '%s %s %s' foo bar"
prop_checkPrintfVar6 = verify checkPrintfVar "printf foo bar baz"
prop_checkPrintfVar7 = verify checkPrintfVar "printf -- foo bar baz"
prop_checkPrintfVar8 = verifyNot checkPrintfVar "printf '%s %s %s' \"${var[@]}\""
prop_checkPrintfVar9 = verifyNot checkPrintfVar "printf '%s %s %s\\n' *.png"
prop_checkPrintfVar10= verifyNot checkPrintfVar "printf '%s %s %s' foo bar baz"
prop_checkPrintfVar11= verifyNot checkPrintfVar "printf '%(%s%s)T' -1"
checkPrintfVar = CommandCheck (Exactly "printf") (f . arguments) where
    f (doubledash:rest) | getLiteralString doubledash == Just "--" = f rest
    f (dashv:var:rest) | getLiteralString dashv == Just "-v" = f rest
    f (format:params) = check format params
    f _ = return ()

    countFormats string =
        case string of
            '%':'%':rest -> countFormats rest
            '%':'(':rest -> 1 + countFormats (dropWhile (/= ')') rest)
            '%':rest -> 1 + countFormats rest
            _:rest -> countFormats rest
            [] -> 0

    check format more = do
        fromMaybe (return ()) $ do
            string <- getLiteralString format
            let vars = countFormats string

            return $ do
                when (vars == 0 && more /= []) $
                    err (getId format) 2182
                        "This printf format string has no variables. Other arguments are ignored."

                when (vars > 0
                        && length more < vars
                        && all (not . mayBecomeMultipleArgs) more) $
                    warn (getId format) 2183 $
                        "This format string has " ++ show vars ++ " variables, but is passed " ++ show (length more) ++ " arguments."


        unless ('%' `elem` concat (oversimplify format) || isLiteral format) $
          info (getId format) 2059
              "Don't use variables in the printf format string. Use printf \"..%s..\" \"$foo\"."




prop_checkUuoeCmd1 = verify checkUuoeCmd "echo $(date)"
prop_checkUuoeCmd2 = verify checkUuoeCmd "echo `date`"
prop_checkUuoeCmd3 = verify checkUuoeCmd "echo \"$(date)\""
prop_checkUuoeCmd4 = verify checkUuoeCmd "echo \"`date`\""
prop_checkUuoeCmd5 = verifyNot checkUuoeCmd "echo \"The time is $(date)\""
prop_checkUuoeCmd6 = verifyNot checkUuoeCmd "echo \"$(<file)\""
checkUuoeCmd = CommandCheck (Exactly "echo") (f . arguments) where
    msg id = style id 2005 "Useless echo? Instead of 'echo $(cmd)', just use 'cmd'."
    f [token] = when (tokenIsJustCommandOutput token) $ msg (getId token)
    f _ = return ()


prop_checkSetAssignment1 = verify checkSetAssignment "set foo 42"
prop_checkSetAssignment2 = verify checkSetAssignment "set foo = 42"
prop_checkSetAssignment3 = verify checkSetAssignment "set foo=42"
prop_checkSetAssignment4 = verifyNot checkSetAssignment "set -- if=/dev/null"
prop_checkSetAssignment5 = verifyNot checkSetAssignment "set 'a=5'"
prop_checkSetAssignment6 = verifyNot checkSetAssignment "set"
checkSetAssignment = CommandCheck (Exactly "set") (f . arguments)
  where
    f (var:value:rest) =
        let str = literal var in
            when (isVariableName str || isAssignment str) $
                msg (getId var)
    f (var:_) =
        when (isAssignment $ literal var) $
            msg (getId var)
    f _ = return ()

    msg id = warn id 2121 "To assign a variable, use just 'var=value', no 'set ..'."

    isAssignment str = '=' `elem` str
    literal (T_NormalWord _ l) = concatMap literal l
    literal (T_Literal _ str) = str
    literal _ = "*"


prop_checkExportedExpansions1 = verify checkExportedExpansions "export $foo"
prop_checkExportedExpansions2 = verify checkExportedExpansions "export \"$foo\""
prop_checkExportedExpansions3 = verifyNot checkExportedExpansions "export foo"
checkExportedExpansions = CommandCheck (Exactly "export") (check . arguments)
  where
    check = mapM_ checkForVariables
    checkForVariables f =
        case getWordParts f of
            [t@(T_DollarBraced {})] ->
                warn (getId t) 2163 "Exporting an expansion rather than a variable."
            _ -> return ()


prop_checkAliasesUsesArgs1 = verify checkAliasesUsesArgs "alias a='cp $1 /a'"
prop_checkAliasesUsesArgs2 = verifyNot checkAliasesUsesArgs "alias $1='foo'"
prop_checkAliasesUsesArgs3 = verify checkAliasesUsesArgs "alias a=\"echo \\${@}\""
checkAliasesUsesArgs = CommandCheck (Exactly "alias") (f . arguments)
  where
    re = mkRegex "\\$\\{?[0-9*@]"
    f = mapM_ checkArg
    checkArg arg =
        let string = fromJust $ getLiteralStringExt (const $ return "_") arg in
            when ('=' `elem` string && string `matches` re) $
                err (getId arg) 2142
                    "Aliases can't use positional parameters. Use a function."


prop_checkAliasesExpandEarly1 = verify checkAliasesExpandEarly "alias foo=\"echo $PWD\""
prop_checkAliasesExpandEarly2 = verifyNot checkAliasesExpandEarly "alias -p"
prop_checkAliasesExpandEarly3 = verifyNot checkAliasesExpandEarly "alias foo='echo {1..10}'"
checkAliasesExpandEarly = CommandCheck (Exactly "alias") (f . arguments)
  where
    f = mapM_ checkArg
    checkArg arg | '=' `elem` concat (oversimplify arg) =
        forM_ (take 1 $ filter (not . isLiteral) $ getWordParts arg) $
            \x -> warn (getId x) 2139 "This expands when defined, not when used. Consider escaping."
    checkArg _ = return ()


prop_checkUnsetGlobs1 = verify checkUnsetGlobs "unset foo[1]"
prop_checkUnsetGlobs2 = verifyNot checkUnsetGlobs "unset foo"
checkUnsetGlobs = CommandCheck (Exactly "unset") (mapM_ check . arguments)
  where
    check arg =
        when (isGlob arg) $
            warn (getId arg) 2184 "Quote arguments to unset so they're not glob expanded."


prop_checkFindWithoutPath1 = verify checkFindWithoutPath "find -type f"
prop_checkFindWithoutPath2 = verify checkFindWithoutPath "find"
prop_checkFindWithoutPath3 = verifyNot checkFindWithoutPath "find . -type f"
prop_checkFindWithoutPath4 = verifyNot checkFindWithoutPath "find -H -L \"$path\" -print"
prop_checkFindWithoutPath5 = verifyNot checkFindWithoutPath "find -O3 ."
prop_checkFindWithoutPath6 = verifyNot checkFindWithoutPath "find -D exec ."
checkFindWithoutPath = CommandCheck (Basename "find") f
  where
    f (T_SimpleCommand _ _ (cmd:args)) =
        unless (hasPath args) $
            info (getId cmd) 2185 "Some finds don't have a default path. Specify '.' explicitly."

    -- This is a bit of a kludge. find supports flag arguments both before and after the path,
    -- as well as multiple non-flag arguments that are not the path. We assume that all the
    -- pre-path flags are single characters, which is generally the case except for -O3.
    hasPath (first:rest) =
        let flag = fromJust $ getLiteralStringExt (const $ return "___") first in
            not ("-" `isPrefixOf` flag) || isLeadingFlag flag && hasPath rest
    hasPath [] = False
    isLeadingFlag flag = length flag <= 2 || "-O" `isPrefixOf` flag


prop_checkTimeParameters1 = verify checkTimeParameters "time -f lol sleep 10"
prop_checkTimeParameters2 = verifyNot checkTimeParameters "time sleep 10"
prop_checkTimeParameters3 = verifyNot checkTimeParameters "time -p foo"
prop_checkTimeParameters4 = verifyNot checkTimeParameters "command time -f lol sleep 10"
checkTimeParameters = CommandCheck (Exactly "time") f
  where
    f (T_SimpleCommand _ _ (cmd:args:_)) =
        whenShell [Bash, Sh] $
            let s = concat $ oversimplify args in
                when ("-" `isPrefixOf` s && s /= "-p") $
                    info (getId cmd) 2023 "The shell may override 'time' as seen in man time(1). Use 'command time ..' for that one."

    f _ = return ()

prop_checkTimedCommand1 = verify checkTimedCommand "#!/bin/sh\ntime -p foo | bar"
prop_checkTimedCommand2 = verify checkTimedCommand "#!/bin/dash\ntime ( foo; bar; )"
prop_checkTimedCommand3 = verifyNot checkTimedCommand "#!/bin/sh\ntime sleep 1"
checkTimedCommand = CommandCheck (Exactly "time") f where
    f (T_SimpleCommand _ _ (c:args@(_:_))) =
        whenShell [Sh, Dash] $ do
            let cmd = last args -- "time" is parsed with a command as argument
            when (isPiped cmd) $
                warn (getId c) 2176 "'time' is undefined for pipelines. time single stage or bash -c instead."
            when (isSimple cmd == Just False) $
                warn (getId cmd) 2177 "'time' is undefined for compound commands, time sh -c instead."
    f _ = return ()
    isPiped cmd =
        case cmd of
            T_Pipeline _ _ (_:_:_) -> True
            _ -> False
    getCommand cmd =
        case cmd of
            T_Pipeline _ _ (T_Redirecting _ _ a : _) -> return a
            _ -> fail ""
    isSimple cmd = do
        innerCommand <- getCommand cmd
        case innerCommand of
            T_SimpleCommand {} -> return True
            _ -> return False

prop_checkLocalScope1 = verify checkLocalScope "local foo=3"
prop_checkLocalScope2 = verifyNot checkLocalScope "f() { local foo=3; }"
checkLocalScope = CommandCheck (Exactly "local") $ \t ->
    whenShell [Bash, Dash] $ do -- Ksh allows it, Sh doesn't support local
        path <- getPathM t
        unless (any isFunction path) $
            err (getId t) 2168 "'local' is only valid in functions."

prop_checkDeprecatedTempfile1 = verify checkDeprecatedTempfile "var=$(tempfile)"
prop_checkDeprecatedTempfile2 = verifyNot checkDeprecatedTempfile "tempfile=$(mktemp)"
checkDeprecatedTempfile = CommandCheck (Basename "tempfile") $
    \t -> warn (getId t) 2186 "tempfile is deprecated. Use mktemp instead."

prop_checkDeprecatedEgrep = verify checkDeprecatedEgrep "egrep '.+'"
checkDeprecatedEgrep = CommandCheck (Basename "egrep") $
    \t -> info (getId t) 2196 "egrep is non-standard and deprecated. Use grep -E instead."

prop_checkDeprecatedFgrep = verify checkDeprecatedFgrep "fgrep '*' files"
checkDeprecatedFgrep = CommandCheck (Basename "fgrep") $
    \t -> info (getId t) 2197 "fgrep is non-standard and deprecated. Use grep -F instead."

prop_checkWhileGetoptsCase1 = verify checkWhileGetoptsCase "while getopts 'a:b' x; do case $x in a) foo;; esac; done"
prop_checkWhileGetoptsCase2 = verify checkWhileGetoptsCase "while getopts 'a:' x; do case $x in a) foo;; b) bar;; esac; done"
prop_checkWhileGetoptsCase3 = verifyNot checkWhileGetoptsCase "while getopts 'a:b' x; do case $x in a) foo;; b) bar;; *) :;esac; done"
prop_checkWhileGetoptsCase4 = verifyNot checkWhileGetoptsCase "while getopts 'a:123' x; do case $x in a) foo;; [0-9]) bar;; esac; done"
prop_checkWhileGetoptsCase5 = verifyNot checkWhileGetoptsCase "while getopts 'a:' x; do case $x in a) foo;; \\?) bar;; *) baz;; esac; done"
checkWhileGetoptsCase = CommandCheck (Exactly "getopts") f
  where
    f :: Token -> Analysis
    f t@(T_SimpleCommand _ _ (cmd:arg1:_))  = do
        path <- getPathM t
        potentially $ do
            options <- getLiteralString arg1
            (T_WhileExpression _ _ body) <- findFirst whileLoop path
            caseCmd <- mapMaybe findCase body !!! 0
            return $ check (getId arg1) (map (:[]) $ filter (/= ':') options) caseCmd
    f _ = return ()

    check :: Id -> [String] -> Token -> Analysis
    check optId opts (T_CaseExpression id _ list) = do
            unless (Nothing `Map.member` handledMap) $ do
                mapM_ (warnUnhandled optId id) $ catMaybes $ Map.keys notHandled

                unless (any (`Map.member` handledMap) [Just "*",Just "?"]) $
                    warn id 2220 "Invalid flags are not handled. Add a *) case."

            mapM_ warnRedundant $ Map.toList notRequested

        where
            handledMap = Map.fromList (concatMap getHandledStrings list)
            requestedMap = Map.fromList $ map (\x -> (Just x, ())) opts

            notHandled = Map.difference requestedMap handledMap
            notRequested = Map.difference handledMap requestedMap

    warnUnhandled optId caseId str =
        warn caseId 2213 $ "getopts specified -" ++ str ++ ", but it's not handled by this 'case'."

    warnRedundant (key, expr) = potentially $ do
        str <- key
        guard $ str `notElem` ["*", ":", "?"]
        return $ warn (getId expr) 2214 "This case is not specified by getopts."

    getHandledStrings (_, globs, _) =
        map (\x -> (literal x, x)) globs

    literal :: Token -> Maybe String
    literal t = do
        getLiteralString t <> fromGlob t

    fromGlob t =
        case t of
            T_Glob _ ('[':c:']':[]) -> return [c]
            T_Glob _ "*" -> return "*"
            T_Glob _ "?" -> return "?"
            _ -> Nothing

    whileLoop t =
        case t of
            T_WhileExpression {} -> return True
            T_Script {} -> return False
            _ -> Nothing

    findCase t =
        case t of
            T_Annotation _ _ x -> findCase x
            T_Pipeline _ _ [x] -> findCase x
            T_Redirecting _ _ x@(T_CaseExpression {}) -> return x
            _ -> Nothing

prop_checkCatastrophicRm1 = verify checkCatastrophicRm "rm -r $1/$2"
prop_checkCatastrophicRm2 = verify checkCatastrophicRm "rm -r /home/$foo"
prop_checkCatastrophicRm3 = verifyNot checkCatastrophicRm "rm -r /home/${USER:?}/*"
prop_checkCatastrophicRm4 = verify checkCatastrophicRm "rm -fr /home/$(whoami)/*"
prop_checkCatastrophicRm5 = verifyNot checkCatastrophicRm "rm -r /home/${USER:-thing}/*"
prop_checkCatastrophicRm6 = verify checkCatastrophicRm "rm --recursive /etc/*$config*"
prop_checkCatastrophicRm8 = verify checkCatastrophicRm "rm -rf /home"
prop_checkCatastrophicRm10= verifyNot checkCatastrophicRm "rm -r \"${DIR}\"/{.gitignore,.gitattributes,ci}"
prop_checkCatastrophicRm11= verify checkCatastrophicRm "rm -r /{bin,sbin}/$exec"
prop_checkCatastrophicRm12= verify checkCatastrophicRm "rm -r /{{usr,},{bin,sbin}}/$exec"
prop_checkCatastrophicRm13= verifyNot checkCatastrophicRm "rm -r /{{a,b},{c,d}}/$exec"
prop_checkCatastrophicRmA = verify checkCatastrophicRm "rm -rf /usr /lib/nvidia-current/xorg/xorg"
prop_checkCatastrophicRmB = verify checkCatastrophicRm "rm -rf \"$STEAMROOT/\"*"
checkCatastrophicRm = CommandCheck (Basename "rm") $ \t ->
    when (isRecursive t) $
        mapM_ (mapM_ checkWord . braceExpand) $ arguments t
  where
    isRecursive = any (`elem` ["r", "R", "recursive"]) . map snd . getAllFlags

    checkWord token =
        case getLiteralString token of
            Just str ->
                when (fixPath str `elem` importantPaths) $
                    warn (getId token) 2114 "Warning: deletes a system directory."
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
                -- This shouldn't handle non-colon cases.
                if any (`isInfixOf` var) [":?", ":-", ":="]
                then Nothing
                else return ""
        f _ = return ""

    stripTrailing c = reverse . dropWhile (== c) . reverse
    skipRepeating c (a:b:rest) | a == b && b == c = skipRepeating c (b:rest)
    skipRepeating c (a:r) = a:skipRepeating c r
    skipRepeating _ [] = []

    paths = [
        "", "/bin", "/etc", "/home", "/mnt", "/usr", "/usr/share", "/usr/local",
        "/var", "/lib", "/dev", "/media", "/boot", "/lib64", "/usr/bin"
        ]
    importantPaths = filter (not . null) $
        ["", "/", "/*", "/*/*"] >>= (\x -> map (++x) paths)


prop_checkLetUsage1 = verify checkLetUsage "let a=1"
prop_checkLetUsage2 = verifyNot checkLetUsage "(( a=1 ))"
checkLetUsage = CommandCheck (Exactly "let") f
  where
    f t = whenShell [Bash,Ksh] $ do
        style (getId t) 2219 $ "Instead of 'let expr', prefer (( expr )) ."


missingDestination handler token = do
    case params of
        [single] -> do
            unless (hasTarget || mayBecomeMultipleArgs single) $
                handler token
        _ -> return ()
  where
    args = getAllFlags token
    params = map fst $ filter (\(_,x) -> x == "") args
    hasTarget =
        any (\x -> x /= "" && x `isPrefixOf` "target-directory") $
            map snd args

prop_checkMvArguments1 = verify    checkMvArguments "mv 'foo bar'"
prop_checkMvArguments2 = verifyNot checkMvArguments "mv foo bar"
prop_checkMvArguments3 = verifyNot checkMvArguments "mv 'foo bar'{,bak}"
prop_checkMvArguments4 = verifyNot checkMvArguments "mv \"$@\""
prop_checkMvArguments5 = verifyNot checkMvArguments "mv -t foo bar"
prop_checkMvArguments6 = verifyNot checkMvArguments "mv --target-directory=foo bar"
prop_checkMvArguments7 = verifyNot checkMvArguments "mv --target-direc=foo bar"
prop_checkMvArguments8 = verifyNot checkMvArguments "mv --version"
prop_checkMvArguments9 = verifyNot checkMvArguments "mv \"${!var}\""
checkMvArguments = CommandCheck (Basename "mv") $ missingDestination f
  where
    f t = err (getId t) 2224 "This mv has no destination. Check the arguments."

checkCpArguments = CommandCheck (Basename "cp") $ missingDestination f
  where
    f t = err (getId t) 2225 "This cp has no destination. Check the arguments."

checkLnArguments = CommandCheck (Basename "ln") $ missingDestination f
  where
    f t = warn (getId t) 2226 "This ln has no destination. Check the arguments, or specify '.' explicitly."


return []
runTests =  $( [| $(forAllProperties) (quickCheckWithResult (stdArgs { maxSuccess = 1 }) ) |])
