{-
    Copyright 2012-2019 Vidar Holen

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
{-# LANGUAGE MultiWayIf #-}

-- This module contains checks that examine specific commands by name.
module ShellCheck.Checks.Commands (checker, optionalChecks, ShellCheck.Checks.Commands.runTests) where

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
import Data.Functor.Identity
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
    ,checkExit
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
    ,checkFindRedirections
    ,checkReadExpansions
    ,checkSudoRedirect
    ,checkSudoArgs
    ,checkSourceArgs
    ,checkChmodDashr
    ]

optionalChecks = map fst optionalCommandChecks
optionalCommandChecks :: [(CheckDescription, CommandCheck)]
optionalCommandChecks = [
    (newCheckDescription {
        cdName = "deprecate-which",
        cdDescription = "Suggest 'command -v' instead of 'which'",
        cdPositive = "which javac",
        cdNegative = "command -v javac"
    }, checkWhich)
    ]
optionalCheckMap = Map.fromList $ map (\(desc, check) -> (cdName desc, check)) optionalCommandChecks

prop_verifyOptionalExamples = all check optionalCommandChecks
  where
    check (desc, check) =
      verify check (cdPositive desc)
      && verifyNot check (cdNegative desc)

buildCommandMap :: [CommandCheck] -> Map.Map CommandName (Token -> Analysis)
buildCommandMap = foldl' addCheck Map.empty
  where
    addCheck map (CommandCheck name function) =
        Map.insertWith composeAnalyzers name function map


checkCommand :: Map.Map CommandName (Token -> Analysis) -> Token -> Analysis
checkCommand map t@(T_SimpleCommand id cmdPrefix (cmd:rest)) = sequence_ $ do
    name <- getLiteralString cmd
    return $
        if '/' `elem` name
        then
            Map.findWithDefault nullCheck (Basename $ basename name) map t
        else if name == "builtin" && not (null rest) then
            let t' = T_SimpleCommand id cmdPrefix rest
                selectedBuiltin = fromMaybe "" $ getLiteralString . head $ rest
            in Map.findWithDefault nullCheck (Exactly selectedBuiltin) map t'
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


checker :: AnalysisSpec -> Parameters -> Checker
checker spec params = getChecker $ commandChecks ++ optionals
  where
    keys = asOptionalChecks spec
    optionals =
        if "all" `elem` keys
        then map snd optionalCommandChecks
        else mapMaybe (\x -> Map.lookup x optionalCheckMap) keys

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
prop_checkTr12= verifyNot checkTr "tr '[=e=]' 'e'"
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
          unless ("[:" `isPrefixOf` s || "[=" `isPrefixOf` s) $
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
    acceptsGlob s = s `elem` [ "-ilname", "-iname", "-ipath", "-iregex", "-iwholename", "-lname", "-name", "-path", "-regex", "-wholename" ]
    f [] = return ()
    f (x:xs) = foldr g (const $ return ()) xs x
    g b acc a = do
        forM_ (getLiteralString a) $ \s -> when (acceptsGlob s && isGlob b) $
            warn (getId b) 2061 $ "Quote the parameter to " ++ s ++ " so the shell won't interpret it."
        acc b


prop_checkNeedlessExpr = verify checkNeedlessExpr "foo=$(expr 3 + 2)"
prop_checkNeedlessExpr2 = verify checkNeedlessExpr "foo=`echo \\`expr 3 + 2\\``"
prop_checkNeedlessExpr3 = verifyNot checkNeedlessExpr "foo=$(expr foo : regex)"
prop_checkNeedlessExpr4 = verifyNot checkNeedlessExpr "foo=$(expr foo \\< regex)"
checkNeedlessExpr = CommandCheck (Basename "expr") f where
    f t =
        when (all (`notElem` exceptions) (words $ arguments t)) $
            style (getId $ getCommandTokenOrThis t) 2003
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
prop_checkGrepRe16= verifyNot checkGrepRe "grep --include 'Foo*' file"
prop_checkGrepRe17= verifyNot checkGrepRe "grep --exclude 'Foo*' file"
prop_checkGrepRe18= verifyNot checkGrepRe "grep --exclude-dir 'Foo*' file"
prop_checkGrepRe19= verify checkGrepRe "grep -- 'Foo*' file"
prop_checkGrepRe20= verifyNot checkGrepRe "grep --fixed-strings 'Foo*' file"
prop_checkGrepRe21= verifyNot checkGrepRe "grep -o 'x*' file"
prop_checkGrepRe22= verifyNot checkGrepRe "grep --only-matching 'x*' file"
prop_checkGrepRe23= verifyNot checkGrepRe "grep '.*' file"

checkGrepRe = CommandCheck (Basename "grep") check where
    check cmd = f cmd (arguments cmd)
    -- --regex=*(extglob) doesn't work. Fixme?
    skippable s = not ("--regex=" `isPrefixOf` s) && "-" `isPrefixOf` s
    f _ [] = return ()
    f cmd (x:r) =
        let str = getLiteralStringDef "_" x
        in
            if str `elem` ["--", "-e", "--regex"]
            then checkRE cmd r -- Regex is *after* this
            else
                if skippable str
                then f cmd r           -- Regex is elsewhere
                else checkRE cmd (x:r) -- Regex is this

    checkRE _ [] = return ()
    checkRE cmd (re:_) = do
        when (isGlob re) $
            warn (getId re) 2062 "Quote the grep pattern so the shell won't interpret it."

        unless (any (`elem` flags) grepGlobFlags) $ do
            let string = concat $ oversimplify re
            if isConfusedGlobRegex string then
                warn (getId re) 2063 "Grep uses regex, but this looks like a glob."
              else sequence_ $ do
                char <- getSuspiciousRegexWildcard string
                return $ info (getId re) 2022 $
                    "Note that unlike globs, " ++ [char] ++ "* here matches '" ++ [char, char, char] ++ "' but not '" ++ wordStartingWith char ++ "'."
      where
        flags = map snd $ getAllFlags cmd
        grepGlobFlags = ["fixed-strings", "F", "include", "exclude", "exclude-dir", "o", "only-matching"]

    wordStartingWith c =
        headOrDefault (c:"test") . filter ([c] `isPrefixOf`) $ candidates
      where
        candidates =
            sampleWords ++ map (\(x:r) -> toUpper x : r) sampleWords

    getSuspiciousRegexWildcard str = case matchRegex suspicious str of
        Just [[c]] | not (str `matches` contra) -> Just c
        _ -> fail "looks good"
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
    checkExpansions (T_DollarBraced id _ _) = warning id
    checkExpansions (T_DollarArithmetic id _) = warning id
    checkExpansions _ = return ()


prop_checkReturn1 = verifyNot checkReturn "return"
prop_checkReturn2 = verifyNot checkReturn "return 1"
prop_checkReturn3 = verifyNot checkReturn "return $var"
prop_checkReturn4 = verifyNot checkReturn "return $((a|b))"
prop_checkReturn5 = verify checkReturn "return -1"
prop_checkReturn6 = verify checkReturn "return 1000"
prop_checkReturn7 = verify checkReturn "return 'hello world'"
checkReturn = CommandCheck (Exactly "return") (returnOrExit
        (\c -> err c 2151 "Only one integer 0-255 can be returned. Use stdout for other data.")
        (\c -> err c 2152 "Can only return 0-255. Other data should be written to stdout."))

prop_checkExit1 = verifyNot checkExit "exit"
prop_checkExit2 = verifyNot checkExit "exit 1"
prop_checkExit3 = verifyNot checkExit "exit $var"
prop_checkExit4 = verifyNot checkExit "exit $((a|b))"
prop_checkExit5 = verify checkExit "exit -1"
prop_checkExit6 = verify checkExit "exit 1000"
prop_checkExit7 = verify checkExit "exit 'hello world'"
checkExit = CommandCheck (Exactly "exit") (returnOrExit
        (\c -> err c 2241 "The exit status can only be one integer 0-255. Use stdout for other data.")
        (\c -> err c 2242 "Can only exit with status 0-255. Other data should be written to stdout/stderr."))

returnOrExit multi invalid = (f . arguments)
  where
    f (first:second:_) =
        multi (getId first)
    f [value] =
        when (isInvalid $ literal value) $
            invalid (getId value)
    f _ = return ()

    isInvalid s = null s || any (not . isDigit) s || length s > 5
        || let value = (read s :: Integer) in value > 255

    literal token = runIdentity $ getLiteralStringExt lit token
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
        let cmdS = getLiteralStringDef " " arg

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
checkUnusedEchoEscapes = CommandCheck (Basename "echo") f
  where
    hasEscapes = mkRegex "\\\\[rnt]"
    f cmd =
        whenShell [Sh, Bash, Ksh] $
            unless (cmd `hasFlag` "e") $
                mapM_ examine $ arguments cmd

    examine token = do
        let str = onlyLiteralString token
        when (str `matches` hasEscapes) $
            info (getId token) 2028 "echo may not expand escape sequences. Use printf."


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
    check t = sequence_ $ do
        let flags = getAllFlags t
        dashP <- find (\(_,f) -> f == "p" || f == "parents") flags
        dashM <- find (\(_,f) -> f == "m" || f == "mode") flags
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
        first:rest | not $ isFlag first -> mapM_ check rest
        _ -> return ()

    check param = sequence_ $ do
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
        path <- getPathM cmd
        when (all undirected path) $
            info (getId cmd) 2117
                "To run commands as another user, use su -c or sudo."

    undirected (T_Pipeline _ _ (_:_:_)) = False
    -- This should really just be modifications to stdin, but meh
    undirected (T_Redirecting _ (_:_) _) = False
    undirected _ = True


-- This is hard to get right without properly parsing ssh args
prop_checkSshCmdStr1 = verify checkSshCommandString "ssh host \"echo $PS1\""
prop_checkSshCmdStr2 = verifyNot checkSshCommandString "ssh host \"ls foo\""
prop_checkSshCmdStr3 = verifyNot checkSshCommandString "ssh \"$host\""
prop_checkSshCmdStr4 = verifyNot checkSshCommandString "ssh -i key \"$host\""
checkSshCommandString = CommandCheck (Basename "ssh") (f . arguments)
  where
    isOption x = "-" `isPrefixOf` (concat $ oversimplify x)
    f args =
        case partition isOption args of
            ([], hostport:r@(_:_)) -> checkArg $ last r
            _ -> return ()
    checkArg (T_NormalWord _ [T_DoubleQuoted id parts]) =
        forM_ (find (not . isConstant) parts) $
            \x -> info (getId x) 2029
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
prop_checkPrintfVar12= verify checkPrintfVar "printf '%s %s\\n' 1 2 3"
prop_checkPrintfVar13= verifyNot checkPrintfVar "printf '%s %s\\n' 1 2 3 4"
prop_checkPrintfVar14= verify checkPrintfVar "printf '%*s\\n' 1"
prop_checkPrintfVar15= verifyNot checkPrintfVar "printf '%*s\\n' 1 2"
prop_checkPrintfVar16= verifyNot checkPrintfVar "printf $'string'"
prop_checkPrintfVar17= verify checkPrintfVar "printf '%-*s\\n' 1"
prop_checkPrintfVar18= verifyNot checkPrintfVar "printf '%-*s\\n' 1 2"
prop_checkPrintfVar19= verifyNot checkPrintfVar "printf '%(%s)T'"
prop_checkPrintfVar20= verifyNot checkPrintfVar "printf '%d %(%s)T' 42"
prop_checkPrintfVar21= verify checkPrintfVar "printf '%d %(%s)T'"
checkPrintfVar = CommandCheck (Exactly "printf") (f . arguments) where
    f (doubledash:rest) | getLiteralString doubledash == Just "--" = f rest
    f (dashv:var:rest) | getLiteralString dashv == Just "-v" = f rest
    f (format:params) = check format params
    f _ = return ()

    check format more = do
        sequence_ $ do
            string <- getLiteralString format
            let formats = getPrintfFormats string
            let formatCount = length formats
            let argCount = length more

            return $ if
                | argCount == 0 && formatCount == 0 ->
                    return () -- This is fine
                | formatCount == 0 && argCount > 0 ->
                    err (getId format) 2182
                        "This printf format string has no variables. Other arguments are ignored."
                | any mayBecomeMultipleArgs more ->
                    return () -- We don't know so trust the user
                | argCount < formatCount && onlyTrailingTs formats argCount ->
                    return () -- Allow trailing %()Ts since they use the current time
                | argCount > 0 && argCount `mod` formatCount == 0 ->
                    return () -- Great: a suitable number of arguments
                | otherwise ->
                    warn (getId format) 2183 $
                        "This format string has " ++ show formatCount ++ " variables, but is passed " ++ show argCount ++ " arguments."

        unless ('%' `elem` concat (oversimplify format) || isLiteral format) $
          info (getId format) 2059
              "Don't use variables in the printf format string. Use printf '..%s..' \"$foo\"."
      where
        onlyTrailingTs format argCount =
            all (== 'T') $ drop argCount format


prop_checkGetPrintfFormats1 = getPrintfFormats "%s" == "s"
prop_checkGetPrintfFormats2 = getPrintfFormats "%0*s" == "*s"
prop_checkGetPrintfFormats3 = getPrintfFormats "%(%s)T" == "T"
prop_checkGetPrintfFormats4 = getPrintfFormats "%d%%%(%s)T" == "dT"
prop_checkGetPrintfFormats5 = getPrintfFormats "%bPassed: %d, %bFailed: %d%b, Skipped: %d, %bErrored: %d%b\\n" == "bdbdbdbdb"
getPrintfFormats = getFormats
  where
    -- Get the arguments in the string as a string of type characters,
    -- e.g. "Hello %s" -> "s" and "%(%s)T %0*d\n" -> "T*d"
    getFormats :: String -> String
    getFormats string =
        case string of
            '%':'%':rest -> getFormats rest
            '%':'(':rest ->
                case dropWhile (/= ')') rest of
                    ')':c:trailing -> c : getFormats trailing
                    _ -> ""
            '%':rest -> regexBasedGetFormats rest
            _:rest -> getFormats rest
            [] -> ""

    regexBasedGetFormats rest =
        case matchRegex re rest of
            Just [width, precision, typ, rest] ->
                (if width == "*" then "*" else "") ++
                (if precision == "*" then "*" else "") ++
                typ ++ getFormats rest
            Nothing -> take 1 rest ++ getFormats rest
      where
        -- constructed based on specifications in "man printf"
        re = mkRegex "#?-?\\+? ?0?(\\*|\\d*)\\.?(\\d*|\\*)([diouxXfFeEgGaAcsbq])(.*)"
        --            \____ _____/\___ ____/   \____ ____/\_________ _________/ \ /
        --                 V          V             V               V            V
        --               flags    field width  precision   format character     rest
        -- field width and precision can be specified with a '*' instead of a digit,
        -- in which case printf will accept one more argument for each '*' used


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
    f (var:rest)
        | (not (null rest) && isVariableName str) || isAssignment str =
            warn (getId var) 2121 "To assign a variable, use just 'var=value', no 'set ..'."
      where str = literal var
    f _ = return ()

    isAssignment str = '=' `elem` str
    literal (T_NormalWord _ l) = concatMap literal l
    literal (T_Literal _ str) = str
    literal _ = "*"


prop_checkExportedExpansions1 = verify checkExportedExpansions "export $foo"
prop_checkExportedExpansions2 = verify checkExportedExpansions "export \"$foo\""
prop_checkExportedExpansions3 = verifyNot checkExportedExpansions "export foo"
prop_checkExportedExpansions4 = verifyNot checkExportedExpansions "export ${foo?}"
checkExportedExpansions = CommandCheck (Exactly "export") (mapM_ check . arguments)
  where
    check t = sequence_ $ do
        name <- getSingleUnmodifiedBracedString t
        return . warn (getId t) 2163 $
            "This does not export '" ++ name ++ "'. Remove $/${} for that, or use ${var?} to quiet."

prop_checkReadExpansions1 = verify checkReadExpansions "read $var"
prop_checkReadExpansions2 = verify checkReadExpansions "read -r $var"
prop_checkReadExpansions3 = verifyNot checkReadExpansions "read -p $var"
prop_checkReadExpansions4 = verifyNot checkReadExpansions "read -rd $delim name"
prop_checkReadExpansions5 = verify checkReadExpansions "read \"$var\""
prop_checkReadExpansions6 = verify checkReadExpansions "read -a $var"
prop_checkReadExpansions7 = verifyNot checkReadExpansions "read $1"
prop_checkReadExpansions8 = verifyNot checkReadExpansions "read ${var?}"
checkReadExpansions = CommandCheck (Exactly "read") check
  where
    options = getGnuOpts flagsForRead
    getVars cmd = fromMaybe [] $ do
        opts <- options cmd
        return [y | (x,y) <- opts, null x || x == "a"]

    check cmd = mapM_ warning $ getVars cmd
    warning t = sequence_ $ do
        name <- getSingleUnmodifiedBracedString t
        guard $ isVariableName name   -- e.g. not $1
        return . warn (getId t) 2229 $
            "This does not read '" ++ name ++ "'. Remove $/${} for that, or use ${var?} to quiet."

-- Return the single variable expansion that makes up this word, if any.
-- e.g. $foo -> $foo, "$foo"'' -> $foo , "hello $name" -> Nothing
getSingleUnmodifiedBracedString :: Token -> Maybe String
getSingleUnmodifiedBracedString word =
    case getWordParts word of
        [T_DollarBraced _ _ l] ->
            let contents = concat $ oversimplify l
                name = getBracedReference contents
            in guard (contents == name) >> return contents
        _ -> Nothing

prop_checkAliasesUsesArgs1 = verify checkAliasesUsesArgs "alias a='cp $1 /a'"
prop_checkAliasesUsesArgs2 = verifyNot checkAliasesUsesArgs "alias $1='foo'"
prop_checkAliasesUsesArgs3 = verify checkAliasesUsesArgs "alias a=\"echo \\${@}\""
checkAliasesUsesArgs = CommandCheck (Exactly "alias") (f . arguments)
  where
    re = mkRegex "\\$\\{?[0-9*@]"
    f = mapM_ checkArg
    checkArg arg =
        let string = getLiteralStringDef "_" arg in
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
        forM_ (find (not . isLiteral) $ getWordParts arg) $
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
prop_checkFindWithoutPath7 = verifyNot checkFindWithoutPath "find --help"
prop_checkFindWithoutPath8 = verifyNot checkFindWithoutPath "find -Hx . -print"
checkFindWithoutPath = CommandCheck (Basename "find") f
  where
    f t@(T_SimpleCommand _ _ (cmd:args)) =
        unless (t `hasFlag` "help" || hasPath args) $
            info (getId cmd) 2185 "Some finds don't have a default path. Specify '.' explicitly."

    -- This is a bit of a kludge. find supports flag arguments both before and
    -- after the path, as well as multiple non-flag arguments that are not the
    -- path. We assume that all the pre-path flags are single characters from a
    -- list of GNU and macOS flags.
    hasPath (first:rest) =
        let flag = getLiteralStringDef "___" first in
            not ("-" `isPrefixOf` flag) || isLeadingFlag flag && hasPath rest
    hasPath [] = False
    isLeadingFlag flag = length flag <= 2 || all (`elem` leadingFlagChars) flag
    leadingFlagChars="-EHLPXdfsxO0123456789"


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
        unless (any isFunctionLike path) $
            err (getId $ getCommandTokenOrThis t) 2168 "'local' is only valid in functions."

prop_checkDeprecatedTempfile1 = verify checkDeprecatedTempfile "var=$(tempfile)"
prop_checkDeprecatedTempfile2 = verifyNot checkDeprecatedTempfile "tempfile=$(mktemp)"
checkDeprecatedTempfile = CommandCheck (Basename "tempfile") $
    \t -> warn (getId $ getCommandTokenOrThis t) 2186 "tempfile is deprecated. Use mktemp instead."

prop_checkDeprecatedEgrep = verify checkDeprecatedEgrep "egrep '.+'"
checkDeprecatedEgrep = CommandCheck (Basename "egrep") $
    \t -> info (getId $ getCommandTokenOrThis t) 2196 "egrep is non-standard and deprecated. Use grep -E instead."

prop_checkDeprecatedFgrep = verify checkDeprecatedFgrep "fgrep '*' files"
checkDeprecatedFgrep = CommandCheck (Basename "fgrep") $
    \t -> info (getId $ getCommandTokenOrThis t) 2197 "fgrep is non-standard and deprecated. Use grep -F instead."

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
        sequence_ $ do
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

    warnRedundant (Just str, expr)
        | str `notElem` ["*", ":", "?"] =
            warn (getId expr) 2214 "This case is not specified by getopts."
    warnRedundant _ = return ()

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

    checkWord' token = sequence_ $ do
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
        f (T_DollarBraced _ _ word) =
            let var = onlyLiteralString word in
                -- This shouldn't handle non-colon cases.
                if any (`isInfixOf` var) [":?", ":-", ":="]
                then Nothing
                else return ""
        f _ = return ""

    stripTrailing c = reverse . dropWhile (== c) . reverse
    skipRepeating c = foldr go []
      where
        go a r = a : case r of b:rest | b == c && a == b -> rest; _ -> r

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
    params = [x | (x,"") <- args]
    hasTarget =
        any (\(_,x) -> x /= "" && x `isPrefixOf` "target-directory") args

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


prop_checkFindRedirections1 = verify    checkFindRedirections "find . -exec echo {} > file \\;"
prop_checkFindRedirections2 = verifyNot checkFindRedirections "find . -exec echo {} \\; > file"
prop_checkFindRedirections3 = verifyNot checkFindRedirections "find . -execdir sh -c 'foo > file' \\;"
checkFindRedirections = CommandCheck (Basename "find") f
  where
    f t = do
        redirecting <- getClosestCommandM t
        case redirecting of
            Just (T_Redirecting _ redirs@(_:_) (T_SimpleCommand _ _ args@(_:_:_))) -> do
                -- This assumes IDs are sequential, which is mostly but not always true.
                let minRedir = minimum $ map getId redirs
                let maxArg   = maximum $ map getId args
                when (minRedir < maxArg) $
                    warn minRedir 2227
                        "Redirection applies to the find command itself. Rewrite to work per action (or move to end)."
            _ -> return ()

prop_checkWhich = verify checkWhich "which '.+'"
checkWhich = CommandCheck (Basename "which") $
    \t -> info (getId $ getCommandTokenOrThis t) 2230 "which is non-standard. Use builtin 'command -v' instead."

prop_checkSudoRedirect1 = verify checkSudoRedirect "sudo echo 3 > /proc/file"
prop_checkSudoRedirect2 = verify checkSudoRedirect "sudo cmd < input"
prop_checkSudoRedirect3 = verify checkSudoRedirect "sudo cmd >> file"
prop_checkSudoRedirect4 = verify checkSudoRedirect "sudo cmd &> file"
prop_checkSudoRedirect5 = verifyNot checkSudoRedirect "sudo cmd 2>&1"
prop_checkSudoRedirect6 = verifyNot checkSudoRedirect "sudo cmd 2> log"
prop_checkSudoRedirect7 = verifyNot checkSudoRedirect "sudo cmd > /dev/null 2>&1"
checkSudoRedirect = CommandCheck (Basename "sudo") f
  where
    f t = do
        t_redir <- getClosestCommandM t
        case t_redir of
            Just (T_Redirecting _ redirs _) ->
                mapM_ warnAbout redirs
    warnAbout (T_FdRedirect _ s (T_IoFile id op file))
        | (null s || s == "&") && not (special file) =
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

prop_checkSudoArgs1 = verify checkSudoArgs "sudo cd /root"
prop_checkSudoArgs2 = verify checkSudoArgs "sudo export x=3"
prop_checkSudoArgs3 = verifyNot checkSudoArgs "sudo ls /usr/local/protected"
prop_checkSudoArgs4 = verifyNot checkSudoArgs "sudo ls && export x=3"
prop_checkSudoArgs5 = verifyNot checkSudoArgs "sudo echo ls"
prop_checkSudoArgs6 = verifyNot checkSudoArgs "sudo -n -u export ls"
prop_checkSudoArgs7 = verifyNot checkSudoArgs "sudo docker export foo"
checkSudoArgs = CommandCheck (Basename "sudo") f
  where
    f t = sequence_ $ do
        opts <- parseOpts t
        let nonFlags = [x | ("",x) <- opts]
        commandArg <- nonFlags !!! 0
        command <- getLiteralString commandArg
        guard $ command `elem` builtins
        return $ warn (getId t) 2232 $ "Can't use sudo with builtins like " ++ command ++ ". Did you want sudo sh -c .. instead?"
    builtins = [ "cd", "eval", "export", "history", "read", "source", "wait" ]
    -- This mess is why ShellCheck prefers not to know.
    parseOpts = getBsdOpts "vAknSbEHPa:g:h:p:u:c:T:r:"

prop_checkSourceArgs1 = verify checkSourceArgs "#!/bin/sh\n. script arg"
prop_checkSourceArgs2 = verifyNot checkSourceArgs "#!/bin/sh\n. script"
prop_checkSourceArgs3 = verifyNot checkSourceArgs "#!/bin/bash\n. script arg"
checkSourceArgs = CommandCheck (Exactly ".") f
  where
    f t = whenShell [Sh, Dash] $
        case arguments t of
            (file:arg1:_) -> warn (getId arg1) 2240 $
                "The dot command does not support arguments in sh/dash. Set them as variables."
            _ -> return ()

prop_checkChmodDashr1 = verify checkChmodDashr "chmod -r 0755 dir"
prop_checkChmodDashr2 = verifyNot checkChmodDashr "chmod -R 0755 dir"
prop_checkChmodDashr3 = verifyNot checkChmodDashr "chmod a-r dir"
checkChmodDashr = CommandCheck (Basename "chmod") f
  where
    f t = mapM_ check $ arguments t
    check t = sequence_ $ do
        flag <- getLiteralString t
        guard $ flag == "-r"
        return $ warn (getId t) 2253 "Use -R to recurse, or explicitly a-r to remove read permissions."

return []
runTests =  $( [| $(forAllProperties) (quickCheckWithResult (stdArgs { maxSuccess = 1 }) ) |])
