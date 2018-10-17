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
-- This module contains checks that examine specific commands by name.
module ShellCheck.Checks.Commands (checker) where

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
    ,checkFindRedirections
    ,checkReadExpansions
    ,checkWhich
    ,checkSudoRedirect
    ,checkSudoArgs
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

-- |
-- >>> prop $ verify checkTr "tr [a-f] [A-F]"
-- >>> prop $ verify checkTr "tr 'a-z' 'A-Z'"
-- >>> prop $ verify checkTr "tr '[a-z]' '[A-Z]'"
-- >>> prop $ verifyNot checkTr "tr -d '[:lower:]'"
-- >>> prop $ verifyNot checkTr "tr -d '[:upper:]'"
-- >>> prop $ verifyNot checkTr "tr -d '|/_[:upper:]'"
-- >>> prop $ verifyNot checkTr "ls [a-z]"
-- >>> prop $ verify checkTr "tr foo bar"
-- >>> prop $ verify checkTr "tr 'hello' 'world'"
-- >>> prop $ verifyNot checkTr "tr aeiou _____"
-- >>> prop $ verifyNot checkTr "a-z n-za-m"
-- >>> prop $ verifyNot checkTr "tr --squeeze-repeats rl lr"
-- >>> prop $ verifyNot checkTr "tr abc '[d*]'"
-- >>> prop $ verifyNot checkTr "tr '[=e=]' 'e'"
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

-- |
-- >>> prop $ verify checkFindNameGlob "find / -name *.php"
-- >>> prop $ verify checkFindNameGlob "find / -type f -ipath *(foo)"
-- >>> prop $ verifyNot checkFindNameGlob "find * -name '*.php'"
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


-- |
-- >>> prop $ verify checkNeedlessExpr "foo=$(expr 3 + 2)"
-- >>> prop $ verify checkNeedlessExpr "foo=`echo \\`expr 3 + 2\\``"
-- >>> prop $ verifyNot checkNeedlessExpr "foo=$(expr foo : regex)"
-- >>> prop $ verifyNot checkNeedlessExpr "foo=$(expr foo \\< regex)"
checkNeedlessExpr = CommandCheck (Basename "expr") f where
    f t =
        when (all (`notElem` exceptions) (words $ arguments t)) $
            style (getId $ getCommandTokenOrThis t) 2003
                "expr is antiquated. Consider rewriting this using $((..)), ${} or [[ ]]."
    -- These operators are hard to replicate in POSIX
    exceptions = [ ":", "<", ">", "<=", ">=" ]
    words = mapMaybe getLiteralString


-- |
-- >>> prop $ verify checkGrepRe "cat foo | grep *.mp3"
-- >>> prop $ verify checkGrepRe "grep -Ev cow*test *.mp3"
-- >>> prop $ verify checkGrepRe "grep --regex=*.mp3 file"
-- >>> prop $ verifyNot checkGrepRe "grep foo *.mp3"
-- >>> prop $ verifyNot checkGrepRe "grep-v  --regex=moo *"
-- >>> prop $ verifyNot checkGrepRe "grep foo \\*.mp3"
-- >>> prop $ verify checkGrepRe "grep *foo* file"
-- >>> prop $ verify checkGrepRe "ls | grep foo*.jpg"
-- >>> prop $ verifyNot checkGrepRe "grep '[0-9]*' file"
-- >>> prop $ verifyNot checkGrepRe "grep '^aa*' file"
-- >>> prop $ verifyNot checkGrepRe "grep --include=*.png foo"
-- >>> prop $ verifyNot checkGrepRe "grep -F 'Foo*' file"
-- >>> prop $ verifyNot checkGrepRe "grep -- -foo bar*"
-- >>> prop $ verifyNot checkGrepRe "grep -e -foo bar*"
-- >>> prop $ verifyNot checkGrepRe "grep --regex -foo bar*"

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


-- |
-- >>> prop $ verify checkTrapQuotes "trap \"echo $num\" INT"
-- >>> prop $ verify checkTrapQuotes "trap \"echo `ls`\" INT"
-- >>> prop $ verifyNot checkTrapQuotes "trap 'echo $num' INT"
-- >>> prop $ verify checkTrapQuotes "trap \"echo $((1+num))\" EXIT DEBUG"
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


-- |
-- >>> prop $ verifyNot checkReturn "return"
-- >>> prop $ verifyNot checkReturn "return 1"
-- >>> prop $ verifyNot checkReturn "return $var"
-- >>> prop $ verifyNot checkReturn "return $((a|b))"
-- >>> prop $ verify checkReturn "return -1"
-- >>> prop $ verify checkReturn "return 1000"
-- >>> prop $ verify checkReturn "return 'hello world'"
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


-- |
-- >>> prop $ verify checkFindExecWithSingleArgument "find . -exec 'cat {} | wc -l' \\;"
-- >>> prop $ verify checkFindExecWithSingleArgument "find . -execdir 'cat {} | wc -l' +"
-- >>> prop $ verifyNot checkFindExecWithSingleArgument "find . -exec wc -l {} \\;"
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


-- |
-- >>> prop $ verify checkUnusedEchoEscapes "echo 'foo\\nbar\\n'"
-- >>> prop $ verifyNot checkUnusedEchoEscapes "echo -e 'foi\\nbar'"
-- >>> prop $ verify checkUnusedEchoEscapes "echo \"n:\\t42\""
-- >>> prop $ verifyNot checkUnusedEchoEscapes "echo lol"
-- >>> prop $ verifyNot checkUnusedEchoEscapes "echo -n -e '\n'"
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


-- |
-- >>> prop $ verify checkInjectableFindSh "find . -exec sh -c 'echo {}' \\;"
-- >>> prop $ verify checkInjectableFindSh "find . -execdir bash -c 'rm \"{}\"' ';'"
-- >>> prop $ verifyNot checkInjectableFindSh "find . -exec sh -c 'rm \"$@\"' _ {} \\;"
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


-- |
-- >>> prop $ verify checkFindActionPrecedence "find . -name '*.wav' -o -name '*.au' -exec rm {} +"
-- >>> prop $ verifyNot checkFindActionPrecedence "find . -name '*.wav' -o \\( -name '*.au' -exec rm {} + \\)"
-- >>> prop $ verifyNot checkFindActionPrecedence "find . -name '*.wav' -o -name '*.au'"
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


-- |
-- >>> prop $ verify checkMkdirDashPM "mkdir -p -m 0755 a/b"
-- >>> prop $ verify checkMkdirDashPM "mkdir -pm 0755 $dir"
-- >>> prop $ verify checkMkdirDashPM "mkdir -vpm 0755 a/b"
-- >>> prop $ verify checkMkdirDashPM "mkdir -pm 0755 -v a/b"
-- >>> prop $ verify checkMkdirDashPM "mkdir --parents --mode=0755 a/b"
-- >>> prop $ verify checkMkdirDashPM "mkdir --parents --mode 0755 a/b"
-- >>> prop $ verify checkMkdirDashPM "mkdir -p --mode=0755 a/b"
-- >>> prop $ verify checkMkdirDashPM "mkdir --parents -m 0755 a/b"
-- >>> prop $ verifyNot checkMkdirDashPM "mkdir -p a/b"
-- >>> prop $ verifyNot checkMkdirDashPM "mkdir -m 0755 a/b"
-- >>> prop $ verifyNot checkMkdirDashPM "mkdir a/b"
-- >>> prop $ verifyNot checkMkdirDashPM "mkdir --parents a/b"
-- >>> prop $ verifyNot checkMkdirDashPM "mkdir --mode=0755 a/b"
-- >>> prop $ verifyNot checkMkdirDashPM "mkdir_func -pm 0755 a/b"
-- >>> prop $ verifyNot checkMkdirDashPM "mkdir -p -m 0755 singlelevel"
-- >>> prop $ verifyNot checkMkdirDashPM "mkdir -p -m 0755 ../bin"
-- >>> prop $ verify checkMkdirDashPM "mkdir -p -m 0755 ../bin/laden"
-- >>> prop $ verifyNot checkMkdirDashPM "mkdir -p -m 0755 ./bin"
-- >>> prop $ verify checkMkdirDashPM "mkdir -p -m 0755 ./bin/laden"
-- >>> prop $ verifyNot checkMkdirDashPM "mkdir -p -m 0755 ./../bin"
-- >>> prop $ verifyNot checkMkdirDashPM "mkdir -p -m 0755 .././bin"
-- >>> prop $ verifyNot checkMkdirDashPM "mkdir -p -m 0755 ../../bin"
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


-- |
-- >>> prop $ verify checkNonportableSignals "trap f 8"
-- >>> prop $ verifyNot checkNonportableSignals "trap f 0"
-- >>> prop $ verifyNot checkNonportableSignals "trap f 14"
-- >>> prop $ verify checkNonportableSignals "trap f SIGKILL"
-- >>> prop $ verify checkNonportableSignals "trap f 9"
-- >>> prop $ verify checkNonportableSignals "trap f stop"
-- >>> prop $ verifyNot checkNonportableSignals "trap 'stop' int"
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


-- |
-- >>> prop $ verify checkInteractiveSu "su; rm file; su $USER"
-- >>> prop $ verify checkInteractiveSu "su foo; something; exit"
-- >>> prop $ verifyNot checkInteractiveSu "echo rm | su foo"
-- >>> prop $ verifyNot checkInteractiveSu "su root < script"
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


-- |
-- This is hard to get right without properly parsing ssh args
--
-- >>> prop $ verify checkSshCommandString "ssh host \"echo $PS1\""
-- >>> prop $ verifyNot checkSshCommandString "ssh host \"ls foo\""
-- >>> prop $ verifyNot checkSshCommandString "ssh \"$host\""
-- >>> prop $ verifyNot checkSshCommandString "ssh -i key \"$host\""
checkSshCommandString = CommandCheck (Basename "ssh") (f . arguments)
  where
    isOption x = "-" `isPrefixOf` (concat $ oversimplify x)
    f args =
        case partition isOption args of
            ([], hostport:r@(_:_)) -> checkArg $ last r
            _ -> return ()
    checkArg (T_NormalWord _ [T_DoubleQuoted id parts]) =
        case filter (not . isConstant) parts of
            [] -> return ()
            (x:_) -> info (getId x) 2029
                "Note that, unescaped, this expands on the client side."
    checkArg _ = return ()


-- |
-- >>> prop $ verify checkPrintfVar "printf \"Lol: $s\""
-- >>> prop $ verifyNot checkPrintfVar "printf 'Lol: $s'"
-- >>> prop $ verify checkPrintfVar "printf -v cow $(cmd)"
-- >>> prop $ verifyNot checkPrintfVar "printf \"%${count}s\" var"
-- >>> prop $ verify checkPrintfVar "printf '%s %s %s' foo bar"
-- >>> prop $ verify checkPrintfVar "printf foo bar baz"
-- >>> prop $ verify checkPrintfVar "printf -- foo bar baz"
-- >>> prop $ verifyNot checkPrintfVar "printf '%s %s %s' \"${var[@]}\""
-- >>> prop $ verifyNot checkPrintfVar "printf '%s %s %s\\n' *.png"
-- >>> prop $ verifyNot checkPrintfVar "printf '%s %s %s' foo bar baz"
-- >>> prop $ verifyNot checkPrintfVar "printf '%(%s%s)T' -1"
-- >>> prop $ verify checkPrintfVar "printf '%s %s\\n' 1 2 3"
-- >>> prop $ verifyNot checkPrintfVar "printf '%s %s\\n' 1 2 3 4"
-- >>> prop $ verify checkPrintfVar "printf '%*s\\n' 1"
-- >>> prop $ verifyNot checkPrintfVar "printf '%*s\\n' 1 2"
-- >>> prop $ verifyNot checkPrintfVar "printf $'string'"
-- >>> prop $ verify checkPrintfVar "printf '%-*s\\n' 1"
-- >>> prop $ verifyNot checkPrintfVar "printf '%-*s\\n' 1 2"
checkPrintfVar = CommandCheck (Exactly "printf") (f . arguments) where
    f (doubledash:rest) | getLiteralString doubledash == Just "--" = f rest
    f (dashv:var:rest) | getLiteralString dashv == Just "-v" = f rest
    f (format:params) = check format params
    f _ = return ()

    countFormats string =
        case string of
            '%':'%':rest -> countFormats rest
            '%':'(':rest -> 1 + countFormats (dropWhile (/= ')') rest)
            '%':rest -> regexBasedCountFormats rest + countFormats (dropWhile (/= '%') rest)
            _:rest -> countFormats rest
            [] -> 0

    regexBasedCountFormats rest =
        maybe 1 (foldl (\acc group -> acc + (if group == "*" then 1 else 0)) 1) (matchRegex re rest)
      where
        -- constructed based on specifications in "man printf"
        re = mkRegex "#?-?\\+? ?0?(\\*|\\d*).?(\\d*|\\*)[diouxXfFeEgGaAcsb]"
        --            \____ _____/\___ ____/ \____ ____/\________ ________/
        --                 V          V           V               V
        --               flags    field width  precision   format character
        -- field width and precision can be specified with a '*' instead of a digit,
        -- in which case printf will accept one more argument for each '*' used
    check format more = do
        fromMaybe (return ()) $ do
            string <- getLiteralString format
            let vars = countFormats string

            return $ do
                when (vars == 0 && more /= []) $
                    err (getId format) 2182
                        "This printf format string has no variables. Other arguments are ignored."

                when (vars > 0
                        && ((length more) `mod` vars /= 0 || null more)
                        && all (not . mayBecomeMultipleArgs) more) $
                    warn (getId format) 2183 $
                        "This format string has " ++ show vars ++ " variables, but is passed " ++ show (length more) ++ " arguments."


        unless ('%' `elem` concat (oversimplify format) || isLiteral format) $
          info (getId format) 2059
              "Don't use variables in the printf format string. Use printf \"..%s..\" \"$foo\"."




-- |
-- >>> prop $ verify checkUuoeCmd "echo $(date)"
-- >>> prop $ verify checkUuoeCmd "echo `date`"
-- >>> prop $ verify checkUuoeCmd "echo \"$(date)\""
-- >>> prop $ verify checkUuoeCmd "echo \"`date`\""
-- >>> prop $ verifyNot checkUuoeCmd "echo \"The time is $(date)\""
-- >>> prop $ verifyNot checkUuoeCmd "echo \"$(<file)\""
checkUuoeCmd = CommandCheck (Exactly "echo") (f . arguments) where
    msg id = style id 2005 "Useless echo? Instead of 'echo $(cmd)', just use 'cmd'."
    f [token] = when (tokenIsJustCommandOutput token) $ msg (getId token)
    f _ = return ()


-- |
-- >>> prop $ verify checkSetAssignment "set foo 42"
-- >>> prop $ verify checkSetAssignment "set foo = 42"
-- >>> prop $ verify checkSetAssignment "set foo=42"
-- >>> prop $ verifyNot checkSetAssignment "set -- if=/dev/null"
-- >>> prop $ verifyNot checkSetAssignment "set 'a=5'"
-- >>> prop $ verifyNot checkSetAssignment "set"
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


-- |
-- >>> prop $ verify checkExportedExpansions "export $foo"
-- >>> prop $ verify checkExportedExpansions "export \"$foo\""
-- >>> prop $ verifyNot checkExportedExpansions "export foo"
-- >>> prop $ verifyNot checkExportedExpansions "export ${foo?}"
checkExportedExpansions = CommandCheck (Exactly "export") (mapM_ check . arguments)
  where
    check t = potentially $ do
        var <- getSingleUnmodifiedVariable t
        let name = bracedString var
        return . warn (getId t) 2163 $
            "This does not export '" ++ name ++ "'. Remove $/${} for that, or use ${var?} to quiet."

-- |
-- >>> prop $ verify checkReadExpansions "read $var"
-- >>> prop $ verify checkReadExpansions "read -r $var"
-- >>> prop $ verifyNot checkReadExpansions "read -p $var"
-- >>> prop $ verifyNot checkReadExpansions "read -rd $delim name"
-- >>> prop $ verify checkReadExpansions "read \"$var\""
-- >>> prop $ verify checkReadExpansions "read -a $var"
-- >>> prop $ verifyNot checkReadExpansions "read $1"
-- >>> prop $ verifyNot checkReadExpansions "read ${var?}"
checkReadExpansions = CommandCheck (Exactly "read") check
  where
    options = getGnuOpts "sreu:n:N:i:p:a:"
    getVars cmd = fromMaybe [] $ do
        opts <- options cmd
        return . map snd $ filter (\(x,_) -> x == "" || x == "a") opts

    check cmd = mapM_ warning $ getVars cmd
    warning t = potentially $ do
        var <- getSingleUnmodifiedVariable t
        let name = bracedString var
        guard $ isVariableName name   -- e.g. not $1
        return . warn (getId t) 2229 $
            "This does not read '" ++ name ++ "'. Remove $/${} for that, or use ${var?} to quiet."

-- Return the single variable expansion that makes up this word, if any.
-- e.g. $foo -> $foo, "$foo"'' -> $foo , "hello $name" -> Nothing
getSingleUnmodifiedVariable :: Token -> Maybe Token
getSingleUnmodifiedVariable word =
    case getWordParts word of
        [t@(T_DollarBraced {})] ->
            let contents = bracedString t
                name = getBracedReference contents
            in guard (contents == name) >> return t
        _ -> Nothing

-- |
-- >>> prop $ verify checkAliasesUsesArgs "alias a='cp $1 /a'"
-- >>> prop $ verifyNot checkAliasesUsesArgs "alias $1='foo'"
-- >>> prop $ verify checkAliasesUsesArgs "alias a=\"echo \\${@}\""
checkAliasesUsesArgs = CommandCheck (Exactly "alias") (f . arguments)
  where
    re = mkRegex "\\$\\{?[0-9*@]"
    f = mapM_ checkArg
    checkArg arg =
        let string = fromJust $ getLiteralStringExt (const $ return "_") arg in
            when ('=' `elem` string && string `matches` re) $
                err (getId arg) 2142
                    "Aliases can't use positional parameters. Use a function."


-- |
-- >>> prop $ verify checkAliasesExpandEarly "alias foo=\"echo $PWD\""
-- >>> prop $ verifyNot checkAliasesExpandEarly "alias -p"
-- >>> prop $ verifyNot checkAliasesExpandEarly "alias foo='echo {1..10}'"
checkAliasesExpandEarly = CommandCheck (Exactly "alias") (f . arguments)
  where
    f = mapM_ checkArg
    checkArg arg | '=' `elem` concat (oversimplify arg) =
        forM_ (take 1 $ filter (not . isLiteral) $ getWordParts arg) $
            \x -> warn (getId x) 2139 "This expands when defined, not when used. Consider escaping."
    checkArg _ = return ()


-- >>> prop $ verify checkUnsetGlobs "unset foo[1]"
-- >>> prop $ verifyNot checkUnsetGlobs "unset foo"
checkUnsetGlobs = CommandCheck (Exactly "unset") (mapM_ check . arguments)
  where
    check arg =
        when (isGlob arg) $
            warn (getId arg) 2184 "Quote arguments to unset so they're not glob expanded."


-- |
-- >>> prop $ verify checkFindWithoutPath "find -type f"
-- >>> prop $ verify checkFindWithoutPath "find"
-- >>> prop $ verifyNot checkFindWithoutPath "find . -type f"
-- >>> prop $ verifyNot checkFindWithoutPath "find -H -L \"$path\" -print"
-- >>> prop $ verifyNot checkFindWithoutPath "find -O3 ."
-- >>> prop $ verifyNot checkFindWithoutPath "find -D exec ."
-- >>> prop $ verifyNot checkFindWithoutPath "find --help"
-- >>> prop $ verifyNot checkFindWithoutPath "find -Hx . -print"
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
        let flag = fromJust $ getLiteralStringExt (const $ return "___") first in
            not ("-" `isPrefixOf` flag) || isLeadingFlag flag && hasPath rest
    hasPath [] = False
    isLeadingFlag flag = length flag <= 2 || all (`elem` leadingFlagChars) flag
    leadingFlagChars="-EHLPXdfsxO0123456789"


-- |
-- >>> prop $ verify checkTimeParameters "time -f lol sleep 10"
-- >>> prop $ verifyNot checkTimeParameters "time sleep 10"
-- >>> prop $ verifyNot checkTimeParameters "time -p foo"
-- >>> prop $ verifyNot checkTimeParameters "command time -f lol sleep 10"
checkTimeParameters = CommandCheck (Exactly "time") f
  where
    f (T_SimpleCommand _ _ (cmd:args:_)) =
        whenShell [Bash, Sh] $
            let s = concat $ oversimplify args in
                when ("-" `isPrefixOf` s && s /= "-p") $
                    info (getId cmd) 2023 "The shell may override 'time' as seen in man time(1). Use 'command time ..' for that one."

    f _ = return ()

-- |
-- >>> prop $ verify checkTimedCommand "#!/bin/sh\ntime -p foo | bar"
-- >>> prop $ verify checkTimedCommand "#!/bin/dash\ntime ( foo; bar; )"
-- >>> prop $ verifyNot checkTimedCommand "#!/bin/sh\ntime sleep 1"
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

-- |
-- >>> prop $ verify checkLocalScope "local foo=3"
-- >>> prop $ verifyNot checkLocalScope "f() { local foo=3; }"
checkLocalScope = CommandCheck (Exactly "local") $ \t ->
    whenShell [Bash, Dash] $ do -- Ksh allows it, Sh doesn't support local
        path <- getPathM t
        unless (any isFunction path) $
            err (getId $ getCommandTokenOrThis t) 2168 "'local' is only valid in functions."

-- |
-- >>> prop $ verify checkDeprecatedTempfile "var=$(tempfile)"
-- >>> prop $ verifyNot checkDeprecatedTempfile "tempfile=$(mktemp)"
checkDeprecatedTempfile = CommandCheck (Basename "tempfile") $
    \t -> warn (getId $ getCommandTokenOrThis t) 2186 "tempfile is deprecated. Use mktemp instead."

-- |
-- >>> prop $ verify checkDeprecatedEgrep "egrep '.+'"
checkDeprecatedEgrep = CommandCheck (Basename "egrep") $
    \t -> info (getId $ getCommandTokenOrThis t) 2196 "egrep is non-standard and deprecated. Use grep -E instead."

-- |
-- >>> prop $ verify checkDeprecatedFgrep "fgrep '*' files"
checkDeprecatedFgrep = CommandCheck (Basename "fgrep") $
    \t -> info (getId $ getCommandTokenOrThis t) 2197 "fgrep is non-standard and deprecated. Use grep -F instead."

-- |
-- >>> prop $ verify checkWhileGetoptsCase "while getopts 'a:b' x; do case $x in a) foo;; esac; done"
-- >>> prop $ verify checkWhileGetoptsCase "while getopts 'a:' x; do case $x in a) foo;; b) bar;; esac; done"
-- >>> prop $ verifyNot checkWhileGetoptsCase "while getopts 'a:b' x; do case $x in a) foo;; b) bar;; *) :;esac; done"
-- >>> prop $ verifyNot checkWhileGetoptsCase "while getopts 'a:123' x; do case $x in a) foo;; [0-9]) bar;; esac; done"
-- >>> prop $ verifyNot checkWhileGetoptsCase "while getopts 'a:' x; do case $x in a) foo;; \\?) bar;; *) baz;; esac; done"
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

-- |
-- >>> prop $ verify checkCatastrophicRm "rm -r $1/$2"
-- >>> prop $ verify checkCatastrophicRm "rm -r /home/$foo"
-- >>> prop $ verifyNot checkCatastrophicRm "rm -r /home/${USER:?}/*"
-- >>> prop $ verify checkCatastrophicRm "rm -fr /home/$(whoami)/*"
-- >>> prop $ verifyNot checkCatastrophicRm "rm -r /home/${USER:-thing}/*"
-- >>> prop $ verify checkCatastrophicRm "rm --recursive /etc/*$config*"
-- >>> prop $ verify checkCatastrophicRm "rm -rf /home"
-- >>> prop $ verifyNot checkCatastrophicRm "rm -r \"${DIR}\"/{.gitignore,.gitattributes,ci}"
-- >>> prop $ verify checkCatastrophicRm "rm -r /{bin,sbin}/$exec"
-- >>> prop $ verify checkCatastrophicRm "rm -r /{{usr,},{bin,sbin}}/$exec"
-- >>> prop $ verifyNot checkCatastrophicRm "rm -r /{{a,b},{c,d}}/$exec"
-- >>> prop $ verify checkCatastrophicRm "rm -rf /usr /lib/nvidia-current/xorg/xorg"
-- >>> prop $ verify checkCatastrophicRm "rm -rf \"$STEAMROOT/\"*"
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


-- |
-- >>> prop $ verify checkLetUsage "let a=1"
-- >>> prop $ verifyNot checkLetUsage "(( a=1 ))"
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

-- |
-- >>> prop $ verify    checkMvArguments "mv 'foo bar'"
-- >>> prop $ verifyNot checkMvArguments "mv foo bar"
-- >>> prop $ verifyNot checkMvArguments "mv 'foo bar'{,bak}"
-- >>> prop $ verifyNot checkMvArguments "mv \"$@\""
-- >>> prop $ verifyNot checkMvArguments "mv -t foo bar"
-- >>> prop $ verifyNot checkMvArguments "mv --target-directory=foo bar"
-- >>> prop $ verifyNot checkMvArguments "mv --target-direc=foo bar"
-- >>> prop $ verifyNot checkMvArguments "mv --version"
-- >>> prop $ verifyNot checkMvArguments "mv \"${!var}\""
checkMvArguments = CommandCheck (Basename "mv") $ missingDestination f
  where
    f t = err (getId t) 2224 "This mv has no destination. Check the arguments."

checkCpArguments = CommandCheck (Basename "cp") $ missingDestination f
  where
    f t = err (getId t) 2225 "This cp has no destination. Check the arguments."

checkLnArguments = CommandCheck (Basename "ln") $ missingDestination f
  where
    f t = warn (getId t) 2226 "This ln has no destination. Check the arguments, or specify '.' explicitly."


-- |
-- >>> prop $ verify    checkFindRedirections "find . -exec echo {} > file \\;"
-- >>> prop $ verifyNot checkFindRedirections "find . -exec echo {} \\; > file"
-- >>> prop $ verifyNot checkFindRedirections "find . -execdir sh -c 'foo > file' \\;"
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

-- >>> prop $ verify checkWhich "which '.+'"
checkWhich = CommandCheck (Basename "which") $
    \t -> info (getId $ getCommandTokenOrThis t) 2230 "which is non-standard. Use builtin 'command -v' instead."

-- |
-- >>> prop $ verify checkSudoRedirect "sudo echo 3 > /proc/file"
-- >>> prop $ verify checkSudoRedirect "sudo cmd < input"
-- >>> prop $ verify checkSudoRedirect "sudo cmd >> file"
-- >>> prop $ verify checkSudoRedirect "sudo cmd &> file"
-- >>> prop $ verifyNot checkSudoRedirect "sudo cmd 2>&1"
-- >>> prop $ verifyNot checkSudoRedirect "sudo cmd 2> log"
-- >>> prop $ verifyNot checkSudoRedirect "sudo cmd > /dev/null 2>&1"
checkSudoRedirect = CommandCheck (Basename "sudo") f
  where
    f t = do
        t_redir <- getClosestCommandM t
        case t_redir of
            Just (T_Redirecting _ redirs _) ->
                mapM_ warnAbout redirs
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

-- |
-- >>> prop $ verify checkSudoArgs "sudo cd /root"
-- >>> prop $ verify checkSudoArgs "sudo export x=3"
-- >>> prop $ verifyNot checkSudoArgs "sudo ls /usr/local/protected"
-- >>> prop $ verifyNot checkSudoArgs "sudo ls && export x=3"
-- >>> prop $ verifyNot checkSudoArgs "sudo echo ls"
-- >>> prop $ verifyNot checkSudoArgs "sudo -n -u export ls"
-- >>> prop $ verifyNot checkSudoArgs "sudo docker export foo"
checkSudoArgs = CommandCheck (Basename "sudo") f
  where
    f t = potentially $ do
        opts <- parseOpts t
        let nonFlags = map snd $ filter (\(flag, _) -> flag == "") opts
        commandArg <- nonFlags !!! 0
        command <- getLiteralString commandArg
        guard $ command `elem` builtins
        return $ warn (getId t) 2232 $ "Can't use sudo with builtins like " ++ command ++ ". Did you want sudo sh -c .. instead?"
    builtins = [ "cd", "eval", "export", "history", "read", "source", "wait" ]
    -- This mess is why ShellCheck prefers not to know.
    parseOpts = getBsdOpts "vAknSbEHPa:g:h:p:u:c:T:r:"
