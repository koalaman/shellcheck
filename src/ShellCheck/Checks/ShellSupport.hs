{-
    Copyright 2012-2016 Vidar Holen

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
module ShellCheck.Checks.ShellSupport (checker) where

import ShellCheck.AST
import ShellCheck.ASTLib
import ShellCheck.AnalyzerLib
import ShellCheck.Interface
import ShellCheck.Regex

import Control.Monad
import Control.Monad.RWS
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as Map

data ForShell = ForShell [Shell] (Token -> Analysis)

getChecker params list = Checker {
        perScript = nullCheck,
        perToken = foldl composeAnalyzers nullCheck $ mapMaybe include list
    }
  where
    shell = shellType params
    include (ForShell list a) = do
        guard $ shell `elem` list
        return a

checker params = getChecker params checks

checks = [
    checkForDecimals
    ,checkBashisms
    ,checkEchoSed
    ,checkBraceExpansionVars
    ,checkMultiDimensionalArrays
    ,checkPS1Assignments
    ]

testChecker (ForShell _ t) =
    Checker {
        perScript = nullCheck,
        perToken = t
    }
verify c s = producesComments (testChecker c) s == Just True
verifyNot c s = producesComments (testChecker c) s == Just False

-- |
-- >>> prop $ verify checkForDecimals "((3.14*c))"
-- >>> prop $ verify checkForDecimals "foo[1.2]=bar"
-- >>> prop $ verifyNot checkForDecimals "declare -A foo; foo[1.2]=bar"
checkForDecimals = ForShell [Sh, Dash, Bash] f
  where
    f t@(TA_Expansion id _) = potentially $ do
        str <- getLiteralString t
        first <- str !!! 0
        guard $ isDigit first && '.' `elem` str
        return $ err id 2079 "(( )) doesn't support decimals. Use bc or awk."
    f _ = return ()


-- |
-- >>> prop $ verify checkBashisms "while read a; do :; done < <(a)"
-- >>> prop $ verify checkBashisms "[ foo -nt bar ]"
-- >>> prop $ verify checkBashisms "echo $((i++))"
-- >>> prop $ verify checkBashisms "rm !(*.hs)"
-- >>> prop $ verify checkBashisms "source file"
-- >>> prop $ verify checkBashisms "[ \"$a\" == 42 ]"
-- >>> prop $ verify checkBashisms "echo ${var[1]}"
-- >>> prop $ verify checkBashisms "echo ${!var[@]}"
-- >>> prop $ verify checkBashisms "echo ${!var*}"
-- >>> prop $ verify checkBashisms "echo ${var:4:12}"
-- >>> prop $ verifyNot checkBashisms "echo ${var:-4}"
-- >>> prop $ verify checkBashisms "echo ${var//foo/bar}"
-- >>> prop $ verify checkBashisms "exec -c env"
-- >>> prop $ verify checkBashisms "echo -n \"Foo: \""
-- >>> prop $ verify checkBashisms "let n++"
-- >>> prop $ verify checkBashisms "echo $RANDOM"
-- >>> prop $ verify checkBashisms "echo $((RANDOM%6+1))"
-- >>> prop $ verify checkBashisms "foo &> /dev/null"
-- >>> prop $ verify checkBashisms "foo > file*.txt"
-- >>> prop $ verify checkBashisms "read -ra foo"
-- >>> prop $ verify checkBashisms "[ -a foo ]"
-- >>> prop $ verifyNot checkBashisms "[ foo -a bar ]"
-- >>> prop $ verify checkBashisms "trap mything ERR INT"
-- >>> prop $ verifyNot checkBashisms "trap mything INT TERM"
-- >>> prop $ verify checkBashisms "cat < /dev/tcp/host/123"
-- >>> prop $ verify checkBashisms "trap mything ERR SIGTERM"
-- >>> prop $ verify checkBashisms "echo *[^0-9]*"
-- >>> prop $ verify checkBashisms "exec {n}>&2"
-- >>> prop $ verify checkBashisms "echo ${!var}"
-- >>> prop $ verify checkBashisms "printf -v '%s' \"$1\""
-- >>> prop $ verify checkBashisms "printf '%q' \"$1\""
-- >>> prop $ verifyNot checkBashisms "#!/bin/dash\n[ foo -nt bar ]"
-- >>> prop $ verify checkBashisms "#!/bin/sh\necho -n foo"
-- >>> prop $ verifyNot checkBashisms "#!/bin/dash\necho -n foo"
-- >>> prop $ verifyNot checkBashisms "#!/bin/dash\nlocal foo"
-- >>> prop $ verifyNot checkBashisms "#!/bin/dash\nread -p foo -r bar"
-- >>> prop $ verifyNot checkBashisms "HOSTNAME=foo; echo $HOSTNAME"
-- >>> prop $ verify checkBashisms "RANDOM=9; echo $RANDOM"
-- >>> prop $ verify checkBashisms "foo-bar() { true; }"
-- >>> prop $ verify checkBashisms "echo $(<file)"
-- >>> prop $ verify checkBashisms "echo `<file`"
-- >>> prop $ verify checkBashisms "trap foo int"
-- >>> prop $ verify checkBashisms "trap foo sigint"
-- >>> prop $ verifyNot checkBashisms "#!/bin/dash\ntrap foo int"
-- >>> prop $ verifyNot checkBashisms "#!/bin/dash\ntrap foo INT"
-- >>> prop $ verify checkBashisms "#!/bin/dash\ntrap foo SIGINT"
-- >>> prop $ verify checkBashisms "#!/bin/dash\necho foo 42>/dev/null"
-- >>> prop $ verifyNot checkBashisms "#!/bin/sh\necho $LINENO"
-- >>> prop $ verify checkBashisms "#!/bin/dash\necho $MACHTYPE"
-- >>> prop $ verify checkBashisms "#!/bin/sh\ncmd >& file"
-- >>> prop $ verifyNot checkBashisms "#!/bin/sh\ncmd 2>&1"
-- >>> prop $ verifyNot checkBashisms "#!/bin/sh\ncmd >&2"
-- >>> prop $ verifyNot checkBashisms "#!/bin/sh\nprintf -- -f\n"
-- >>> prop $ verify checkBashisms "#!/bin/sh\nfoo+=bar"
-- >>> prop $ verify checkBashisms "#!/bin/sh\necho ${@%foo}"
-- >>> prop $ verifyNot checkBashisms "#!/bin/sh\necho ${##}"
checkBashisms = ForShell [Sh, Dash] $ \t -> do
    params <- ask
    kludge params t
 where
  -- This code was copy-pasted from Analytics where params was a variable
  kludge params = bashism
   where
    isDash = shellType params == Dash
    warnMsg id s =
        if isDash
        then warn id 2169 $ "In dash, " ++ s ++ " not supported."
        else warn id 2039 $ "In POSIX sh, " ++ s ++ " undefined."

    bashism (T_ProcSub id _ _) = warnMsg id "process substitution is"
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
        | op `elem` [ "<", ">", "\\<", "\\>", "<=", ">=", "\\<=", "\\>="] =
            unless isDash $ warnMsg id $ "lexicographical " ++ op ++ " is"
    bashism (TC_Binary id SingleBracket op _ _)
        | op `elem` [ "-nt", "-ef" ] =
            unless isDash $ warnMsg id $ op ++ " is"
    bashism (TC_Binary id SingleBracket "==" _ _) =
            warnMsg id "== in place of = is"
    bashism (TC_Binary id SingleBracket "=~" _ _) =
            warnMsg id "=~ regex matching is"
    bashism (TC_Unary id _ "-a" _) =
            warnMsg id "unary -a in place of -e is"
    bashism (TA_Unary id op _)
        | op `elem` [ "|++", "|--", "++|", "--|"] =
            warnMsg id $ filter (/= '|') op ++ " is"
    bashism (TA_Binary id "**" _ _) = warnMsg id "exponentials are"
    bashism (T_FdRedirect id "&" (T_IoFile _ (T_Greater _) _)) = warnMsg id "&> is"
    bashism (T_FdRedirect id "" (T_IoFile _ (T_GREATAND _) _)) = warnMsg id ">& is"
    bashism (T_FdRedirect id ('{':_) _) = warnMsg id "named file descriptors are"
    bashism (T_FdRedirect id num _)
        | all isDigit num && length num > 1 = warnMsg id "FDs outside 0-9 are"
    bashism (T_Assignment id Append _ _ _) =
        warnMsg id "+= is"
    bashism (T_IoFile id _ word) | isNetworked =
            warnMsg id "/dev/{tcp,udp} is"
        where
            file = onlyLiteralString word
            isNetworked = any (`isPrefixOf` file) ["/dev/tcp", "/dev/udp"]
    bashism (T_Glob id str) | "[^" `isInfixOf` str =
            warnMsg id "^ in place of ! in glob bracket expressions is"

    bashism t@(TA_Variable id str _) | isBashVariable str =
        warnMsg id $ str ++ " is"

    bashism t@(T_DollarBraced id token) = do
        mapM_ check expansion
        when (isBashVariable var) $
                    warnMsg id $ var ++ " is"
      where
        str = bracedString t
        var = getBracedReference str
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

    bashism (T_Function id _ _ str _) | not (isVariableName str) =
        warnMsg id "naming functions outside [a-zA-Z_][a-zA-Z0-9_]* is"

    bashism (T_DollarExpansion id [x]) | isOnlyRedirection x =
        warnMsg id "$(<file) to read files is"
    bashism (T_Backticked id [x]) | isOnlyRedirection x =
        warnMsg id "`<file` to read files is"

    bashism t@(T_SimpleCommand _ _ (cmd:arg:_))
        | t `isCommand` "echo" && "-" `isPrefixOf` argString =
            unless ("--" `isPrefixOf` argString) $ -- echo "-----"
                if isDash
                then
                    when (argString /= "-n") $
                        warnMsg (getId arg) "echo flags besides -n"
                else
                    warnMsg (getId arg) "echo flags are"
      where argString = concat $ oversimplify arg
    bashism t@(T_SimpleCommand _ _ (cmd:arg:_))
        | t `isCommand` "exec" && "-" `isPrefixOf` concat (oversimplify arg) =
            warnMsg (getId arg) "exec flags are"
    bashism t@(T_SimpleCommand id _ _)
        | t `isCommand` "let" = warnMsg id "'let' is"

    bashism t@(T_SimpleCommand id _ (cmd:rest)) =
        let name = fromMaybe "" $ getCommandName t
            flags = getLeadingFlags t
        in do
            when (name `elem` unsupportedCommands) $
                warnMsg id $ "'" ++ name ++ "' is"
            potentially $ do
                allowed <- Map.lookup name allowedFlags
                (word, flag) <- listToMaybe $
                    filter (\x -> (not . null . snd $ x) && snd x `notElem` allowed) flags
                return . warnMsg (getId word) $ name ++ " -" ++ flag ++ " is"

            when (name == "source") $ warnMsg id "'source' in place of '.' is"
            when (name == "trap") $
                let
                    check token = potentially $ do
                        str <- getLiteralString token
                        let upper = map toUpper str
                        return $ do
                            when (upper `elem` ["ERR", "DEBUG", "RETURN"]) $
                                warnMsg (getId token) $ "trapping " ++ str ++ " is"
                            when ("SIG" `isPrefixOf` upper) $
                                warnMsg (getId token)
                                    "prefixing signal names with 'SIG' is"
                            when (not isDash && upper /= str) $
                                warnMsg (getId token)
                                    "using lower/mixed case for signal names is"
                in
                    mapM_ check (drop 1 rest)

            when (name == "printf") $ potentially $ do
                format <- rest !!! 0  -- flags are covered by allowedFlags
                let literal = onlyLiteralString format
                guard $ "%q" `isInfixOf` literal
                return $ warnMsg (getId format) "printf %q is"
      where
        unsupportedCommands = [
            "let", "caller", "builtin", "complete", "compgen", "declare", "dirs", "disown",
            "enable", "mapfile", "readarray", "pushd", "popd", "shopt", "suspend",
            "typeset"
            ] ++ if not isDash then ["local"] else []
        allowedFlags = Map.fromList [
            ("exec", []),
            ("export", ["-p"]),
            ("printf", []),
            ("read", if isDash then ["r", "p"] else ["r"]),
            ("ulimit", ["f"])
            ]
    bashism t@(T_SourceCommand id src _) =
        let name = fromMaybe "" $ getCommandName src
        in do
            when (name == "source") $ warnMsg id "'source' in place of '.' is"
    bashism _ = return ()

    varChars="_0-9a-zA-Z"
    expansion = let re = mkRegex in [
        (re $ "^![" ++ varChars ++ "]", "indirect expansion is"),
        (re $ "^[" ++ varChars ++ "]+\\[.*\\]$", "array references are"),
        (re $ "^![" ++ varChars ++ "]+\\[[*@]]$", "array key expansion is"),
        (re $ "^![" ++ varChars ++ "]+[*@]$", "name matching prefixes are"),
        (re $ "^[" ++ varChars ++ "*@]+:[^-=?+]", "string indexing is"),
        (re $ "^([*@][%#]|#[@*])", "string operations on $@/$* are"),
        (re $ "^[" ++ varChars ++ "*@]+(\\[.*\\])?/", "string replacement is")
        ]
    bashVars = [
        "OSTYPE", "MACHTYPE", "HOSTTYPE", "HOSTNAME",
        "DIRSTACK", "EUID", "UID", "SHLVL", "PIPESTATUS", "SHELLOPTS"
        ]
    bashDynamicVars = [ "RANDOM", "SECONDS" ]
    dashVars = [ ]
    isBashVariable var =
        (var `elem` bashDynamicVars
            || var `elem` bashVars && not (isAssigned var))
        && not (isDash && var `elem` dashVars)
    isAssigned var = any f (variableFlow params)
      where
        f x = case x of
                Assignment (_, _, name, _) -> name == var
                _ -> False

-- |
-- >>> prop $ verify checkEchoSed "FOO=$(echo \"$cow\" | sed 's/foo/bar/g')"
-- >>> prop $ verify checkEchoSed "rm $(echo $cow | sed -e 's,foo,bar,')"
checkEchoSed = ForShell [Bash, Ksh] f
  where
    f (T_Pipeline id _ [a, b]) =
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
            let delimiters = filter (== head first) rest
            guard $ length delimiters == 2
            return True

        acmd = oversimplify a
        bcmd = oversimplify b
        checkIn s =
            when (isSimpleSed s) $
                style id 2001 "See if you can use ${variable//search/replace} instead."
    f _ = return ()


-- |
-- >>> prop $ verify checkBraceExpansionVars "echo {1..$n}"
-- >>> prop $ verifyNot checkBraceExpansionVars "echo {1,3,$n}"
-- >>> prop $ verify checkBraceExpansionVars "eval echo DSC{0001..$n}.jpg"
-- >>> prop $ verify checkBraceExpansionVars "echo {$i..100}"
checkBraceExpansionVars = ForShell [Bash] f
  where
    f t@(T_BraceExpansion id list) = mapM_ check list
      where
        check element =
            when (any (`isInfixOf` toString element) ["$..", "..$"]) $ do
                c <- isEvaled element
                if c
                    then style id 2175 "Quote this invalid brace expansion since it should be passed literally to eval."
                    else warn id 2051 "Bash doesn't support variables in brace range expansions."
    f _ = return ()

    literalExt t =
        case t of
            T_DollarBraced {} -> return "$"
            T_DollarExpansion {} -> return "$"
            T_DollarArithmetic {} -> return "$"
            otherwise -> return "-"
    toString t = fromJust $ getLiteralStringExt literalExt t
    isEvaled t = do
        cmd <- getClosestCommandM t
        return $ isJust cmd && fromJust cmd `isUnqualifiedCommand` "eval"


-- |
-- >>> prop $ verify checkMultiDimensionalArrays "foo[a][b]=3"
-- >>> prop $ verifyNot checkMultiDimensionalArrays "foo[a]=3"
-- >>> prop $ verify checkMultiDimensionalArrays "foo=( [a][b]=c )"
-- >>> prop $ verifyNot checkMultiDimensionalArrays "foo=( [a]=c )"
-- >>> prop $ verify checkMultiDimensionalArrays "echo ${foo[bar][baz]}"
-- >>> prop $ verifyNot checkMultiDimensionalArrays "echo ${foo[bar]}"
checkMultiDimensionalArrays = ForShell [Bash] f
  where
    f token =
        case token of
            T_Assignment _ _ name (first:second:_) _ -> about second
            T_IndexedElement _ (first:second:_) _ -> about second
            T_DollarBraced {} ->
                when (isMultiDim token) $ about token
            _ -> return ()
    about t = warn (getId t) 2180 "Bash does not support multidimensional arrays. Use 1D or associative arrays."

    re = mkRegex "^\\[.*\\]\\[.*\\]"  -- Fixme, this matches ${foo:- [][]} and such as well
    isMultiDim t = getBracedModifier (bracedString t) `matches` re

-- |
-- >>> prop $ verify checkPS1Assignments "PS1='\\033[1;35m\\$ '"
-- >>> prop $ verify checkPS1Assignments "export PS1='\\033[1;35m\\$ '"
-- >>> prop $ verify checkPS1Assignments "PS1='\\h \\e[0m\\$ '"
-- >>> prop $ verify checkPS1Assignments "PS1=$'\\x1b[c '"
-- >>> prop $ verify checkPS1Assignments "PS1=$'\\e[3m; '"
-- >>> prop $ verify checkPS1Assignments "export PS1=$'\\e[3m; '"
-- >>> prop $ verifyNot checkPS1Assignments "PS1='\\[\\033[1;35m\\]\\$ '"
-- >>> prop $ verifyNot checkPS1Assignments "PS1='\\[\\e1m\\e[1m\\]\\$ '"
-- >>> prop $ verifyNot checkPS1Assignments "PS1='e033x1B'"
-- >>> prop $ verifyNot checkPS1Assignments "PS1='\\[\\e\\]'"
checkPS1Assignments = ForShell [Bash] f
  where
    f token = case token of
        (T_Assignment _ _ "PS1" _ word) -> warnFor word
        _ -> return ()

    warnFor word =
        let contents = concat $ oversimplify word in
            when (containsUnescaped contents) $
                info (getId word) 2025 "Make sure all escape sequences are enclosed in \\[..\\] to prevent line wrapping issues"
    containsUnescaped s =
        let unenclosed = subRegex enclosedRegex s "" in
           isJust $ matchRegex escapeRegex unenclosed
    enclosedRegex = mkRegex "\\\\\\[.*\\\\\\]" -- FIXME: shouldn't be eager
    escapeRegex = mkRegex "\\\\x1[Bb]|\\\\e|\x1B|\\\\033"
