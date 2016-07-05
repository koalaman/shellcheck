{-
    Copyright 2012-2015 Vidar Holen

    This file is part of ShellCheck.
    http://www.vidarholen.net/contents/shellcheck

    ShellCheck is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    ShellCheck is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

-- This module contains checks that examine specific commands by name.
module ShellCheck.Checks.Commands (runChecks
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
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Test.QuickCheck.All (forAllProperties)
import Test.QuickCheck.Test (quickCheckWithResult, stdArgs, maxSuccess)

data CommandName = Exactly String | Basename String
    deriving (Eq, Ord)

data CommandCheck =
    CommandCheck CommandName (Token -> Analysis)

nullCheck :: Token -> Analysis
nullCheck _ = return ()


verify :: CommandCheck -> String -> Bool
verify f s = producesComments f s == Just True
verifyNot f s = producesComments f s == Just False

producesComments :: CommandCheck -> String -> Maybe Bool
producesComments f s = do
        root <- pScript s
        return . not . null $ runList (defaultSpec root) [f]

composeChecks f g t = do
    f t
    g t

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
    ]

buildCommandMap :: [CommandCheck] -> Map.Map CommandName (Token -> Analysis)
buildCommandMap = foldl' addCheck Map.empty
  where
    addCheck map (CommandCheck name function) =
        Map.insertWith' composeChecks name function map


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

runList spec list = notes
    where
        root = asScript spec
        params = makeParameters spec
        notes = execWriter $ runReaderT (doAnalysis (checkCommand map) root) params
        map = buildCommandMap list

runChecks spec = runList spec commandChecks


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

checkGrepRe = CommandCheck (Basename "grep") (f . arguments) where
    -- --regex=*(extglob) doesn't work. Fixme?
    skippable (Just s) = not ("--regex=" `isPrefixOf` s) && "-" `isPrefixOf` s
    skippable _ = False
    f [] = return ()
    f (x:r) | skippable (getLiteralStringExt (const $ return "_") x) = f r
    f (re:_) = do
        when (isGlob re) $
            warn (getId re) 2062 "Quote the grep pattern so the shell won't interpret it."
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
        (`elem` ["sh", "bash", "ksh"]),
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
    isAction = isParam [ "-exec", "-execdir", "-delete", "-print", "-print0" ]
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
checkMkdirDashPM = CommandCheck (Basename "mkdir") check
  where
    check t = potentially $ do
        let flags = getAllFlags t
        dashP <- find ((\f -> f == "p" || f == "parents") . snd) flags
        dashM <- find ((\f -> f == "m" || f == "mode") . snd) flags
        guard $ any couldHaveSubdirs (drop 1 $ arguments t) -- mkdir -pm 0700 dir  is fine, but dir/subdir is not.
        return $ warn (getId $ fst dashM) 2174 "When used with -p, -m only applies to the deepest directory."
    couldHaveSubdirs t = fromMaybe True $ do
        name <- getLiteralString t
        return $ '/' `elem` name


prop_checkNonportableSignals1 = verify checkNonportableSignals "trap f 8"
prop_checkNonportableSignals2 = verifyNot checkNonportableSignals "trap f 0"
prop_checkNonportableSignals3 = verifyNot checkNonportableSignals "trap f 14"
prop_checkNonportableSignals4 = verify checkNonportableSignals "trap f SIGKILL"
prop_checkNonportableSignals5 = verify checkNonportableSignals "trap f 9"
prop_checkNonportableSignals6 = verify checkNonportableSignals "trap f stop"
checkNonportableSignals = CommandCheck (Exactly "trap") (f . arguments)
  where
    f = mapM_ check
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
checkPrintfVar = CommandCheck (Exactly "printf") (f . arguments) where
    f (dashv:var:rest) | getLiteralString dashv == Just "-v" = f rest
    f (format:params) = check format
    f _ = return ()
    check format =
        unless ('%' `elem` concat (oversimplify format) || isLiteral format) $
          warn (getId format) 2059
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


return []
runTests =  $( [| $(forAllProperties) (quickCheckWithResult (stdArgs { maxSuccess = 1 }) ) |])
