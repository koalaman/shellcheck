{-
    Copyright 2012-2021 Vidar Holen

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
module ShellCheck.ASTLib where

import ShellCheck.AST
import ShellCheck.Prelude
import ShellCheck.Regex

import Control.Monad.Writer
import Control.Monad
import Data.Char
import Data.Functor
import Data.Functor.Identity
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Numeric (showHex)

import Test.QuickCheck

arguments (T_SimpleCommand _ _ (cmd:args)) = args

-- Is this a type of loop?
isLoop t = case t of
        T_WhileExpression {} -> True
        T_UntilExpression {} -> True
        T_ForIn {} -> True
        T_ForArithmetic {} -> True
        T_SelectIn {}  -> True
        _ -> False

-- Will this split into multiple words when used as an argument?
willSplit x =
  case x of
    T_DollarBraced {} -> True
    T_DollarExpansion {} -> True
    T_Backticked {} -> True
    T_BraceExpansion {} -> True
    T_Glob {} -> True
    T_Extglob {} -> True
    T_DoubleQuoted _ l -> any willBecomeMultipleArgs l
    T_NormalWord _ l -> any willSplit l
    _ -> False

isGlob t = case t of
    T_Extglob {} -> True
    T_Glob {} -> True
    T_NormalWord _ l -> any isGlob l || hasSplitRange l
    _ -> False
  where
    -- foo[x${var}y] gets parsed as foo,[,x,$var,y],
    -- so check if there's such an interval
    hasSplitRange l =
        let afterBracket = dropWhile (not . isHalfOpenRange) l
        in any isClosingRange afterBracket

    isHalfOpenRange t =
        case t of
            T_Literal _ "[" -> True
            _ -> False

    isClosingRange t =
        case t of
            T_Literal _ str -> ']' `elem` str
            _ -> False


-- Is this shell word a constant?
isConstant token =
    case token of
        -- This ignores some cases like ~"foo":
        T_NormalWord _ (T_Literal _ ('~':_) : _)  -> False
        T_NormalWord _ l   -> all isConstant l
        T_DoubleQuoted _ l -> all isConstant l
        T_SingleQuoted _ _ -> True
        T_Literal _ _ -> True
        _ -> False

-- Is this an empty literal?
isEmpty token =
    case token of
        T_NormalWord _ l   -> all isEmpty l
        T_DoubleQuoted _ l -> all isEmpty l
        T_SingleQuoted _ "" -> True
        T_Literal _ "" -> True
        _ -> False

-- Quick&lazy oversimplification of commands, throwing away details
-- and returning a list like  ["find", ".", "-name", "${VAR}*" ].
oversimplify token =
    case token of
        (T_NormalWord _ l) -> [concat (concatMap oversimplify l)]
        (T_DoubleQuoted _ l) -> [concat (concatMap oversimplify l)]
        (T_SingleQuoted _ s) -> [s]
        (T_DollarBraced _ _ _) -> ["${VAR}"]
        (T_DollarArithmetic _ _) -> ["${VAR}"]
        (T_DollarExpansion _ _) -> ["${VAR}"]
        (T_Backticked _ _) -> ["${VAR}"]
        (T_Glob _ s) -> [s]
        (T_Pipeline _ _ [x]) -> oversimplify x
        (T_Literal _ x) -> [x]
        (T_ParamSubSpecialChar _ x) -> [x]
        (T_SimpleCommand _ vars words) -> concatMap oversimplify words
        (T_Redirecting _ _ foo) -> oversimplify foo
        (T_DollarSingleQuoted _ s) -> [s]
        (T_Annotation _ _ s) -> oversimplify s
        -- Workaround for let "foo = bar" parsing
        (TA_Sequence _ [TA_Expansion _ v]) -> concatMap oversimplify v
        _ -> []


-- Turn a SimpleCommand foo -avz --bar=baz into args "a", "v", "z", "bar",
-- each in a tuple of (token, stringFlag). Non-flag arguments are added with
-- stringFlag == "".
getFlagsUntil stopCondition (T_SimpleCommand _ _ (_:args)) =
    let tokenAndText = map (\x -> (x, concat $ oversimplify x)) args
        (flagArgs, rest) = break (stopCondition . snd) tokenAndText
    in
        concatMap flag flagArgs ++ map (\(t, _) -> (t, "")) rest
  where
    flag (x, '-':'-':arg) = [ (x, takeWhile (/= '=') arg) ]
    flag (x, '-':args) = map (\v -> (x, [v])) args
    flag (x, _) = [ (x, "") ]
getFlagsUntil _ _ = error $ pleaseReport "getFlags on non-command"

-- Get all flags in a GNU way, up until --
getAllFlags :: Token -> [(Token, String)]
getAllFlags = getFlagsUntil (== "--")
-- Get all flags in a BSD way, up until first non-flag argument or --
getLeadingFlags = getFlagsUntil (\x -> x == "--" || (not $ "-" `isPrefixOf` x))

-- Check if a command has a flag.
hasFlag cmd str = str `elem` (map snd $ getAllFlags cmd)

-- Is this token a word that starts with a dash?
isFlag token =
    case getWordParts token of
        T_Literal _ ('-':_) : _ -> True
        _ -> False

-- Is this token a flag where the - is unquoted?
isUnquotedFlag token = fromMaybe False $ do
    str <- getLeadingUnquotedString token
    return $ "-" `isPrefixOf` str

-- getGnuOpts "erd:u:" will parse a list of arguments tokens like `read`
--     -re -d : -u 3 bar
-- into
--     Just [("r", (-re, -re)), ("e", (-re, -re)), ("d", (-d,:)), ("u", (-u,3)), ("", (bar,bar))]
--
-- Each string flag maps to a tuple of (flag, argument), where argument=flag if it
-- doesn't take a specific one.
--
-- Any unrecognized flag will result in Nothing. The exception is if arbitraryLongOpts
-- is set, in which case --anything will map to "anything".
getGnuOpts :: String -> [Token] -> Maybe [(String, (Token, Token))]
getGnuOpts str args = getOpts (True, False) str [] args

-- As above, except the first non-arg string will treat the rest as arguments
getBsdOpts :: String -> [Token] -> Maybe [(String, (Token, Token))]
getBsdOpts str args = getOpts (False, False) str [] args

-- Tests for this are in Commands.hs where it's more frequently used
getOpts ::
    -- Behavioral config: gnu style, allow arbitrary long options
    (Bool, Bool)
    -- A getopts style string
    -> String
    -- List of long options and whether they take arguments
    -> [(String, Bool)]
    -- List of arguments (excluding command)
    -> [Token]
    -- List of flags to tuple of (optionToken, valueToken)
    -> Maybe [(String, (Token, Token))]

getOpts (gnu, arbitraryLongOpts) string longopts args = process args
  where
    flagList (c:':':rest) = ([c], True) : flagList rest
    flagList (c:rest)     = ([c], False) : flagList rest
    flagList []           = longopts
    flagMap = Map.fromList $ ("", False) : flagList string

    process [] = return []
    process (token:rest) = do
        case getLiteralStringDef "\0" token of
            "--" -> return $ listToArgs rest
            '-':'-':word -> do
                let (name, arg) = span (/= '=') word
                needsArg <-
                    if arbitraryLongOpts
                    then return $ Map.findWithDefault False name flagMap
                    else Map.lookup name flagMap

                if needsArg && null arg
                  then
                    case rest of
                        (arg:rest2) -> do
                            more <- process rest2
                            return $ (name, (token, arg)) : more
                        _ -> fail "Missing arg"
                  else do
                    more <- process rest
                    -- Consider splitting up token to get arg
                    return $ (name, (token, token)) : more
            '-':opts -> shortToOpts opts token rest
            arg ->
                if gnu
                then do
                    more <- process rest
                    return $ ("", (token, token)):more
                else return $ listToArgs (token:rest)

    shortToOpts opts token args =
        case opts of
            c:rest -> do
                needsArg <- Map.lookup [c] flagMap
                case () of
                    _ | needsArg && null rest -> do
                        (next:restArgs) <- return args
                        more <- process restArgs
                        return $ ([c], (token, next)):more
                    _ | needsArg -> do
                        more <- process args
                        return $ ([c], (token, token)):more
                    _ -> do
                        more <- shortToOpts rest token args
                        return $ ([c], (token, token)):more
            [] -> process args

    listToArgs = map (\x -> ("", (x, x)))


-- Generic getOpts that doesn't rely on a format string, but may also be inaccurate.
-- This provides a best guess interpretation instead of failing when new options are added.
--
--    "--" is treated as end of arguments
--    "--anything[=foo]" is treated as a long option without argument
--    "-any" is treated as -a -n -y, with the next arg as an option to -y unless it starts with -
--    anything else is an argument
getGenericOpts :: [Token] -> [(String, (Token, Token))]
getGenericOpts = process
  where
    process (token:rest) =
        case getLiteralStringDef "\0" token of
            "--" -> map (\c -> ("", (c,c))) rest
            '-':'-':word -> (takeWhile (`notElem` "\0=") word, (token, token)) : process rest
            '-':optString ->
                let opts = takeWhile (/= '\0') optString
                in
                    case rest of
                        next:_ | "-" `isPrefixOf` getLiteralStringDef "\0" next  ->
                            map (\c -> ([c], (token, token))) opts ++ process rest
                        next:remainder ->
                            case reverse opts of
                                last:initial ->
                                    map (\c -> ([c], (token, token))) (reverse initial)
                                        ++ [([last], (token, next))]
                                        ++ process remainder
                                [] -> process remainder
                        [] -> map (\c -> ([c], (token, token))) opts
            _ -> ("", (token, token)) : process rest
    process [] = []


-- Is this an expansion of multiple items of an array?
isArrayExpansion (T_DollarBraced _ _ l) =
    let string = concat $ oversimplify l in
        "@" `isPrefixOf` string ||
            not ("#" `isPrefixOf` string) && "[@]" `isInfixOf` string
isArrayExpansion _ = False

-- Is it possible that this arg becomes multiple args?
mayBecomeMultipleArgs t = willBecomeMultipleArgs t || f False t
  where
    f quoted (T_DollarBraced _ _ l) =
        let string = concat $ oversimplify l in
            not quoted || "!" `isPrefixOf` string
    f quoted (T_DoubleQuoted _ parts) = any (f True) parts
    f quoted (T_NormalWord _ parts) = any (f quoted) parts
    f _ _ = False

-- Is it certain that this word will becomes multiple words?
willBecomeMultipleArgs t = willConcatInAssignment t || f t
  where
    f T_Extglob {} = True
    f T_Glob {} = True
    f T_BraceExpansion {} = True
    f (T_NormalWord _ parts) = any f parts
    f _ = False

-- This does token cause implicit concatenation in assignments?
willConcatInAssignment token =
    case token of
        t@T_DollarBraced {} -> isArrayExpansion t
        (T_DoubleQuoted _ parts) -> any willConcatInAssignment parts
        (T_NormalWord _ parts) -> any willConcatInAssignment parts
        _ -> False

-- Maybe get the literal string corresponding to this token
getLiteralString :: Token -> Maybe String
getLiteralString = getLiteralStringExt (const Nothing)

-- Definitely get a literal string, with a given default for all non-literals
getLiteralStringDef :: String -> Token -> String
getLiteralStringDef x = runIdentity . getLiteralStringExt (const $ return x)

-- Definitely get a literal string, skipping over all non-literals
onlyLiteralString :: Token -> String
onlyLiteralString = getLiteralStringDef ""

-- Maybe get a literal string, but only if it's an unquoted argument.
getUnquotedLiteral (T_NormalWord _ list) =
    concat <$> mapM str list
  where
    str (T_Literal _ s) = return s
    str _ = Nothing
getUnquotedLiteral _ = Nothing

isQuotes t =
    case t of
        T_DoubleQuoted {} -> True
        T_SingleQuoted {} -> True
        _ -> False

-- Get the last unquoted T_Literal in a word like "${var}foo"THIS
-- or nothing if the word does not end in an unquoted literal.
getTrailingUnquotedLiteral :: Token -> Maybe Token
getTrailingUnquotedLiteral t =
    case t of
        (T_NormalWord _ list@(_:_)) ->
            from (last list)
        _ -> Nothing
  where
    from t =
        case t of
            T_Literal {} -> return t
            _ -> Nothing

-- Get the leading, unquoted, literal string of a token (if any).
getLeadingUnquotedString :: Token -> Maybe String
getLeadingUnquotedString t =
    case t of
        T_NormalWord _ ((T_Literal _ s) : rest) -> return $ s ++ from rest
        _ -> Nothing
  where
    from ((T_Literal _ s):rest) = s ++ from rest
    from _ = ""

-- Maybe get the literal string of this token and any globs in it.
getGlobOrLiteralString = getLiteralStringExt f
  where
    f (T_Glob _ str) = return str
    f _ = Nothing


prop_getLiteralString1 = getLiteralString (T_DollarSingleQuoted (Id 0) "\\x01") == Just "\1"
prop_getLiteralString2 = getLiteralString (T_DollarSingleQuoted (Id 0) "\\xyz") == Just "\\xyz"
prop_getLiteralString3 = getLiteralString (T_DollarSingleQuoted (Id 0) "\\x1") == Just "\x1"
prop_getLiteralString4 = getLiteralString (T_DollarSingleQuoted (Id 0) "\\x1y") == Just "\x1y"
prop_getLiteralString5 = getLiteralString (T_DollarSingleQuoted (Id 0) "\\xy") == Just "\\xy"
prop_getLiteralString6 = getLiteralString (T_DollarSingleQuoted (Id 0) "\\x") == Just "\\x"
prop_getLiteralString7 = getLiteralString (T_DollarSingleQuoted (Id 0) "\\1x") == Just "\1x"
prop_getLiteralString8 = getLiteralString (T_DollarSingleQuoted (Id 0) "\\12x") == Just "\o12x"
prop_getLiteralString9 = getLiteralString (T_DollarSingleQuoted (Id 0) "\\123x") == Just "\o123x"
prop_getLiteralString10 = getLiteralString (T_DollarSingleQuoted (Id 0) "\\1234") == Just "\o123\&4"
prop_getLiteralString11 = getLiteralString (T_DollarSingleQuoted (Id 0) "\\1") == Just "\1"
prop_getLiteralString12 = getLiteralString (T_DollarSingleQuoted (Id 0) "\\12") == Just "\o12"
prop_getLiteralString13 = getLiteralString (T_DollarSingleQuoted (Id 0) "\\123") == Just "\o123"

-- Maybe get the literal value of a token, using a custom function
-- to map unrecognized Tokens into strings.
getLiteralStringExt :: Monad m => (Token -> m String) -> Token -> m String
getLiteralStringExt more = g
  where
    allInList = fmap concat . mapM g
    g (T_DoubleQuoted _ l) = allInList l
    g (T_DollarDoubleQuoted _ l) = allInList l
    g (T_NormalWord _ l) = allInList l
    g (TA_Expansion _ l) = allInList l
    g (T_SingleQuoted _ s) = return s
    g (T_Literal _ s) = return s
    g (T_ParamSubSpecialChar _ s) = return s
    g (T_DollarSingleQuoted _ s) = return $ decodeEscapes s
    g x = more x

    -- Bash style $'..' decoding
    decodeEscapes ('\\':c:cs) =
        case c of
            'a' -> '\a' : rest
            'b' -> '\b' : rest
            'e' -> '\x1B' : rest
            'f' -> '\f' : rest
            'n' -> '\n' : rest
            'r' -> '\r' : rest
            't' -> '\t' : rest
            'v' -> '\v' : rest
            '\'' -> '\'' : rest
            '"' -> '"' : rest
            '\\' -> '\\' : rest
            'x' ->
                case cs of
                    (x:y:more) | isHexDigit x && isHexDigit y ->
                        chr (16*(digitToInt x) + (digitToInt y)) : decodeEscapes more
                    (x:more) | isHexDigit x ->
                        chr (digitToInt x) : decodeEscapes more
                    more -> '\\' : 'x' : decodeEscapes more
            _ | isOctDigit c ->
                let (digits, more) = spanMax isOctDigit 3 (c:cs)
                    num = (parseOct digits) `mod` 256
                in (chr num) : decodeEscapes more
            _ -> '\\' : c : rest
      where
        rest = decodeEscapes cs
        parseOct = f 0
          where
            f n "" = n
            f n (c:rest) = f (n * 8 + digitToInt c) rest
        spanMax f n list =
            let (first, second) = span f list
                (prefix, suffix) = splitAt n first
            in
                (prefix, suffix ++ second)
    decodeEscapes (c:cs) = c : decodeEscapes cs
    decodeEscapes [] = []

-- Is this token a string literal?
isLiteral t = isJust $ getLiteralString t

-- Escape user data for messages.
-- Messages generally avoid repeating user data, but sometimes it's helpful.
e4m = escapeForMessage
escapeForMessage :: String -> String
escapeForMessage str = concatMap f str
  where
    f '\\' = "\\\\"
    f '\n' = "\\n"
    f '\r' = "\\r"
    f '\t' = "\\t"
    f '\x1B' = "\\e"
    f c =
        if shouldEscape c
        then
            if ord c < 256
            then "\\x" ++ (pad0 2 $ toHex c)
            else "\\U" ++ (pad0 4 $ toHex c)
        else [c]

    shouldEscape c =
        (not $ isPrint c)
        || (not (isAscii c) && not (isLetter c))

    pad0 :: Int -> String -> String
    pad0 n s =
        let l = length s in
            if l < n
            then (replicate (n-l) '0') ++ s
            else s
    toHex :: Char -> String
    toHex c = map toUpper $ showHex (ord c) ""

-- Turn a NormalWord like foo="bar $baz" into a series of constituent elements like [foo=,bar ,$baz]
getWordParts (T_NormalWord _ l)   = concatMap getWordParts l
getWordParts (T_DoubleQuoted _ l) = l
-- TA_Expansion is basically T_NormalWord for arithmetic expressions
getWordParts (TA_Expansion _ l)   = concatMap getWordParts l
getWordParts other                = [other]

-- Return a list of NormalWords that would result from brace expansion
braceExpand (T_NormalWord id list) = take 1000 $ do
    items <- mapM part list
    return $ T_NormalWord id items
  where
    part (T_BraceExpansion id items) = do
        item <- items
        braceExpand item
    part x = return x

-- Maybe get a SimpleCommand from immediate wrappers like T_Redirections
getCommand t =
    case t of
        T_Redirecting _ _ w -> getCommand w
        T_SimpleCommand _ _ (w:_) -> return t
        T_Annotation _ _ t -> getCommand t
        _ -> Nothing

-- Maybe get the command name string of a token representing a command
getCommandName :: Token -> Maybe String
getCommandName = fst . getCommandNameAndToken False

-- Maybe get the name+arguments of a command.
getCommandArgv t = do
    (T_SimpleCommand _ _ args@(_:_)) <- getCommand t
    return args

-- Get the command name token from a command, i.e.
-- the token representing 'ls' in 'ls -la 2> foo'.
-- If it can't be determined, return the original token.
getCommandTokenOrThis = snd . getCommandNameAndToken False

-- Given a command, get the string and token that represents the command name.
-- If direct, return the actual command (e.g. exec in 'exec ls')
-- If not, return the logical command (e.g. 'ls' in 'exec ls')

getCommandNameAndToken :: Bool -> Token -> (Maybe String, Token)
getCommandNameAndToken direct t = fromMaybe (Nothing, t) $ do
    cmd@(T_SimpleCommand _ _ (w:rest)) <- getCommand t
    s <- getLiteralString w
    return $ fromMaybe (Just s, w) $ do
        guard $ not direct
        actual <- getEffectiveCommandToken s cmd rest
        return (getLiteralString actual, actual)
  where
    getEffectiveCommandToken str cmd args =
        let
            firstArg = do
                arg <- listToMaybe args
                guard . not $ isFlag arg
                return arg
        in
            case str of
                "busybox" -> firstArg
                "builtin" -> firstArg
                "command" -> firstArg
                "run" -> firstArg -- Used by bats
                "exec" -> do
                    opts <- getBsdOpts "cla:" args
                    (_, (t, _)) <- find (null . fst) opts
                    return t
                _ -> fail ""

-- If a command substitution is a single command, get its name.
--  $(date +%s) = Just "date"
getCommandNameFromExpansion :: Token -> Maybe String
getCommandNameFromExpansion t =
    case t of
        T_DollarExpansion _ [c] -> extract c
        T_Backticked _ [c] -> extract c
        T_DollarBraceCommandExpansion _ [c] -> extract c
        _ -> Nothing
  where
    extract (T_Pipeline _ _ [cmd]) = getCommandName cmd
    extract _ = Nothing

-- Get the basename of a token representing a command
getCommandBasename = fmap basename . getCommandName

basename = reverse . takeWhile (/= '/') . reverse

isAssignment t =
    case t of
        T_Redirecting _ _ w -> isAssignment w
        T_SimpleCommand _ (w:_) [] -> True
        T_Assignment {} -> True
        T_Annotation _ _ w -> isAssignment w
        _ -> False

isOnlyRedirection t =
    case t of
        T_Pipeline _ _ [x] -> isOnlyRedirection x
        T_Annotation _ _ w -> isOnlyRedirection w
        T_Redirecting _ (_:_) c -> isOnlyRedirection c
        T_SimpleCommand _ [] [] -> True
        _ -> False

isFunction t = case t of T_Function {} -> True; _ -> False

-- Bats tests are functions for the purpose of 'local' and such
isFunctionLike t =
    case t of
        T_Function {} -> True
        T_BatsTest {} -> True
        _ -> False


isBraceExpansion t = case t of T_BraceExpansion {} -> True; _ -> False

-- Get the lists of commands from tokens that contain them, such as
-- the conditions and bodies of while loops or branches of if statements.
getCommandSequences :: Token -> [[Token]]
getCommandSequences t =
    case t of
        T_Script _ _ cmds -> [cmds]
        T_BraceGroup _ cmds -> [cmds]
        T_Subshell _ cmds -> [cmds]
        T_WhileExpression _ cond cmds -> [cond, cmds]
        T_UntilExpression _ cond cmds -> [cond, cmds]
        T_ForIn _ _ _ cmds -> [cmds]
        T_ForArithmetic _ _ _ _ cmds -> [cmds]
        T_IfExpression _ thens elses -> (concatMap (\(a,b) -> [a,b]) thens) ++ [elses]
        T_Annotation _ _ t -> getCommandSequences t

        T_DollarExpansion _ cmds -> [cmds]
        T_DollarBraceCommandExpansion _ cmds -> [cmds]
        T_Backticked _ cmds -> [cmds]
        _ -> []

-- Get a list of names of associative arrays
getAssociativeArrays t =
    nub . execWriter $ doAnalysis f t
  where
    f :: Token -> Writer [String] ()
    f t@T_SimpleCommand {} = sequence_ $ do
        name <- getCommandName t
        let assocNames = ["declare","local","typeset"]
        guard $ name `elem` assocNames
        let flags = getAllFlags t
        guard $ "A" `elem` map snd flags
        let args = [arg | (arg, "") <- flags]
        let names = mapMaybe (getLiteralStringExt nameAssignments) args
        return $ tell names
    f _ = return ()

    nameAssignments t =
        case t of
            T_Assignment _ _ name _ _ -> return name
            _ -> Nothing

-- A Pseudoglob is a wildcard pattern used for checking if a match can succeed.
-- For example, [[ $(cmd).jpg == [a-z] ]] will give the patterns *.jpg and ?, which
-- can be proven never to match.
data PseudoGlob = PGAny | PGMany | PGChar Char
    deriving (Eq, Show)

-- Turn a word into a PG pattern, replacing all unknown/runtime values with
-- PGMany.
wordToPseudoGlob :: Token -> [PseudoGlob]
wordToPseudoGlob = fromMaybe [PGMany] . wordToPseudoGlob' False

-- Turn a word into a PG pattern, but only if we can preserve
-- exact semantics.
wordToExactPseudoGlob :: Token -> Maybe [PseudoGlob]
wordToExactPseudoGlob = wordToPseudoGlob' True

wordToPseudoGlob' :: Bool -> Token -> Maybe [PseudoGlob]
wordToPseudoGlob' exact word =
    simplifyPseudoGlob <$> toGlob word
  where
    toGlob :: Token -> Maybe [PseudoGlob]
    toGlob word =
        case word of
            T_NormalWord _ (T_Literal _ ('~':str):rest) -> do
                guard $ not exact
                let this = (PGMany : (map PGChar $ dropWhile (/= '/') str))
                tail <- concat <$> (mapM f $ concatMap getWordParts rest)
                return $ this ++ tail
            _ -> concat <$> (mapM f $ getWordParts word)

    f x = case x of
        T_Literal _ s      -> return $ map PGChar s
        T_SingleQuoted _ s -> return $ map PGChar s
        T_Glob _ "?"       -> return [PGAny]
        T_Glob _ "*"       -> return [PGMany]
        T_Glob _ ('[':_) | not exact -> return [PGAny]
        _ -> if exact then fail "" else return [PGMany]


-- Reorder a PseudoGlob for more efficient matching, e.g.
-- f?*?**g -> f??*g
simplifyPseudoGlob :: [PseudoGlob] -> [PseudoGlob]
simplifyPseudoGlob = f
  where
    f [] = []
    f (x@(PGChar _) : rest ) = x : f rest
    f list =
        let (anys, rest) = span (\x -> x == PGMany || x == PGAny) list in
            order anys ++ f rest

    order s = let (any, many) = partition (== PGAny) s in
        any ++ take 1 many

-- Check whether the two patterns can ever overlap.
pseudoGlobsCanOverlap :: [PseudoGlob] -> [PseudoGlob] -> Bool
pseudoGlobsCanOverlap = matchable
  where
    matchable x@(xf:xs) y@(yf:ys) =
        case (xf, yf) of
            (PGMany, _) -> matchable x ys || matchable xs y
            (_, PGMany) -> matchable x ys || matchable xs y
            (PGAny, _) -> matchable xs ys
            (_, PGAny) -> matchable xs ys
            (_, _) -> xf == yf && matchable xs ys

    matchable [] [] = True
    matchable (PGMany : rest) [] = matchable rest []
    matchable (_:_) [] = False
    matchable [] r = matchable r []

-- Check whether the first pattern always overlaps the second.
pseudoGlobIsSuperSetof :: [PseudoGlob] -> [PseudoGlob] -> Bool
pseudoGlobIsSuperSetof = matchable
  where
    matchable x@(xf:xs) y@(yf:ys) =
        case (xf, yf) of
            (PGMany, PGMany) -> matchable x ys
            (PGMany, _) -> matchable x ys || matchable xs y
            (_, PGMany) -> False
            (PGAny, _) -> matchable xs ys
            (_, PGAny) -> False
            (_, _) -> xf == yf && matchable xs ys

    matchable [] [] = True
    matchable (PGMany : rest) [] = matchable rest []
    matchable _ _ = False

wordsCanBeEqual x y = pseudoGlobsCanOverlap (wordToPseudoGlob x) (wordToPseudoGlob y)

-- Is this an expansion that can be quoted,
-- e.g. $(foo) `foo` $foo (but not {foo,})?
isQuoteableExpansion t = case t of
    T_DollarBraced {} -> True
    _ -> isCommandSubstitution t

isCommandSubstitution t = case t of
    T_DollarExpansion {} -> True
    T_DollarBraceCommandExpansion {} -> True
    T_Backticked {} -> True
    _ -> False

-- Is this an expansion that results in a simple string?
isStringExpansion t = isCommandSubstitution t || case t of
    T_DollarArithmetic {} -> True
    T_DollarBraced {} -> not (isArrayExpansion t)
    _ -> False

-- Is this a T_Annotation that ignores a specific code?
isAnnotationIgnoringCode code t =
    case t of
        T_Annotation _ anns _ -> any hasNum anns
        _ -> False
  where
    hasNum (DisableComment from to) = code >= from && code < to
    hasNum _                   = False

prop_executableFromShebang1 = executableFromShebang "/bin/sh" == "sh"
prop_executableFromShebang2 = executableFromShebang "/bin/bash" == "bash"
prop_executableFromShebang3 = executableFromShebang "/usr/bin/env ksh" == "ksh"
prop_executableFromShebang4 = executableFromShebang "/usr/bin/env -S foo=bar bash -x" == "bash"
prop_executableFromShebang5 = executableFromShebang "/usr/bin/env --split-string=bash -x" == "bash"
prop_executableFromShebang6 = executableFromShebang "/usr/bin/env --split-string=foo=bar bash -x" == "bash"
prop_executableFromShebang7 = executableFromShebang "/usr/bin/env --split-string bash -x" == "bash"
prop_executableFromShebang8 = executableFromShebang "/usr/bin/env --split-string foo=bar bash -x" == "bash"
prop_executableFromShebang9 = executableFromShebang "/usr/bin/env foo=bar dash" == "dash"
prop_executableFromShebang10 = executableFromShebang "/bin/busybox sh" == "ash"
prop_executableFromShebang11 = executableFromShebang "/bin/busybox ash" == "ash"

-- Get the shell executable from a string like '/usr/bin/env bash'
executableFromShebang :: String -> String
executableFromShebang = shellFor
  where
    re = mkRegex "/env +(-S|--split-string=?)? *(.*)"
    shellFor s | s `matches` re =
        case matchRegex re s of
            Just [flag, shell] -> fromEnvArgs (words shell)
            _ -> ""
    shellFor sb =
        case words sb of
            [] -> ""
            [x] -> basename x
            (first:second:args) | basename first == "busybox" ->
                case basename second of
                   "sh" -> "ash" -- busybox sh is ash
                   x -> x
            (first:args) | basename first == "env" ->
                fromEnvArgs args
            (first:_) -> basename first

    fromEnvArgs args = fromMaybe "" $ find (notElem '=') $ skipFlags args
    basename s = reverse . takeWhile (/= '/') . reverse $ s
    skipFlags = dropWhile ("-" `isPrefixOf`)


-- Determining if a name is a variable
isVariableStartChar x = x == '_' || isAsciiLower x || isAsciiUpper x
isVariableChar x = isVariableStartChar x || isDigit x
isSpecialVariableChar = (`elem` "*@#?-$!")
variableNameRegex = mkRegex "[_a-zA-Z][_a-zA-Z0-9]*"

prop_isVariableName1 = isVariableName "_fo123"
prop_isVariableName2 = not $ isVariableName "4"
prop_isVariableName3 = not $ isVariableName "test: "
isVariableName (x:r) = isVariableStartChar x && all isVariableChar r
isVariableName _     = False


-- Get the variable name from an expansion like ${var:-foo}
prop_getBracedReference1 = getBracedReference "foo" == "foo"
prop_getBracedReference2 = getBracedReference "#foo" == "foo"
prop_getBracedReference3 = getBracedReference "#" == "#"
prop_getBracedReference4 = getBracedReference "##" == "#"
prop_getBracedReference5 = getBracedReference "#!" == "!"
prop_getBracedReference6 = getBracedReference "!#" == "#"
prop_getBracedReference7 = getBracedReference "!foo#?" == "foo"
prop_getBracedReference8 = getBracedReference "foo-bar" == "foo"
prop_getBracedReference9 = getBracedReference "foo:-bar" == "foo"
prop_getBracedReference10 = getBracedReference "foo: -1" == "foo"
prop_getBracedReference11 = getBracedReference "!os*" == ""
prop_getBracedReference11b = getBracedReference "!os@" == ""
prop_getBracedReference12 = getBracedReference "!os?bar**" == ""
prop_getBracedReference13 = getBracedReference "foo[bar]" == "foo"
getBracedReference s = fromMaybe s $
    nameExpansion s `mplus` takeName noPrefix `mplus` getSpecial noPrefix `mplus` getSpecial s
  where
    noPrefix = dropPrefix s
    dropPrefix (c:rest) | c `elem` "!#" = rest
    dropPrefix cs = cs
    takeName s = do
        let name = takeWhile isVariableChar s
        guard . not $ null name
        return name
    getSpecial (c:_) | isSpecialVariableChar c = return [c]
    getSpecial _ = fail "empty or not special"

    nameExpansion ('!':next:rest) = do -- e.g. ${!foo*bar*}
        guard $ isVariableChar next -- e.g. ${!@}
        first <- find (not . isVariableChar) rest
        guard $ first `elem` "*?@"
        return ""
    nameExpansion _ = Nothing

-- Get the variable modifier like /a/b in ${var/a/b}
prop_getBracedModifier1 = getBracedModifier "foo:bar:baz" == ":bar:baz"
prop_getBracedModifier2 = getBracedModifier "!var:-foo" == ":-foo"
prop_getBracedModifier3 = getBracedModifier "foo[bar]" == "[bar]"
prop_getBracedModifier4 = getBracedModifier "foo[@]@Q" == "[@]@Q"
prop_getBracedModifier5 = getBracedModifier "@@Q" == "@Q"
getBracedModifier s = headOrDefault "" $ do
    let var = getBracedReference s
    a <- dropModifier s
    dropPrefix var a
  where
    dropPrefix [] t        = return t
    dropPrefix (a:b) (c:d) | a == c = dropPrefix b d
    dropPrefix _ _         = []

    dropModifier (c:rest) | c `elem` "#!" = [rest, c:rest]
    dropModifier x        = [x]

-- Get the variables from indices like ["x", "y"] in ${var[x+y+1]}
prop_getIndexReferences1 = getIndexReferences "var[x+y+1]" == ["x", "y"]
getIndexReferences s = fromMaybe [] $ do
    match <- matchRegex re s
    index <- match !!! 0
    return $ matchAllStrings variableNameRegex index
  where
    re = mkRegex "(\\[.*\\])"

prop_getOffsetReferences1 = getOffsetReferences ":bar" == ["bar"]
prop_getOffsetReferences2 = getOffsetReferences ":bar:baz" == ["bar", "baz"]
prop_getOffsetReferences3 = getOffsetReferences "[foo]:bar" == ["bar"]
prop_getOffsetReferences4 = getOffsetReferences "[foo]:bar:baz" == ["bar", "baz"]
getOffsetReferences mods = fromMaybe [] $ do
-- if mods start with [, then drop until ]
    match <- matchRegex re mods
    offsets <- match !!! 1
    return $ matchAllStrings variableNameRegex offsets
  where
    re = mkRegex "^(\\[.+\\])? *:([^-=?+].*)"


-- Returns whether a token is a parameter expansion without any modifiers.
-- True for $var ${var} $1 $#
-- False for ${#var} ${var[x]} ${var:-0}
isUnmodifiedParameterExpansion t =
    case t of
        T_DollarBraced _ False _ -> True
        T_DollarBraced _ _ list ->
            let str = concat $ oversimplify list
            in getBracedReference str == str
        _ -> False

--- A list of the element and all its parents up to the root node.
getPath tree t = t :
    case Map.lookup (getId t) tree of
        Nothing     -> []
        Just parent -> getPath tree parent

isClosingFileOp op =
    case op of
        T_IoDuplicate _ (T_GREATAND _) "-" -> True
        T_IoDuplicate _ (T_LESSAND  _) "-" -> True
        _                                  -> False

getEnableDirectives root =
    case root of
        T_Annotation _ list _ -> [s | EnableComment s <- list]
        _ -> []

return []
runTests = $quickCheckAll
