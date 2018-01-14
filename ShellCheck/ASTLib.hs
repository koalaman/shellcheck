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
module ShellCheck.ASTLib where

import ShellCheck.AST

import Control.Monad.Writer
import Control.Monad
import Data.Functor
import Data.List
import Data.Maybe

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
    T_NormalWord _ l -> any willSplit l
    _ -> False

isGlob T_Extglob {} = True
isGlob T_Glob {} = True
isGlob (T_NormalWord _ l) = any isGlob l
isGlob _ = False

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
        (T_DollarBraced _ _) -> ["${VAR}"]
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
getFlagsUntil _ _ = error "Internal shellcheck error, please report! (getFlags on non-command)"

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

-- Given a T_DollarBraced, return a simplified version of the string contents.
bracedString (T_DollarBraced _ l) = concat $ oversimplify l
bracedString _ = error "Internal shellcheck error, please report! (bracedString on non-variable)"

-- Is this an expansion of multiple items of an array?
isArrayExpansion t@(T_DollarBraced _ _) =
    let string = bracedString t in
        "@" `isPrefixOf` string ||
            not ("#" `isPrefixOf` string) && "[@]" `isInfixOf` string
isArrayExpansion _ = False

-- Is it possible that this arg becomes multiple args?
mayBecomeMultipleArgs t = willBecomeMultipleArgs t || f t
  where
    f t@(T_DollarBraced _ _) =
        let string = bracedString t in
            "!" `isPrefixOf` string
    f (T_DoubleQuoted _ parts) = any f parts
    f (T_NormalWord _ parts) = any f parts
    f _ = False

-- Is it certain that this word will becomes multiple words?
willBecomeMultipleArgs t = willConcatInAssignment t || f t
  where
    f T_Extglob {} = True
    f T_Glob {} = True
    f T_BraceExpansion {} = True
    f (T_DoubleQuoted _ parts) = any f parts
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

-- Definitely get a literal string, skipping over all non-literals
onlyLiteralString :: Token -> String
onlyLiteralString = fromJust . getLiteralStringExt (const $ return "")

-- Maybe get a literal string, but only if it's an unquoted argument.
getUnquotedLiteral (T_NormalWord _ list) =
    concat <$> mapM str list
  where
    str (T_Literal _ s) = return s
    str _ = Nothing
getUnquotedLiteral _ = Nothing

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
        T_NormalWord _ ((T_Literal _ s) : _) -> return s
        _ -> Nothing

-- Maybe get the literal string of this token and any globs in it.
getGlobOrLiteralString = getLiteralStringExt f
  where
    f (T_Glob _ str) = return str
    f _ = Nothing

-- Maybe get the literal value of a token, using a custom function
-- to map unrecognized Tokens into strings.
getLiteralStringExt :: (Token -> Maybe String) -> Token -> Maybe String
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
    g x = more x

-- Is this token a string literal?
isLiteral t = isJust $ getLiteralString t


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

-- Maybe get the command name of a token representing a command
getCommandName t = do
    (T_SimpleCommand _ _ (w:rest)) <- getCommand t
    s <- getLiteralString w
    if "busybox" `isSuffixOf` s
        then
            case rest of
                (applet:_) -> getLiteralString applet
                _ -> return s
        else
            return s

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
  where
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

isBraceExpansion t = case t of T_BraceExpansion {} -> True; _ -> False

-- Get the lists of commands from tokens that contain them, such as
-- the body of while loops or branches of if statements.
getCommandSequences :: Token -> [[Token]]
getCommandSequences t =
    case t of
        T_Script _ _ cmds -> [cmds]
        T_BraceGroup _ cmds -> [cmds]
        T_Subshell _ cmds -> [cmds]
        T_WhileExpression _ _ cmds -> [cmds]
        T_UntilExpression _ _ cmds -> [cmds]
        T_ForIn _ _ _ cmds -> [cmds]
        T_ForArithmetic _ _ _ _ cmds -> [cmds]
        T_IfExpression _ thens elses -> map snd thens ++ [elses]
        T_Annotation _ _ t -> getCommandSequences t
        _ -> []

-- Get a list of names of associative arrays
getAssociativeArrays t =
    nub . execWriter $ doAnalysis f t
  where
    f :: Token -> Writer [String] ()
    f t@T_SimpleCommand {} = fromMaybe (return ()) $ do
        name <- getCommandName t
        let assocNames = ["declare","local","typeset"]
        guard $ elem name assocNames
        let flags = getAllFlags t
        guard $ elem "A" $ map snd flags
        let args = map fst . filter ((==) "" . snd) $ flags
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
wordToPseudoGlob :: Token -> Maybe [PseudoGlob]
wordToPseudoGlob word =
    simplifyPseudoGlob . concat <$> mapM f (getWordParts word)
  where
    f x = case x of
        T_Literal _ s -> return $ map PGChar s
        T_SingleQuoted _ s -> return $ map PGChar s

        T_DollarBraced {} -> return [PGMany]
        T_DollarExpansion {} -> return [PGMany]
        T_Backticked {} -> return [PGMany]

        T_Glob _ "?" -> return [PGAny]
        T_Glob _ ('[':_)  -> return [PGAny]
        T_Glob {} -> return [PGMany]

        T_Extglob {} -> return [PGMany]

        _ -> return [PGMany]

-- Turn a word into a PG pattern, but only if we can preserve
-- exact semantics.
wordToExactPseudoGlob :: Token -> Maybe [PseudoGlob]
wordToExactPseudoGlob word =
    simplifyPseudoGlob . concat <$> mapM f (getWordParts word)
  where
    f x = case x of
        T_Literal _ s -> return $ map PGChar s
        T_SingleQuoted _ s -> return $ map PGChar s
        T_Glob _ "?" -> return [PGAny]
        T_Glob _ "*" -> return [PGMany]
        _ -> fail "Unknown token type"

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

wordsCanBeEqual x y = fromMaybe True $
    liftM2 pseudoGlobsCanOverlap (wordToPseudoGlob x) (wordToPseudoGlob y)
