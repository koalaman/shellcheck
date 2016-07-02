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
module ShellCheck.ASTLib where

import ShellCheck.AST

import Control.Monad.Writer
import Control.Monad
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

isGlob (T_Extglob {}) = True
isGlob (T_Glob {}) = True
isGlob (T_NormalWord _ l) = any isGlob l
isGlob _ = False

-- Is this shell word a constant?
isConstant token =
    case token of
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
        (T_SimpleCommand _ vars words) -> concatMap oversimplify words
        (T_Redirecting _ _ foo) -> oversimplify foo
        (T_DollarSingleQuoted _ s) -> [s]
        (T_Annotation _ _ s) -> oversimplify s
        -- Workaround for let "foo = bar" parsing
        (TA_Sequence _ [TA_Expansion _ v]) -> concatMap oversimplify v
        otherwise -> []


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
getAllFlags = getFlagsUntil (== "--")
-- Get all flags in a BSD way, up until first non-flag argument
getLeadingFlags = getFlagsUntil (not . ("-" `isPrefixOf`))


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
    f (T_Extglob {}) = True
    f (T_Glob {}) = True
    f (T_BraceExpansion {}) = True
    f (T_DoubleQuoted _ parts) = any f parts
    f (T_NormalWord _ parts) = any f parts
    f _ = False

-- This does token cause implicit concatenation in assignments?
willConcatInAssignment token =
    case token of
        t@(T_DollarBraced {}) -> isArrayExpansion t
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
    liftM concat $ mapM str list
  where
    str (T_Literal _ s) = return s
    str _ = Nothing
getUnquotedLiteral _ = Nothing

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
    allInList = liftM concat . mapM g
    g (T_DoubleQuoted _ l) = allInList l
    g (T_DollarDoubleQuoted _ l) = allInList l
    g (T_NormalWord _ l) = allInList l
    g (TA_Expansion _ l) = allInList l
    g (T_SingleQuoted _ s) = return s
    g (T_Literal _ s) = return s
    g x = more x

-- Is this token a string literal?
isLiteral t = isJust $ getLiteralString t


-- Turn a NormalWord like foo="bar $baz" into a series of constituent elements like [foo=,bar ,$baz]
getWordParts (T_NormalWord _ l)   = concatMap getWordParts l
getWordParts (T_DoubleQuoted _ l) = l
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

-- Maybe get the command name of a token representing a command
getCommandName t =
    case t of
        T_Redirecting _ _ w -> getCommandName w
        T_SimpleCommand _ _ (w:_) -> getLiteralString w
        T_Annotation _ _ t -> getCommandName t
        otherwise -> Nothing

-- If a command substitution is a single command, get its name.
--  $(date +%s) = Just "date"
getCommandNameFromExpansion :: Token -> Maybe String
getCommandNameFromExpansion t =
    case t of
        T_DollarExpansion _ [c] -> extract c
        T_Backticked _ [c] -> extract c
        T_DollarBraceCommandExpansion _ [c] -> extract c
        otherwise -> Nothing
  where
    extract (T_Pipeline _ _ [cmd]) = getCommandName cmd
    extract _ = Nothing

-- Get the basename of a token representing a command
getCommandBasename = liftM basename . getCommandName
  where
    basename = reverse . takeWhile (/= '/') . reverse

isAssignment t =
    case t of
        T_Redirecting _ _ w -> isAssignment w
        T_SimpleCommand _ (w:_) [] -> True
        T_Assignment {} -> True
        T_Annotation _ _ w -> isAssignment w
        otherwise -> False

isOnlyRedirection t =
    case t of
        T_Pipeline _ _ [x] -> isOnlyRedirection x
        T_Annotation _ _ w -> isOnlyRedirection w
        T_Redirecting _ (_:_) c -> isOnlyRedirection c
        T_SimpleCommand _ [] [] -> True
        otherwise -> False

isFunction t = case t of T_Function {} -> True; _ -> False

-- Get the lists of commands from tokens that contain them, such as
-- the body of while loops or branches of if statements.
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
        otherwise -> []

-- Get a list of names of associative arrays
getAssociativeArrays t =
    nub . execWriter $ doAnalysis f t
  where
    f :: Token -> Writer [String] ()
    f t@(T_SimpleCommand {}) = fromMaybe (return ()) $ do
        name <- getCommandName t
        guard $ name == "declare"
        let flags = getAllFlags t
        guard $ elem "A" $ map snd flags
        let args = map fst . filter ((==) "" . snd) $ flags
        let names = mapMaybe (getLiteralStringExt nameAssignments) args
        return $ tell names
    f _ = return ()

    nameAssignments t =
        case t of
            T_Assignment _ _ name _ _ -> return name
            otherwise -> Nothing
