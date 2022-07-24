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
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveTraversable, PatternSynonyms #-}
module ShellCheck.AST where

import GHC.Generics (Generic)
import Control.Monad.Identity
import Control.DeepSeq
import Text.Parsec
import qualified ShellCheck.Regex as Re
import Prelude hiding (id)

newtype Id = Id Int deriving (Show, Eq, Ord, Generic, NFData)

data Quoted = Quoted | Unquoted deriving (Show, Eq)
data Dashed = Dashed | Undashed deriving (Show, Eq)
data AssignmentMode = Assign | Append deriving (Show, Eq)
newtype FunctionKeyword = FunctionKeyword Bool deriving (Show, Eq)
newtype FunctionParentheses = FunctionParentheses Bool deriving (Show, Eq)
data CaseType = CaseBreak | CaseFallThrough | CaseContinue deriving (Show, Eq)

newtype Root = Root Token
data Token = OuterToken Id (InnerToken Token) deriving (Show)

data InnerToken t =
    Inner_TA_Binary String t t
    | Inner_TA_Assignment String t t
    | Inner_TA_Variable String [t]
    | Inner_TA_Expansion [t]
    | Inner_TA_Sequence [t]
    | Inner_TA_Parenthesis t
    | Inner_TA_Trinary t t t
    | Inner_TA_Unary String t
    | Inner_TC_And ConditionType String t t
    | Inner_TC_Binary ConditionType String t t
    | Inner_TC_Group ConditionType t
    | Inner_TC_Nullary ConditionType t
    | Inner_TC_Or ConditionType String t t
    | Inner_TC_Unary ConditionType String t
    | Inner_TC_Empty ConditionType
    | Inner_T_AND_IF
    | Inner_T_AndIf t t
    | Inner_T_Arithmetic t
    | Inner_T_Array [t]
    | Inner_T_IndexedElement [t] t
    -- Store the index as string, and parse as arithmetic or string later
    | Inner_T_UnparsedIndex SourcePos String
    | Inner_T_Assignment AssignmentMode String [t] t
    | Inner_T_Backgrounded t
    | Inner_T_Backticked [t]
    | Inner_T_Bang
    | Inner_T_Banged t
    | Inner_T_BraceExpansion [t]
    | Inner_T_BraceGroup [t]
    | Inner_T_CLOBBER
    | Inner_T_Case
    | Inner_T_CaseExpression t [(CaseType, [t], [t])]
    | Inner_T_Condition ConditionType t
    | Inner_T_DGREAT
    | Inner_T_DLESS
    | Inner_T_DLESSDASH
    | Inner_T_DSEMI
    | Inner_T_Do
    | Inner_T_DollarArithmetic t
    | Inner_T_DollarBraced Bool t
    | Inner_T_DollarBracket t
    | Inner_T_DollarDoubleQuoted [t]
    | Inner_T_DollarExpansion [t]
    | Inner_T_DollarSingleQuoted String
    | Inner_T_DollarBraceCommandExpansion [t]
    | Inner_T_Done
    | Inner_T_DoubleQuoted [t]
    | Inner_T_EOF
    | Inner_T_Elif
    | Inner_T_Else
    | Inner_T_Esac
    | Inner_T_Extglob String [t]
    | Inner_T_FdRedirect String t
    | Inner_T_Fi
    | Inner_T_For
    | Inner_T_ForArithmetic t t t [t]
    | Inner_T_ForIn String [t] [t]
    | Inner_T_Function FunctionKeyword FunctionParentheses String t
    | Inner_T_GREATAND
    | Inner_T_Glob String
    | Inner_T_Greater
    | Inner_T_HereDoc Dashed Quoted String [t]
    | Inner_T_HereString t
    | Inner_T_If
    | Inner_T_IfExpression [([t],[t])] [t]
    | Inner_T_In
    | Inner_T_IoFile t t
    | Inner_T_IoDuplicate t String
    | Inner_T_LESSAND
    | Inner_T_LESSGREAT
    | Inner_T_Lbrace
    | Inner_T_Less
    | Inner_T_Literal String
    | Inner_T_Lparen
    | Inner_T_NEWLINE
    | Inner_T_NormalWord [t]
    | Inner_T_OR_IF
    | Inner_T_OrIf t t
    | Inner_T_ParamSubSpecialChar String -- e.g. '%' in ${foo%bar}  or '/' in ${foo/bar/baz}
    | Inner_T_Pipeline [t] [t] -- [Pipe separators] [Commands]
    | Inner_T_ProcSub String [t]
    | Inner_T_Rbrace
    | Inner_T_Redirecting [t] t
    | Inner_T_Rparen
    | Inner_T_Script t [t] -- Shebang T_Literal, followed by script.
    | Inner_T_Select
    | Inner_T_SelectIn String [t] [t]
    | Inner_T_Semi
    | Inner_T_SimpleCommand [t] [t]
    | Inner_T_SingleQuoted String
    | Inner_T_Subshell [t]
    | Inner_T_Then
    | Inner_T_Until
    | Inner_T_UntilExpression [t] [t]
    | Inner_T_While
    | Inner_T_WhileExpression [t] [t]
    | Inner_T_Annotation [Annotation] t
    | Inner_T_Pipe String
    | Inner_T_CoProc (Maybe String) t
    | Inner_T_CoProcBody t
    | Inner_T_Include t
    | Inner_T_SourceCommand t t
    | Inner_T_BatsTest t t
    deriving (Show, Eq, Functor, Foldable, Traversable)

data Annotation =
    DisableComment Integer Integer -- [from, to)
    | EnableComment String
    | SourceOverride String
    | ShellOverride String
    | SourcePath String
    | ExternalSources Bool
    deriving (Show, Eq)
data ConditionType = DoubleBracket | SingleBracket deriving (Show, Eq)

pattern T_AND_IF id = OuterToken id Inner_T_AND_IF
pattern T_Bang id = OuterToken id Inner_T_Bang
pattern T_Case id = OuterToken id Inner_T_Case
pattern TC_Empty id typ = OuterToken id (Inner_TC_Empty typ)
pattern T_CLOBBER id = OuterToken id Inner_T_CLOBBER
pattern T_DGREAT id = OuterToken id Inner_T_DGREAT
pattern T_DLESS id = OuterToken id Inner_T_DLESS
pattern T_DLESSDASH id = OuterToken id Inner_T_DLESSDASH
pattern T_Do id = OuterToken id Inner_T_Do
pattern T_DollarSingleQuoted id str = OuterToken id (Inner_T_DollarSingleQuoted str)
pattern T_Done id = OuterToken id Inner_T_Done
pattern T_DSEMI id = OuterToken id Inner_T_DSEMI
pattern T_Elif id = OuterToken id Inner_T_Elif
pattern T_Else id = OuterToken id Inner_T_Else
pattern T_EOF id = OuterToken id Inner_T_EOF
pattern T_Esac id = OuterToken id Inner_T_Esac
pattern T_Fi id = OuterToken id Inner_T_Fi
pattern T_For id = OuterToken id Inner_T_For
pattern T_Glob id str = OuterToken id (Inner_T_Glob str)
pattern T_GREATAND id = OuterToken id Inner_T_GREATAND
pattern T_Greater id = OuterToken id Inner_T_Greater
pattern T_If id = OuterToken id Inner_T_If
pattern T_In id = OuterToken id Inner_T_In
pattern T_Lbrace id = OuterToken id Inner_T_Lbrace
pattern T_Less id = OuterToken id Inner_T_Less
pattern T_LESSAND id = OuterToken id Inner_T_LESSAND
pattern T_LESSGREAT id = OuterToken id Inner_T_LESSGREAT
pattern T_Literal id str = OuterToken id (Inner_T_Literal str)
pattern T_Lparen id = OuterToken id Inner_T_Lparen
pattern T_NEWLINE id = OuterToken id Inner_T_NEWLINE
pattern T_OR_IF id = OuterToken id Inner_T_OR_IF
pattern T_ParamSubSpecialChar id str = OuterToken id (Inner_T_ParamSubSpecialChar str)
pattern T_Pipe id str = OuterToken id (Inner_T_Pipe str)
pattern T_Rbrace id = OuterToken id Inner_T_Rbrace
pattern T_Rparen id = OuterToken id Inner_T_Rparen
pattern T_Select id = OuterToken id Inner_T_Select
pattern T_Semi id = OuterToken id Inner_T_Semi
pattern T_SingleQuoted id str = OuterToken id (Inner_T_SingleQuoted str)
pattern T_Then id = OuterToken id Inner_T_Then
pattern T_UnparsedIndex id pos str = OuterToken id (Inner_T_UnparsedIndex pos str)
pattern T_Until id = OuterToken id Inner_T_Until
pattern T_While id = OuterToken id Inner_T_While
pattern TA_Assignment id op t1 t2 = OuterToken id (Inner_TA_Assignment op t1 t2)
pattern TA_Binary id op t1 t2 = OuterToken id (Inner_TA_Binary op t1 t2)
pattern TA_Expansion id t = OuterToken id (Inner_TA_Expansion t)
pattern T_AndIf id t u = OuterToken id (Inner_T_AndIf t u)
pattern T_Annotation id anns t = OuterToken id (Inner_T_Annotation anns t)
pattern T_Arithmetic id c = OuterToken id (Inner_T_Arithmetic c)
pattern T_Array id t = OuterToken id (Inner_T_Array t)
pattern TA_Sequence id l = OuterToken id (Inner_TA_Sequence l)
pattern TA_Parentesis id t = OuterToken id (Inner_TA_Parenthesis t)
pattern T_Assignment id mode var indices value = OuterToken id (Inner_T_Assignment mode var indices value)
pattern TA_Trinary id t1 t2 t3 = OuterToken id (Inner_TA_Trinary t1 t2 t3)
pattern TA_Unary id op t1 = OuterToken id (Inner_TA_Unary op t1)
pattern TA_Variable id str t = OuterToken id (Inner_TA_Variable str t)
pattern T_Backgrounded id l = OuterToken id (Inner_T_Backgrounded l)
pattern T_Backticked id list = OuterToken id (Inner_T_Backticked list)
pattern T_Banged id l = OuterToken id (Inner_T_Banged l)
pattern T_BatsTest id name t = OuterToken id (Inner_T_BatsTest name t)
pattern T_BraceExpansion id list = OuterToken id (Inner_T_BraceExpansion list)
pattern T_BraceGroup id l = OuterToken id (Inner_T_BraceGroup l)
pattern TC_And id typ str t1 t2 = OuterToken id (Inner_TC_And typ str t1 t2)
pattern T_CaseExpression id word cases = OuterToken id (Inner_T_CaseExpression word cases)
pattern TC_Binary id typ op lhs rhs = OuterToken id (Inner_TC_Binary typ op lhs rhs)
pattern TC_Group id typ token = OuterToken id (Inner_TC_Group typ token)
pattern TC_Nullary id typ token = OuterToken id (Inner_TC_Nullary typ token)
pattern T_Condition id typ token = OuterToken id (Inner_T_Condition typ token)
pattern T_CoProcBody id t = OuterToken id (Inner_T_CoProcBody t)
pattern T_CoProc id var body = OuterToken id (Inner_T_CoProc var body)
pattern TC_Or id typ str t1 t2 = OuterToken id (Inner_TC_Or typ str t1 t2)
pattern TC_Unary id typ op token = OuterToken id (Inner_TC_Unary typ op token)
pattern T_DollarArithmetic id c = OuterToken id (Inner_T_DollarArithmetic c)
pattern T_DollarBraceCommandExpansion id list = OuterToken id (Inner_T_DollarBraceCommandExpansion list)
pattern T_DollarBraced id braced op = OuterToken id (Inner_T_DollarBraced braced op)
pattern T_DollarBracket id c = OuterToken id (Inner_T_DollarBracket c)
pattern T_DollarDoubleQuoted id list = OuterToken id (Inner_T_DollarDoubleQuoted list)
pattern T_DollarExpansion id list = OuterToken id (Inner_T_DollarExpansion list)
pattern T_DoubleQuoted id list = OuterToken id (Inner_T_DoubleQuoted list)
pattern T_Extglob id str l = OuterToken id (Inner_T_Extglob str l)
pattern T_FdRedirect id v t = OuterToken id (Inner_T_FdRedirect v t)
pattern T_ForArithmetic id a b c group = OuterToken id (Inner_T_ForArithmetic a b c group)
pattern T_ForIn id v w l = OuterToken id (Inner_T_ForIn v w l)
pattern T_Function id a b name body = OuterToken id (Inner_T_Function a b name body)
pattern T_HereDoc id d q str l = OuterToken id (Inner_T_HereDoc d q str l)
pattern T_HereString id word = OuterToken id (Inner_T_HereString word)
pattern T_IfExpression id conditions elses = OuterToken id (Inner_T_IfExpression conditions elses)
pattern T_Include id script = OuterToken id (Inner_T_Include script)
pattern T_IndexedElement id indices t = OuterToken id (Inner_T_IndexedElement indices t)
pattern T_IoDuplicate id op num = OuterToken id (Inner_T_IoDuplicate op num)
pattern T_IoFile id op file = OuterToken id (Inner_T_IoFile op file)
pattern T_NormalWord id list = OuterToken id (Inner_T_NormalWord list)
pattern T_OrIf id t u = OuterToken id (Inner_T_OrIf t u)
pattern T_Pipeline id l1 l2 = OuterToken id (Inner_T_Pipeline l1 l2)
pattern T_ProcSub id typ l = OuterToken id (Inner_T_ProcSub typ l)
pattern T_Redirecting id redirs cmd = OuterToken id (Inner_T_Redirecting redirs cmd)
pattern T_Script id shebang list = OuterToken id (Inner_T_Script shebang list)
pattern T_SelectIn id v w l = OuterToken id (Inner_T_SelectIn v w l)
pattern T_SimpleCommand id vars cmds = OuterToken id (Inner_T_SimpleCommand vars cmds)
pattern T_SourceCommand id includer t_include = OuterToken id (Inner_T_SourceCommand includer t_include)
pattern T_Subshell id l = OuterToken id (Inner_T_Subshell l)
pattern T_UntilExpression id c l = OuterToken id (Inner_T_UntilExpression c l)
pattern T_WhileExpression id c l = OuterToken id (Inner_T_WhileExpression c l)

{-# COMPLETE T_AND_IF, T_Bang, T_Case, TC_Empty, T_CLOBBER, T_DGREAT, T_DLESS, T_DLESSDASH, T_Do, T_DollarSingleQuoted, T_Done, T_DSEMI, T_Elif, T_Else, T_EOF, T_Esac, T_Fi, T_For, T_Glob, T_GREATAND, T_Greater, T_If, T_In, T_Lbrace, T_Less, T_LESSAND, T_LESSGREAT, T_Literal, T_Lparen, T_NEWLINE, T_OR_IF, T_ParamSubSpecialChar, T_Pipe, T_Rbrace, T_Rparen, T_Select, T_Semi, T_SingleQuoted, T_Then, T_UnparsedIndex, T_Until, T_While, TA_Assignment, TA_Binary, TA_Expansion, T_AndIf, T_Annotation, T_Arithmetic, T_Array, TA_Sequence, TA_Parentesis, T_Assignment, TA_Trinary, TA_Unary, TA_Variable, T_Backgrounded, T_Backticked, T_Banged, T_BatsTest, T_BraceExpansion, T_BraceGroup, TC_And, T_CaseExpression, TC_Binary, TC_Group, TC_Nullary, T_Condition, T_CoProcBody, T_CoProc, TC_Or, TC_Unary, T_DollarArithmetic, T_DollarBraceCommandExpansion, T_DollarBraced, T_DollarBracket, T_DollarDoubleQuoted, T_DollarExpansion, T_DoubleQuoted, T_Extglob, T_FdRedirect, T_ForArithmetic, T_ForIn, T_Function, T_HereDoc, T_HereString, T_IfExpression, T_Include, T_IndexedElement, T_IoDuplicate, T_IoFile, T_NormalWord, T_OrIf, T_Pipeline, T_ProcSub, T_Redirecting, T_Script, T_SelectIn, T_SimpleCommand, T_SourceCommand, T_Subshell, T_UntilExpression, T_WhileExpression #-}

instance Eq Token where
    OuterToken _ a == OuterToken _ b = a == b

analyze :: Monad m => (Token -> m ()) -> (Token -> m ()) -> (Token -> m Token) -> Token -> m Token
analyze f g i =
    round
  where
    round t@(OuterToken id it) = do
        f t
        newIt <- traverse round it
        g t
        i (OuterToken id newIt)

getId :: Token -> Id
getId (OuterToken id _) = id

blank :: Monad m => Token -> m ()
blank = const $ return ()
doAnalysis :: Monad m => (Token -> m ()) -> Token -> m Token
doAnalysis f = analyze f blank return
doStackAnalysis :: Monad m => (Token -> m ()) -> (Token -> m ()) -> Token -> m Token
doStackAnalysis startToken endToken = analyze startToken endToken return
doTransform :: (Token -> Token) -> Token -> Token
doTransform i = runIdentity . analyze blank blank (return . i)

