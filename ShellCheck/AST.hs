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
module ShellCheck.AST where

import Control.Monad.Identity
import Text.Parsec
import qualified ShellCheck.Regex as Re
import Prelude hiding (id)

newtype Id = Id Int deriving (Show, Eq, Ord)

data Quoted = Quoted | Unquoted deriving (Show, Eq)
data Dashed = Dashed | Undashed deriving (Show, Eq)
data AssignmentMode = Assign | Append deriving (Show, Eq)
newtype FunctionKeyword = FunctionKeyword Bool deriving (Show, Eq)
newtype FunctionParentheses = FunctionParentheses Bool deriving (Show, Eq)
data CaseType = CaseBreak | CaseFallThrough | CaseContinue deriving (Show, Eq)

newtype Root = Root Token
data Token =
    TA_Binary Id String Token Token
    | TA_Assignment Id String Token Token
    | TA_Expansion Id [Token]
    | TA_Index Id Token
    | TA_Sequence Id [Token]
    | TA_Trinary Id Token Token Token
    | TA_Unary Id String Token
    | TC_And Id ConditionType String Token Token
    | TC_Binary Id ConditionType String Token Token
    | TC_Group Id ConditionType Token
    | TC_Nullary Id ConditionType Token
    | TC_Or Id ConditionType String Token Token
    | TC_Unary Id ConditionType String Token
    | TC_Empty Id ConditionType
    | T_AND_IF Id
    | T_AndIf Id Token Token
    | T_Arithmetic Id Token
    | T_Array Id [Token]
    | T_IndexedElement Id [Token] Token
    -- Store the index as string, and parse as arithmetic or string later
    | T_UnparsedIndex Id SourcePos String
    | T_Assignment Id AssignmentMode String [Token] Token
    | T_Backgrounded Id Token
    | T_Backticked Id [Token]
    | T_Bang Id
    | T_Banged Id Token
    | T_BraceExpansion Id [Token]
    | T_BraceGroup Id [Token]
    | T_CLOBBER Id
    | T_Case Id
    | T_CaseExpression Id Token [(CaseType, [Token], [Token])]
    | T_Condition Id ConditionType Token
    | T_DGREAT Id
    | T_DLESS Id
    | T_DLESSDASH Id
    | T_DSEMI Id
    | T_Do Id
    | T_DollarArithmetic Id Token
    | T_DollarBraced Id Token
    | T_DollarBracket Id Token
    | T_DollarDoubleQuoted Id [Token]
    | T_DollarExpansion Id [Token]
    | T_DollarSingleQuoted Id String
    | T_DollarBraceCommandExpansion Id [Token]
    | T_Done Id
    | T_DoubleQuoted Id [Token]
    | T_EOF Id
    | T_Elif Id
    | T_Else Id
    | T_Esac Id
    | T_Extglob Id String [Token]
    | T_FdRedirect Id String Token
    | T_Fi Id
    | T_For Id
    | T_ForArithmetic Id Token Token Token [Token]
    | T_ForIn Id String [Token] [Token]
    | T_Function Id FunctionKeyword FunctionParentheses String Token
    | T_GREATAND Id
    | T_Glob Id String
    | T_Greater Id
    | T_HereDoc Id Dashed Quoted String [Token]
    | T_HereString Id Token
    | T_If Id
    | T_IfExpression Id [([Token],[Token])] [Token]
    | T_In  Id
    | T_IoFile Id Token Token
    | T_IoDuplicate Id Token String
    | T_LESSAND Id
    | T_LESSGREAT Id
    | T_Lbrace Id
    | T_Less Id
    | T_Literal Id String
    | T_Lparen Id
    | T_NEWLINE Id
    | T_NormalWord Id [Token]
    | T_OR_IF Id
    | T_OrIf Id Token Token
    | T_ParamSubSpecialChar Id String -- e.g. '%' in ${foo%bar}  or '/' in ${foo/bar/baz}
    | T_Pipeline Id [Token] [Token] -- [Pipe separators] [Commands]
    | T_ProcSub Id String [Token]
    | T_Rbrace Id
    | T_Redirecting Id [Token] Token
    | T_Rparen Id
    | T_Script Id String [Token]
    | T_Select Id
    | T_SelectIn Id String [Token] [Token]
    | T_Semi Id
    | T_SimpleCommand Id [Token] [Token]
    | T_SingleQuoted Id String
    | T_Subshell Id [Token]
    | T_Then Id
    | T_Until Id
    | T_UntilExpression Id [Token] [Token]
    | T_While Id
    | T_WhileExpression Id [Token] [Token]
    | T_Annotation Id [Annotation] Token
    | T_Pipe Id String
    | T_CoProc Id (Maybe String) Token
    | T_CoProcBody Id Token
    | T_Include Id Token Token -- . & source: SimpleCommand T_Script
    deriving (Show)

data Annotation =
    DisableComment Integer
    | SourceOverride String
    | ShellOverride String
    deriving (Show, Eq)
data ConditionType = DoubleBracket | SingleBracket deriving (Show, Eq)

-- This is an abomination.
tokenEquals :: Token -> Token -> Bool
tokenEquals a b = kludge a == kludge b
    where kludge s = Re.subRegex (Re.mkRegex "\\(Id [0-9]+\\)") (show s) "(Id 0)"

instance Eq Token where
    (==) = tokenEquals

analyze :: Monad m => (Token -> m ()) -> (Token -> m ()) -> (Token -> m Token) -> Token -> m Token
analyze f g i =
    round
  where
    round t = do
        f t
        newT <- delve t
        g t
        i newT
    roundAll = mapM round

    dl l v = do
        x <- roundAll l
        return $ v x
    dll l m v = do
        x <- roundAll l
        y <- roundAll m
        return $ v x y
    d1 t v = do
        x <- round t
        return $ v x
    d2 t1 t2 v = do
        x <- round t1
        y <- round t2
        return $ v x y

    delve (T_NormalWord id list) = dl list $ T_NormalWord id
    delve (T_DoubleQuoted id list) = dl list $ T_DoubleQuoted id
    delve (T_DollarDoubleQuoted id list) = dl list $ T_DollarDoubleQuoted id
    delve (T_DollarExpansion id list) = dl list $ T_DollarExpansion id
    delve (T_DollarBraceCommandExpansion id list) = dl list $ T_DollarBraceCommandExpansion id
    delve (T_BraceExpansion id list) = dl list $ T_BraceExpansion id
    delve (T_Backticked id list) = dl list $ T_Backticked id
    delve (T_DollarArithmetic id c) = d1 c $ T_DollarArithmetic id
    delve (T_DollarBracket id c) = d1 c $ T_DollarBracket id
    delve (T_IoFile id op file) = d2 op file $ T_IoFile id
    delve (T_IoDuplicate id op num) = d1 op $ \x -> T_IoDuplicate id x num
    delve (T_HereString id word) = d1 word $ T_HereString id
    delve (T_FdRedirect id v t) = d1 t $ T_FdRedirect id v
    delve (T_Assignment id mode var indices value) = do
        a <- roundAll indices
        b <- round value
        return $ T_Assignment id mode var a b
    delve (T_Array id t) = dl t $ T_Array id
    delve (T_IndexedElement id indices t) = do
        a <- roundAll indices
        b <- round t
        return $ T_IndexedElement id a b
    delve (T_Redirecting id redirs cmd) = do
        a <- roundAll redirs
        b <- round cmd
        return $ T_Redirecting id a b
    delve (T_SimpleCommand id vars cmds) = dll vars cmds $ T_SimpleCommand id
    delve (T_Pipeline id l1 l2) = dll l1 l2 $ T_Pipeline id
    delve (T_Banged id l) = d1 l $ T_Banged id
    delve (T_AndIf id t u) = d2 t u $ T_AndIf id
    delve (T_OrIf id t u) = d2 t u $ T_OrIf id
    delve (T_Backgrounded id l) = d1 l $ T_Backgrounded id
    delve (T_Subshell id l) = dl l $ T_Subshell id
    delve (T_ProcSub id typ l) = dl l $ T_ProcSub id typ
    delve (T_Arithmetic id c) = d1 c $ T_Arithmetic id
    delve (T_IfExpression id conditions elses) = do
        newConds <- mapM (\(c, t) -> do
                            x <- mapM round c
                            y <- mapM round t
                            return (x,y)
                    ) conditions
        newElses <- roundAll elses
        return $ T_IfExpression id newConds newElses
    delve (T_BraceGroup id l) = dl l $ T_BraceGroup id
    delve (T_WhileExpression id c l) = dll c l $ T_WhileExpression id
    delve (T_UntilExpression id c l) = dll c l $ T_UntilExpression id
    delve (T_ForIn id v w l) = dll w l $ T_ForIn id v
    delve (T_SelectIn id v w l) = dll w l $ T_SelectIn id v
    delve (T_CaseExpression id word cases) = do
        newWord <- round word
        newCases <- mapM (\(o, c, t) -> do
                            x <- mapM round c
                            y <- mapM round t
                            return (o, x,y)
                        ) cases
        return $ T_CaseExpression id newWord newCases

    delve (T_ForArithmetic id a b c group) = do
        x <- round a
        y <- round b
        z <- round c
        list <- mapM round group
        return $ T_ForArithmetic id x y z list

    delve (T_Script id s l) = dl l $ T_Script id s
    delve (T_Function id a b name body) = d1 body $ T_Function id a b name
    delve (T_Condition id typ token) = d1 token $ T_Condition id typ
    delve (T_Extglob id str l) = dl l $ T_Extglob id str
    delve (T_DollarBraced id op) = d1 op $ T_DollarBraced id
    delve (T_HereDoc id d q str l) = dl l $ T_HereDoc id d q str

    delve (TC_And id typ str t1 t2) = d2 t1 t2 $ TC_And id typ str
    delve (TC_Or id typ str t1 t2) = d2 t1 t2 $ TC_Or id typ str
    delve (TC_Group id typ token) = d1 token $ TC_Group id typ
    delve (TC_Binary id typ op lhs rhs) = d2 lhs rhs $ TC_Binary id typ op
    delve (TC_Unary id typ op token) = d1 token $ TC_Unary id typ op
    delve (TC_Nullary id typ token) = d1 token $ TC_Nullary id typ

    delve (TA_Binary id op t1 t2) = d2 t1 t2 $ TA_Binary id op
    delve (TA_Assignment id op t1 t2) = d2 t1 t2 $ TA_Assignment id op
    delve (TA_Unary id op t1) = d1 t1 $ TA_Unary id op
    delve (TA_Sequence id l) = dl l $ TA_Sequence id
    delve (TA_Trinary id t1 t2 t3) = do
        a <- round t1
        b <- round t2
        c <- round t3
        return $ TA_Trinary id a b c
    delve (TA_Expansion id t) = dl t $ TA_Expansion id
    delve (TA_Index id t) = d1 t $ TA_Index id
    delve (T_Annotation id anns t) = d1 t $ T_Annotation id anns
    delve (T_CoProc id var body) = d1 body $ T_CoProc id var
    delve (T_CoProcBody id t) = d1 t $ T_CoProcBody id
    delve (T_Include id includer script) = d2 includer script $ T_Include id
    delve t = return t

getId :: Token -> Id
getId t = case t of
        T_AND_IF id  -> id
        T_OR_IF id  -> id
        T_DSEMI id  -> id
        T_Semi id  -> id
        T_DLESS id  -> id
        T_DGREAT id  -> id
        T_LESSAND id  -> id
        T_GREATAND id  -> id
        T_LESSGREAT id  -> id
        T_DLESSDASH id  -> id
        T_CLOBBER id  -> id
        T_If id  -> id
        T_Then id  -> id
        T_Else id  -> id
        T_Elif id  -> id
        T_Fi id  -> id
        T_Do id  -> id
        T_Done id  -> id
        T_Case id  -> id
        T_Esac id  -> id
        T_While id  -> id
        T_Until id  -> id
        T_For id  -> id
        T_Select id  -> id
        T_Lbrace id  -> id
        T_Rbrace id  -> id
        T_Lparen id  -> id
        T_Rparen id  -> id
        T_Bang id  -> id
        T_In  id  -> id
        T_NEWLINE id  -> id
        T_EOF id  -> id
        T_Less id  -> id
        T_Greater id  -> id
        T_SingleQuoted id _  -> id
        T_Literal id _  -> id
        T_NormalWord id _  -> id
        T_DoubleQuoted id _  -> id
        T_DollarExpansion id _  -> id
        T_DollarBraced id _  -> id
        T_DollarArithmetic id _  -> id
        T_BraceExpansion id _  -> id
        T_ParamSubSpecialChar id _ -> id
        T_DollarBraceCommandExpansion id _  -> id
        T_IoFile id _ _  -> id
        T_IoDuplicate id _ _  -> id
        T_HereDoc id _ _ _ _ -> id
        T_HereString id _  -> id
        T_FdRedirect id _ _  -> id
        T_Assignment id _ _ _ _  -> id
        T_Array id _  -> id
        T_IndexedElement id _ _  -> id
        T_Redirecting id _ _  -> id
        T_SimpleCommand id _ _  -> id
        T_Pipeline id _ _  -> id
        T_Banged id _  -> id
        T_AndIf id _ _ -> id
        T_OrIf id _ _ -> id
        T_Backgrounded id _  -> id
        T_IfExpression id _ _  -> id
        T_Subshell id _  -> id
        T_BraceGroup id _  -> id
        T_WhileExpression id _ _  -> id
        T_UntilExpression id _ _  -> id
        T_ForIn id _ _ _  -> id
        T_SelectIn id _ _ _  -> id
        T_CaseExpression id _ _ -> id
        T_Function id _ _ _ _  -> id
        T_Arithmetic id _  -> id
        T_Script id _ _  -> id
        T_Condition id _ _  -> id
        T_Extglob id _ _ -> id
        T_Backticked id _ -> id
        TC_And id _ _ _ _  -> id
        TC_Or id _ _ _ _  -> id
        TC_Group id _ _  -> id
        TC_Binary id _ _ _ _  -> id
        TC_Unary id _ _ _  -> id
        TC_Nullary id _ _  -> id
        TA_Binary id _ _ _  -> id
        TA_Assignment id _ _ _  -> id
        TA_Unary id _ _  -> id
        TA_Sequence id _  -> id
        TA_Trinary id _ _ _  -> id
        TA_Expansion id _  -> id
        TA_Index id _  -> id
        T_ProcSub id _ _ -> id
        T_Glob id _ -> id
        T_ForArithmetic id _ _ _ _ -> id
        T_DollarSingleQuoted id _ -> id
        T_DollarDoubleQuoted id _ -> id
        T_DollarBracket id _ -> id
        T_Annotation id _ _ -> id
        T_Pipe id _ -> id
        T_CoProc id _ _ -> id
        T_CoProcBody id _ -> id
        T_Include id _ _ -> id
        T_UnparsedIndex id _ _ -> id
        TC_Empty id _ -> id

blank :: Monad m => Token -> m ()
blank = const $ return ()
doAnalysis :: Monad m => (Token -> m ()) -> Token -> m Token
doAnalysis f = analyze f blank return
doStackAnalysis :: Monad m => (Token -> m ()) -> (Token -> m ()) -> Token -> m Token
doStackAnalysis startToken endToken = analyze startToken endToken return
doTransform :: (Token -> Token) -> Token -> Token
doTransform i = runIdentity . analyze blank blank (return . i)

