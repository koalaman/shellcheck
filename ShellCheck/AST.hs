{-
    This file is part of ShellCheck.
    http://www.vidarholen.net/contents/shellcheck

    ShellCheck is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    ShellCheck is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
module ShellCheck.AST where

import Control.Monad
import Control.Monad.Identity
import qualified Text.Regex as Re

data Id = Id Int deriving (Show, Eq, Ord)

data Token = T_AND_IF Id | T_OR_IF Id | T_DSEMI Id | T_Semi Id | T_DLESS Id | T_DGREAT Id | T_LESSAND Id | T_GREATAND Id | T_LESSGREAT Id | T_DLESSDASH Id | T_CLOBBER Id | T_If Id | T_Then Id | T_Else Id | T_Elif Id | T_Fi Id | T_Do Id | T_Done Id | T_Case Id | T_Esac Id | T_While Id | T_Until Id | T_For Id | T_Lbrace Id | T_Rbrace Id | T_Lparen Id | T_Rparen Id | T_Bang Id | T_In  Id | T_NEWLINE Id | T_EOF Id | T_Less Id | T_Greater Id | T_SingleQuoted Id String | T_Literal Id String | T_NormalWord Id [Token] | T_DoubleQuoted Id [Token] | T_DollarExpansion Id [Token] | T_DollarBraced Id Token | T_DollarArithmetic Id Token | T_BraceExpansion Id String | T_IoFile Id Token Token | T_HereDoc Id Bool Bool String | T_HereString Id Token | T_FdRedirect Id String Token | T_Assignment Id String Token | T_Array Id [Token] | T_Redirecting Id [Token] Token | T_SimpleCommand Id [Token] [Token] | T_Pipeline Id [Token] | T_Banged Id Token | T_AndIf Id (Token) (Token) | T_OrIf Id (Token) (Token) | T_Backgrounded Id Token | T_IfExpression Id [([Token],[Token])] [Token] | T_Subshell Id [Token] | T_BraceGroup Id [Token] | T_WhileExpression Id [Token] [Token] | T_UntilExpression Id [Token] [Token] | T_ForIn Id String [Token] [Token] | T_CaseExpression Id Token [([Token],[Token])] | T_Function Id String Token | T_Arithmetic Id Token | T_Script Id String [Token] | T_Condition Id ConditionType Token | T_Extglob Id String [Token] | TC_And Id ConditionType String Token Token | TC_Or Id ConditionType String Token Token | TC_Group Id ConditionType Token | TC_Binary Id ConditionType String Token Token | TC_Unary Id ConditionType String Token | TC_Noary Id ConditionType Token | TA_Binary Id String Token Token | TA_Unary Id String Token | TA_Sequence Id [Token] | TA_Variable Id String | TA_Trinary Id Token Token Token | TA_Expansion Id Token | TA_Literal Id String | T_Backticked Id String | T_ProcSub Id String [Token] | T_Glob Id String | T_ForArithmetic Id Token Token Token [Token]

    deriving (Show)

data ConditionType = DoubleBracket | SingleBracket deriving (Show, Eq)

-- I apologize for nothing!
lolHax s = Re.subRegex (Re.mkRegex "(Id [0-9]+)") (show s) "(Id 0)"
instance Eq Token where
    (==) a b = (lolHax a) == (lolHax b)


analyze :: Monad m => (Token -> m ()) -> (Token -> m ()) -> (Token -> Token) -> Token -> m Token
analyze f g i t =
    round t
  where
    round t = do
        f t
        newT <- delve t
        g t
        return . i $ newT
    roundAll = mapM round

    dl l v = do
        x <- roundAll l
        return $ v x
    dll l m v = do
        x <- roundAll l
        y <- roundAll m
        return $ v x m
    d1 t v = do
        x <- round t
        return $ v x
    d2 t1 t2 v = do
        x <- round t1
        y <- round t2
        return $ v x y

    delve (T_NormalWord id list) = dl list $ T_NormalWord id
    delve (T_DoubleQuoted id list) = dl list $ T_DoubleQuoted id
    delve (T_DollarExpansion id list) = dl list $ T_DollarExpansion id
    delve (T_DollarArithmetic id c) = d1 c $ T_DollarArithmetic id
    delve (T_IoFile id op file) = d2 op file $ T_IoFile id
    delve (T_HereString id word) = d1 word $ T_HereString id
    delve (T_FdRedirect id v t) = d1 t $ T_FdRedirect id v
    delve (T_Assignment id v t) = d1 t $ T_Assignment id v
    delve (T_Array id t) = dl t $ T_Array id
    delve (T_Redirecting id redirs cmd) = do
        a <- roundAll redirs
        b <- round cmd
        return $ T_Redirecting id a b
    delve (T_SimpleCommand id vars cmds) = dll vars cmds $ T_SimpleCommand id
    delve (T_Pipeline id l) = dl l $ T_Pipeline id
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
    delve (T_CaseExpression id word cases) = do
        newWord <- round word
        newCases <- mapM (\(c, t) -> do
                            x <- mapM round c
                            y <- mapM round t
                            return (x,y)
                        ) cases
        return $ T_CaseExpression id newWord newCases

    delve (T_ForArithmetic id a b c group) = do
        x <- round a
        y <- round b
        z <- round c
        list <- mapM round group
        return $ T_ForArithmetic id x y z list

    delve (T_Script id s l) = dl l $ T_Script id s
    delve (T_Function id name body) = d1 body $ T_Function id name
    delve (T_Condition id typ token) = d1 token $ T_Condition id typ
    delve (T_Extglob id str l) = dl l $ T_Extglob id str
    delve (T_DollarBraced id op) = d1 op $ T_DollarBraced id

    delve (TC_And id typ str t1 t2) = d2 t1 t2 $ TC_And id typ str
    delve (TC_Or id typ str t1 t2) = d2 t1 t2 $ TC_Or id typ str
    delve (TC_Group id typ token) = d1 token $ TC_Group id typ
    delve (TC_Binary id typ op lhs rhs) = d2 lhs rhs $ TC_Binary id typ op
    delve (TC_Unary id typ op token) = d1 token $ TC_Unary id typ op
    delve (TC_Noary id typ token) = d1 token $ TC_Noary id typ

    delve (TA_Binary id op t1 t2) = d2 t1 t2 $ TA_Binary id op
    delve (TA_Unary id op t1) = d1 t1 $ TA_Unary id op
    delve (TA_Sequence id l) = dl l $ TA_Sequence id
    delve (TA_Trinary id t1 t2 t3) = do
        a <- round t1
        b <- round t2
        c <- round t3
        return $ TA_Trinary id a b c
    delve (TA_Expansion id t) = d1 t $ TA_Expansion id
    delve t = return t

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
        T_IoFile id _ _  -> id
        T_HereDoc id _ _ _ -> id
        T_HereString id _  -> id
        T_FdRedirect id _ _  -> id
        T_Assignment id _ _  -> id
        T_Array id _  -> id
        T_Redirecting id _ _  -> id
        T_SimpleCommand id _ _  -> id
        T_Pipeline id _  -> id
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
        T_CaseExpression id _ _ -> id
        T_Function id _ _  -> id
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
        TC_Noary id _ _  -> id
        TA_Binary id _ _ _  -> id
        TA_Unary id _ _  -> id
        TA_Sequence id _  -> id
        TA_Variable id _  -> id
        TA_Trinary id _ _ _  -> id
        TA_Expansion id _  -> id
        TA_Literal id _ -> id
        T_ProcSub id _ _ -> id
        T_Glob id _ -> id
        T_ForArithmetic id _ _ _ _ -> id

blank :: Monad m => Token -> m ()
blank = const $ return ()
doAnalysis f t = analyze f blank id t
doStackAnalysis startToken endToken t = analyze startToken endToken id t
doTransform i t = runIdentity $ analyze blank blank i t

