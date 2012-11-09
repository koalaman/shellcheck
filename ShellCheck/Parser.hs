{-# LANGUAGE NoMonomorphismRestriction #-}

module ShellCheck.Parser (Token(..), ConditionType(..), Id(..), Note(..), Severity(..), parseShell, ParseResult(..), ParseNote(..), notesFromMap, Metadata(..), doAnalysis, doStackAnalysis, doTransform, sortNotes) where

import Text.Parsec
import Debug.Trace
import Control.Monad
import Control.Monad.Identity
import Data.Char
import Data.List (isInfixOf, isSuffixOf, partition, sortBy, intercalate, nub)
import qualified Data.Map as Map
import qualified Control.Monad.State as Ms
import Data.Maybe
import Prelude hiding (readList)
import System.IO
import Text.Parsec.Error
import qualified Text.Regex as Re
import GHC.Exts (sortWith)



backslash = char '\\'
linefeed = char '\n'
singleQuote = char '\''
doubleQuote = char '"'
variableStart = upper <|> lower <|> oneOf "_"
variableChars = upper <|> lower <|> digit <|> oneOf "_"
specialVariable = oneOf "@*#?-$!"
tokenDelimiter = oneOf "&|;<> \t\n"
quotable = oneOf "#|&;<>()$`\\ \"'\t\n"
doubleQuotable = oneOf "\"$`"
whitespace = oneOf " \t\n"
linewhitespace = oneOf " \t"

prop_spacing = isOk spacing "  \\\n # Comment"
spacing = do
    x <- many (many1 linewhitespace <|> (try $ string "\\\n"))
    optional readComment
    return $ concat x

allspacing = do
    spacing
    x <- option False ((linefeed <|> carriageReturn) >> return True)
    when x allspacing

carriageReturn = do
    parseNote ErrorC "Literal carriage return. Run script through tr -d '\\r' "
    char '\r'

--------- Message/position annotation on top of user state
data Id = Id Int deriving (Show, Eq, Ord)
data Note = Note Severity String deriving (Show, Eq)
data ParseNote = ParseNote SourcePos Severity String deriving (Show, Eq)
data Metadata = Metadata SourcePos [Note] deriving (Show)
data Severity = ErrorC | WarningC | InfoC | StyleC deriving (Show, Eq, Ord)

initialState = (Id $ -1, Map.empty, [])

getInitialMeta pos = Metadata pos []

getLastId = do
    (id, _, _) <- getState
    return id

getNextIdAt sourcepos = do
    (id, map, notes) <- getState
    let newId = incId id
    let newMap = Map.insert newId (getInitialMeta sourcepos) map
    putState (newId, newMap, notes)
    return newId
  where incId (Id n) = (Id $ n+1)

getNextId = do
    pos <- getPosition
    getNextIdAt pos

modifyMap f = do
    (id, map, parsenotes) <- getState
    putState (id, f map, parsenotes)

getMap = do
    (_, map, _) <- getState
    return map

getParseNotes = do
    (_, _, notes) <- getState
    return notes

addParseNote n = do
    (a, b, notes) <- getState
    putState (a, b, n:notes)


-- Store potential parse problems outside of parsec
parseProblem level msg = do
    pos <- getPosition
    parseProblemAt pos level msg

parseProblemAt pos level msg = do
    Ms.modify ((ParseNote pos level msg):)

-- Store non-parse problems inside
addNoteFor id note = modifyMap $ Map.adjust (\(Metadata pos notes) -> Metadata pos (note:notes)) id

addNote note = do
    id <- getLastId
    addNoteFor id note

parseNote l a = do
    pos <- getPosition
    parseNoteAt pos l a

parseNoteAt pos l a = addParseNote $ ParseNote pos l a

--------- Convenient combinators
thenSkip main follow = do
    r <- main
    optional follow
    return r

disregard x = x >> return ()

reluctantlyTill p end = do
    (lookAhead ((disregard $ try end) <|> eof) >> return []) <|> do
        x <- p
        more <- reluctantlyTill p end
        return $ x:more
      <|> return []

reluctantlyTill1 p end = do
    notFollowedBy end
    x <- p
    more <- reluctantlyTill p end
    return $ x:more

attempting rest branch = do
    ((try branch) >> rest) <|> rest

wasIncluded p = option False (p >> return True)

acceptButWarn parser level note = do
    optional $ try (do
        pos <- getPosition
        parser
        parseProblemAt pos level note
      )


readConditionContents single = do
    readCondOr `attempting` (lookAhead $ do
                                pos <- getPosition
                                choice (map (try . string) commonCommands)
                                parseProblemAt pos WarningC "To check a command, skip [] and just do 'if foo | grep bar; then'.")
                                
  where
    typ = if single then SingleBracket else DoubleBracket
    readCondBinaryOp = try $ do
        op <- choice $ (map tryOp ["-nt", "-ot", "-ef", "=", "==", "!=", "<=", ">=", "-eq", "-ne", "-lt", "-le", "-gt", "-ge", "=~", ">", "<"]) 
        hardCondSpacing
        return op
      where tryOp s = try $ do
              id <- getNextId
              string s
              return $ TC_Binary id typ s

    readCondUnaryExp = do
      op <- readCondUnaryOp
      pos <- getPosition
      (do
          arg <- readCondWord
          return $ op arg)
        <|> (do
              parseProblemAt pos ErrorC $ "Expected this to be an argument to the unary condition"
              fail "oops")
              
    readCondUnaryOp = try $ do
        op <- choice $ (map tryOp [ "-a", "-b", "-c", "-d", "-e", "-f", "-g", "-h", "-L", "-k", "-p", "-r", "-s", "-S", "-t", "-u", "-w", "-x", "-O", "-G", "-N",
                    "-z", "-n", "-o"
                    ]) 
        hardCondSpacing
        return op
      where tryOp s = try $ do
              id <- getNextId
              string s
              return $ TC_Unary id typ s

    readCondWord = do 
        notFollowedBy (try (spacing >> (string "]")))
        x <- readNormalWord
        pos <- getPosition
        if (endedWithBracket x) 
            then do
                lookAhead (try $ (many whitespace) >> (eof <|> disregard readSeparator <|> disregard (g_Then <|> g_Do)))
                parseProblemAt pos ErrorC $ "You need a space before the " ++ if single then "]" else "]]"
            else 
                softCondSpacing
        return x
      where endedWithBracket (T_NormalWord id s@(_:_)) = 
                case (last s) of T_Literal id s -> "]" `isSuffixOf` s
                                 _ -> False
            endedWithBracket _ = False

    readCondAndOp = do
        id <- getNextId
        x <- try (string "&&" <|> string "-a")
        when (single && x == "&&") $ addNoteFor id $ Note ErrorC "You can't use && inside [..]. Use [[..]] instead."
        when (not single && x == "-a") $ addNoteFor id $ Note ErrorC "In [[..]], use && instead of -a"
        softCondSpacing
        return $ TC_And id typ x


    readCondOrOp = do
        id <- getNextId
        x <- try (string "||" <|> string "-o")
        when (single && x == "||") $ addNoteFor id $ Note ErrorC "You can't use || inside [..]. Use [[..]] instead."
        when (not single && x == "-o") $ addNoteFor id $ Note ErrorC "In [[..]], use && instead of -o"
        softCondSpacing
        return $ TC_Or id typ x

    readCondNoaryOrBinary = do
      id <- getNextId
      x <- readCondWord `attempting` (do
              pos <- getPosition
              lookAhead (char '[')
              parseProblemAt pos ErrorC $ if single 
                  then "Don't use [] for grouping. Use \\( .. \\) "
                  else "Don't use [] for grouping. Use ()."
            )
      (do
            pos <- getPosition
            op <- readCondBinaryOp
            y <- readCondWord <|> ( (parseProblemAt pos ErrorC $ "Expected another argument for this operator") >> mzero)
            return (x `op` y)
          ) <|> (return $ TC_Noary id typ x)

    readCondGroup = do
          id <- getNextId
          pos <- getPosition
          lparen <- string "(" <|> string "\\(" 
          when (single && lparen == "(") $ parseProblemAt pos ErrorC "In [..] you have to escape (). Use [[..]] instead."
          when single softCondSpacing
          x <- readConditionContents single
          when single softCondSpacing
          rparen <- string ")" <|> string "\\)"
          when (single && rparen == ")") $ parseProblemAt pos ErrorC "In [..] you have to escape (). Use [[..]] instead."
          when (isEscaped lparen `xor` isEscaped rparen) $ parseProblemAt pos ErrorC "Did you just escape one half of () but not the other?"
          return $ TC_Group id typ x
      where
        isEscaped ('\\':_) = True
        isEscaped _ = False
        xor x y = x && not y || not x && y

    readCondTerm = readCondNot <|> readCondExpr
    readCondNot = do
        id <- getNextId
        char '!'
        softCondSpacing
        expr <- readCondExpr
        return $ TC_Not id typ expr

    readCondExpr = 
      readCondGroup <|> readCondUnaryExp <|> readCondNoaryOrBinary

    readCondOr = chainl1 readCondAnd readCondAndOp
    readCondAnd = chainl1 readCondTerm readCondOrOp

    commonCommands = [ "bash", "bunzip2", "busybox", "bzcat", "bzcmp", "bzdiff", "bzegrep", "bzexe", "bzfgrep", "bzgrep", "bzip2", "bzip2recover", "bzless", "bzmore", "cat", "chacl", "chgrp", "chmod", "chown", "cp", "cpio", "dash", "date", "dd", "df", "dir", "dmesg", "dnsdomainname", "domainname", "echo", "ed", "egrep", "false", "fgconsole", "fgrep", "fuser", "getfacl", "grep", "gunzip", "gzexe", "gzip", "hostname", "ip", "kill", "ksh", "ksh93", "less", "lessecho", "lessfile", "lesskey", "lesspipe", "ln", "loadkeys", "login", "ls", "lsmod", "mkdir", "mknod", "mktemp", "more", "mount", "mountpoint", "mt", "mt-gnu", "mv", "nano", "nc", "nc.traditional", "netcat", "netstat", "nisdomainname", "noshell", "pidof", "ping", "ping6", "ps", "pwd", "rbash", "readlink", "rm", "rmdir", "rnano", "run-parts", "sed", "setfacl", "sh", "sh.distrib", "sleep", "stty", "su", "sync", "tailf", "tar", "tempfile", "touch", "true", "umount", "uname", "uncompress", "vdir", "which", "ypdomainname", "zcat", "zcmp", "zdiff", "zegrep", "zfgrep", "zforce", "zgrep", "zless", "zmore", "znew" ]

readCondition = do
  opos <- getPosition
  id <- getNextId
  open <- (try $ string "[[") <|> (string "[")
  let single = open == "["
  condSpacingMsg False $ if single 
        then "You need spaces after the opening [ and before the closing ]"
        else "You need spaces after the opening [[ and before the closing ]]"
  condition <- readConditionContents single
  cpos <- getPosition
  close <- (try $ string "]]") <|> (string "]")
  when (open == "[[" && close /= "]]") $ parseProblemAt cpos ErrorC "Did you mean ]] ?"
  when (open == "[" && close /= "]" ) $ parseProblemAt opos ErrorC "Did you mean [[ ?"
  return $ T_Condition id (if single then SingleBracket else DoubleBracket) condition

 
hardCondSpacing = condSpacingMsg False "You need a space here"
softCondSpacing = condSpacingMsg True "You need a space here"
condSpacingMsg soft msg = do
  pos <- getPosition
  space <- spacing
  when (null space) $ (if soft then parseNoteAt else parseProblemAt) pos ErrorC msg

-- Horrifying AST
data Token = T_AND_IF Id | T_OR_IF Id | T_DSEMI Id | T_Semi Id | T_DLESS Id | T_DGREAT Id | T_LESSAND Id | T_GREATAND Id | T_LESSGREAT Id | T_DLESSDASH Id | T_CLOBBER Id | T_If Id | T_Then Id | T_Else Id | T_Elif Id | T_Fi Id | T_Do Id | T_Done Id | T_Case Id | T_Esac Id | T_While Id | T_Until Id | T_For Id | T_Lbrace Id | T_Rbrace Id | T_Lparen Id | T_Rparen Id | T_Bang Id | T_In  Id | T_NEWLINE Id | T_EOF Id | T_Less Id | T_Greater Id | T_SingleQuoted Id String | T_Literal Id String | T_NormalWord Id [Token] | T_DoubleQuoted Id [Token] | T_DollarExpansion Id [Token] | T_DollarBraced Id String | T_DollarArithmetic Id String | T_BraceExpansion Id String | T_IoFile Id Token Token | T_HereDoc Id Bool Bool String | T_HereString Id Token | T_FdRedirect Id String Token | T_Assignment Id String Token | T_Array Id [Token] | T_Redirecting Id [Token] Token | T_SimpleCommand Id [Token] [Token] | T_Pipeline Id [Token] | T_Banged Id Token | T_AndIf Id (Token) (Token) | T_OrIf Id (Token) (Token) | T_Backgrounded Id Token | T_IfExpression Id [([Token],[Token])] [Token] | T_Subshell Id [Token] | T_BraceGroup Id [Token] | T_WhileExpression Id [Token] [Token] | T_UntilExpression Id [Token] [Token] | T_ForIn Id String [Token] [Token] | T_CaseExpression Id Token [([Token],[Token])] | T_Function Id String Token | T_Arithmetic Id String | T_Script Id [Token] |
  T_Condition Id ConditionType Token | TC_And Id ConditionType String Token Token | TC_Or Id ConditionType String Token Token | TC_Not Id ConditionType Token | TC_Group Id ConditionType Token | TC_Binary Id ConditionType String Token Token | TC_Unary Id ConditionType String Token | TC_Noary Id ConditionType Token
    deriving (Show)

data ConditionType = DoubleBracket | SingleBracket deriving (Show, Eq)


analyzeScopes f g i = mapM (analyze f g i)
analyze f g i s@(T_NormalWord id list) = do
    f s
    a <- analyzeScopes f g i list
    g s
    return . i $ T_NormalWord id a

analyze f g i s@(T_DoubleQuoted id list) = do
    f s
    a <- analyzeScopes f g i list
    g s
    return . i $ T_DoubleQuoted id a

analyze f g i s@(T_DollarExpansion id l) = do
    f s
    nl <- mapM (analyze f g i) l
    g s
    return . i $ T_DollarExpansion id nl

analyze f g i s@(T_IoFile id op file) = do
    f s
    a <- analyze f g i op
    b <- analyze f g i file
    g s
    return . i $ T_IoFile id a b

analyze f g i s@(T_HereString id word) = do
    f s
    a <- analyze f g i word
    g s
    return . i $ T_HereString id a

analyze f g i s@(T_FdRedirect id v t) = do
    f s
    a <- analyze f g i t
    g s
    return . i $ T_FdRedirect id v a

analyze f g i s@(T_Assignment id v t) = do
    f s
    a <- analyze f g i t
    g s
    return . i $ T_Assignment id v a

analyze f g i s@(T_Array id t) = do
    f s
    a <- analyzeScopes f g i t
    g s
    return . i $ T_Array id a

analyze f g i s@(T_Redirecting id redirs cmd) = do
    f s
    newRedirs <- analyzeScopes f g i redirs
    newCmd <- analyze f g i $ cmd
    g s
    return . i $ (T_Redirecting id newRedirs newCmd)

analyze f g i s@(T_SimpleCommand id vars cmds) = do
    f s
    a <- analyzeScopes f g i vars
    b <- analyzeScopes f g i cmds
    g s
    return . i $ T_SimpleCommand id a b

analyze f g i s@(T_Pipeline id l) = do
    f s
    a <- analyzeScopes f g i l
    g s
    return . i $ T_Pipeline id a

analyze f g i s@(T_Banged id l) = do
    f s
    a <- analyze f g i l
    g s
    return . i $ T_Banged id a

analyze f g i s@(T_AndIf id t u) = do
    f s
    a <- analyze f g i t
    b <- analyze f g i u
    g s
    return . i $ T_AndIf id a b

analyze f g i s@(T_OrIf id t u) = do
    f s
    a <- analyze f g i t
    b <- analyze f g i u
    g s
    return . i $ T_OrIf id a b

analyze f g i s@(T_Backgrounded id l) = do
    f s
    a <- analyze f g i l
    g s
    return . i $ T_Backgrounded id a

analyze f g i s@(T_IfExpression id conditions elses) = do
    f s
    newConds <- mapM (\(c, t) -> do
                x <- mapM (analyze f g i) c
                y <- mapM (analyze f g i) t
                return (x, y)
             ) conditions
    newElses <- mapM (analyze f g i) elses
    g s
    return . i $ T_IfExpression id newConds newElses

analyze f g i s@(T_Subshell id l) = do
    f s
    a <- mapM (analyze f g i) l
    g s
    return . i $ T_Subshell id a

analyze f g i s@(T_BraceGroup id l) = do
    f s
    a <- mapM (analyze f g i) l
    g s
    return . i $ T_BraceGroup id a

analyze f g i s@(T_WhileExpression id c l) = do
    f s
    a <- mapM (analyze f g i) c
    b <- mapM (analyze f g i) l
    g s
    return . i $ T_WhileExpression id a b

analyze f g i s@(T_UntilExpression id c l) = do
    f s
    a <- mapM (analyze f g i) c
    b <- mapM (analyze f g i) l
    g s
    return . i $ T_UntilExpression id a b

analyze f g i s@(T_ForIn id v w l) = do
    f s
    a <- mapM (analyze f g i) w
    b <- mapM (analyze f g i) l
    g s
    return . i $ T_ForIn id v a b

analyze f g i s@(T_CaseExpression id word cases) = do
    f s
    newWord <- analyze f g i word
    newCases <- mapM (\(c, t) -> do
                x <- mapM (analyze f g i) c
                y <- mapM (analyze f g i) t
                return (x, y)
             ) cases
    g s
    return . i $ T_CaseExpression id newWord newCases

analyze f g i s@(T_Script id l) = do
    f s
    a <- mapM (analyze f g i) l
    g s
    return . i $ T_Script id a

analyze f g i s@(T_Function id name body) = do
    f s
    a <- analyze f g i body
    g s
    return . i $ T_Function id name a

analyze f g i s@(T_Condition id typ token) = do
  f s
  a <- analyze f g i token
  g s
  return . i $ T_Condition id typ a

analyze f g i s@(TC_And id typ str t1 t2) = do
  f s
  a <- analyze f g i t1
  b <- analyze f g i t2
  g s
  return . i $ TC_And id typ str a b

analyze f g i s@(TC_Or id typ str t1 t2) = do
  f s
  a <- analyze f g i t1
  b <- analyze f g i t2
  g s
  return . i $ TC_Or id typ str a b

analyze f g i s@(TC_Group id typ token) = do
  f s
  a <- analyze f g i token
  g s
  return . i $ TC_Group id typ a

analyze f g i s@(TC_Binary id typ op lhs rhs) = do
  f s
  a <- analyze f g i lhs
  b <- analyze f g i rhs
  g s
  return . i $ TC_Binary id typ op a b

analyze f g i s@(TC_Unary id typ op token) = do
  f s
  a <- analyze f g i token
  g s
  return . i $ TC_Unary id typ op a

analyze f g i s@(TC_Noary id typ token) = do
  f s
  a <- analyze f g i token
  g s
  return . i $ TC_Noary id typ a

analyze f g i t = do
    f t
    g t
    return . i $ t


blank = const $ return ()
doAnalysis f t = analyze f blank id t
doStackAnalysis startToken endToken t = analyze startToken endToken id t
doTransform i t = runIdentity $ analyze blank blank i t


lolHax s = Re.subRegex (Re.mkRegex "(Id [0-9]+)") (show s) "(Id 0)"
instance Eq Token where
    (==) a b = (lolHax a) == (lolHax b)

readComment = do
    char '#'
    anyChar `reluctantlyTill` linefeed

prop_readNormalWord = isOk readNormalWord "'foo'\"bar\"{1..3}baz$(lol)"
readNormalWord = do
    id <- getNextId
    x <- many1 readNormalWordPart
    return $ T_NormalWord id x

readNormalWordPart = readSingleQuoted <|> readDoubleQuoted <|> readDollar <|> readBraced <|> readBackTicked <|> (readNormalLiteral)

prop_readSingleQuoted = isOk readSingleQuoted "'foo bar'"
prop_readSingleQuoted2 = isWarning readSingleQuoted "'foo bar\\'"
readSingleQuoted = do
    id <- getNextId
    singleQuote
    s <- readSingleQuotedPart `reluctantlyTill` singleQuote
    pos <- getPosition
    singleQuote <?> "End single quoted string"

    let string = concat s
    return (T_SingleQuoted id string) `attempting` do
        x <- lookAhead anyChar
        when (isAlpha x && isAlpha (last string)) $ parseProblemAt pos WarningC "This apostrophe terminated the single quoted string!"

readSingleQuotedLiteral = do
    singleQuote
    strs <- many1 readSingleQuotedPart
    singleQuote
    return $ concat strs

readSingleQuotedPart =
    readSingleEscaped
    <|> anyChar `reluctantlyTill1` (singleQuote <|> backslash)

prop_readBackTicked = isWarning readBackTicked "`ls *.mp3`"
readBackTicked = do
    id <- getNextId
    parseNote InfoC "Ignoring deprecated `..` backtick expansion.  Use $(..) instead."
    pos <- getPosition
    char '`'
    f <- readGenericLiteral (char '`')
    char '`' `attempting` (eof >> parseProblemAt pos ErrorC "Can't find terminating backtick for this one")
    return $ T_Literal id f


prop_readDoubleQuoted = isOk readDoubleQuoted "\"Hello $FOO\""
readDoubleQuoted = do
    id <- getNextId
    doubleQuote
    x <- many doubleQuotedPart
    doubleQuote <?> "End double quoted"
    return $ T_DoubleQuoted id x

doubleQuotedPart = readDoubleLiteral <|> readDollar <|> readBackTicked

readDoubleQuotedLiteral = do
    doubleQuote
    x <- readDoubleLiteral
    doubleQuote
    return x

readDoubleLiteral = do
    id <- getNextId
    s <- many1 readDoubleLiteralPart
    return $ T_Literal id (concat s)

readDoubleLiteralPart = do
    x <- (readDoubleEscaped <|> (anyChar >>= \x -> return [x])) `reluctantlyTill1` doubleQuotable
    return $ concat x

prop_readNormalLiteral = isOk readNormalLiteral "hello\\ world"
readNormalLiteral = do
    id <- getNextId
    s <- many1 readNormalLiteralPart
    return $ T_Literal id (concat s)

readNormalLiteralPart = do
    readNormalEscaped <|> (anyChar `reluctantlyTill1` quotable)

readNormalEscaped = do
    pos <- getPosition
    backslash
    do
        next <- (quotable <|> oneOf "?*[]")
        return $ if next == '\n' then "" else [next]
      <|>
        do
            next <- anyChar <?> "No character after \\"
            parseNoteAt pos WarningC $ "Did you mean \"$(printf \"\\" ++ [next] ++ "\")\"? The shell just ignores the \\ here."
            return [next]

readSingleEscaped = do
    s <- backslash
    let attempt level p msg = do { try $ parseNote level msg; x <- p; return [s,x]; }

    do {
        x <- lookAhead singleQuote;
        parseProblem InfoC "Are you trying to escape that single quote? echo 'You'\\''re doing it wrong'.";
        return [s];
    }
        <|> attempt InfoC linefeed "You don't break lines with \\ in single quotes, it results in literal backslash-linefeed."
        <|> do
            x <- anyChar
            return [s,x]


readDoubleEscaped = do
    bs <- backslash
    (linefeed >> return "")
        <|> (doubleQuotable >>= return . return)
        <|> (anyChar >>= (return . \x -> [bs, x]))


readGenericLiteral endExp = do
    strings <- many (readGenericEscaped <|> anyChar `reluctantlyTill1` endExp)
    return $ concat strings

readGenericLiteral1 endExp = do
    strings <- many1 (readGenericEscaped <|> anyChar `reluctantlyTill1` endExp)
    return $ concat strings

readGenericEscaped = do
    backslash
    x <- anyChar
    return $ if x == '\n' then [] else [x]

prop_readBraced = isOk readBraced "{1..4}"
prop_readBraced2 = isOk readBraced "{foo,bar,\"baz lol\"}"
readBraced = try $ do
    let strip (T_Literal _ s) = return ("\"" ++ s ++ "\"")
    id <- getNextId
    char '{'
    str <- many1 ((readDoubleQuotedLiteral >>= (strip)) <|> readGenericLiteral1 (oneOf "}\"" <|> whitespace))
    char '}'
    return $ T_BraceExpansion id $ concat str

readDollar = readDollarArithmetic <|> readDollarBraced <|> readDollarExpansion <|> readDollarVariable <|> readDollarLonely


readParenLiteralHack = do
    strs <- (readParenHack <|> (anyChar >>= \x -> return [x])) `reluctantlyTill1` (string "))")
    return $ concat strs

readParenHack = do
    char '('
    x <- (readParenHack <|> (anyChar >>= (\x -> return [x]))) `reluctantlyTill` (oneOf ")")
    char ')'
    return $ "(" ++ (concat x) ++ ")"

prop_readDollarArithmetic = isOk readDollarArithmetic "$(( 3 * 4 +5))"
prop_readDollarArithmetic2 = isOk readDollarArithmetic "$(((3*4)+(1*2+(3-1))))"
readDollarArithmetic = do
    id <- getNextId
    try (string "$((")
    -- TODO
    str <- readParenLiteralHack
    string "))"
    return (T_DollarArithmetic id str)

readArithmeticExpression = do
    id <- getNextId
    try (string "((")
    -- TODO
    str <- readParenLiteralHack
    string "))"
    return (T_Arithmetic id str)

prop_readDollarBraced = isOk readDollarBraced "${foo//bar/baz}"
readDollarBraced = do
    id <- getNextId
    try (string "${")
    -- TODO
    str <- readGenericLiteral (char '}')
    char '}' <?> "matching }"
    return $ (T_DollarBraced id str)

prop_readDollarExpansion = isOk readDollarExpansion "$(echo foo; ls\n)"
readDollarExpansion = do
    id <- getNextId
    try (string "$(")
    cmds <- readCompoundList
    char ')'
    return $ (T_DollarExpansion id cmds)

prop_readDollarVariable = isOk readDollarVariable "$@"
readDollarVariable = do
    id <- getNextId
    let singleCharred p = do
        n <- p
        return (T_DollarBraced id [n]) `attempting` do
            pos <- getPosition
            num <- lookAhead $ many1 p
            parseNoteAt pos ErrorC $ "$" ++ (n:num) ++ " is equivalent to ${" ++ [n] ++ "}"++ num

    let positional = singleCharred digit
    let special = singleCharred specialVariable

    let regular = do
        name <- readVariableName
        return $ T_DollarBraced id (name)

    try $ char '$' >> (positional <|> special <|> regular)

readVariableName = do
    f <- variableStart
    rest <- many variableChars
    return (f:rest)

readDollarLonely = do
    id <- getNextId
    char '$'
    n <- lookAhead (anyChar <|> (eof >> return '_'))
    when (n /= '\'') $ parseNote StyleC "$ is not used specially and should therefore be escaped"
    return $ T_Literal id "$"

prop_readHereDoc = isOk readHereDoc "<< foo\nlol\ncow\nfoo"
prop_readHereDoc2 = isWarning readHereDoc "<<- EOF\n  cow\n  EOF"
readHereDoc = do
    let stripLiteral (T_Literal _ x) = x
        stripLiteral (T_SingleQuoted _ x) = x
    fid <- getNextId
    try $ string "<<"
    dashed <- (char '-' >> return True) <|> return False
    tokenPosition <- getPosition
    spacing
    hid <- getNextId
    (quoted, endToken) <- (readNormalLiteral >>= (\x -> return (False, stripLiteral x)) )
                            <|> (readDoubleQuotedLiteral >>= return . (\x -> (True, stripLiteral x)))
                            <|> (readSingleQuotedLiteral >>= return . (\x -> (True, x)))
    spacing

    hereInfo <- anyChar `reluctantlyTill` (linefeed >> spacing >> (string endToken) >> (disregard whitespace <|> eof))

    do
        linefeed
        spaces <- spacing
        verifyHereDoc dashed quoted spaces hereInfo
        token <- string endToken
        return $ T_FdRedirect fid "" $ T_HereDoc hid dashed quoted hereInfo
     `attempting` (eof >> debugHereDoc tokenPosition endToken hereInfo)

verifyHereDoc dashed quoted spacing hereInfo = do
    when (not dashed && spacing /= "") $ parseNote ErrorC "Use <<- instead of << if you want to indent the end token"
    when (dashed && filter (/= '\t') spacing /= "" ) $ parseNote ErrorC "When using <<-, you can only indent with tabs"
    return ()

debugHereDoc pos endToken doc =
    if endToken `isInfixOf` doc
        then parseProblemAt pos ErrorC ("Found " ++ endToken ++ " further down, but not by itself at the start of the line")
        else if (map toLower endToken) `isInfixOf` (map toLower doc)
            then parseProblemAt pos ErrorC ("Found " ++ endToken ++ " further down, but with wrong casing.")
            else parseProblemAt pos ErrorC ("Couldn't find end token `" ++ endToken ++ "' in the here document ")


readFilename = readNormalWord
readIoFileOp = choice [g_LESSAND, g_GREATAND, g_DGREAT, g_LESSGREAT, g_CLOBBER, tryToken "<" T_Less, tryToken ">" T_Greater ]

prop_readIoFile = isOk readIoFile ">> \"$(date +%YYmmDD)\""
readIoFile = do
    id <- getNextId
    op <- readIoFileOp
    spacing
    file <- readFilename
    return $ T_FdRedirect id "" $ T_IoFile id op file

readIoNumber = try $ do
    x <- many1 digit
    lookAhead readIoFileOp
    return x

prop_readIoNumberRedirect = isOk readIoNumberRedirect "3>&2"
prop_readIoNumberRedirect2 = isOk readIoNumberRedirect "2> lol"
prop_readIoNumberRedirect3 = isOk readIoNumberRedirect "4>&-"
readIoNumberRedirect = do
    id <- getNextId
    n <- readIoNumber
    op <- readHereString <|> readHereDoc <|> readIoFile
    let actualOp = case op of T_FdRedirect _ "" x -> x
    spacing
    return $ T_FdRedirect id n actualOp

readIoRedirect = choice [ readIoNumberRedirect, readHereString, readHereDoc, readIoFile ] `thenSkip` spacing

readRedirectList = many1 readIoRedirect

prop_readHereString = isOk readHereString "<<< \"Hello $world\""
readHereString = do
    id <- getNextId
    try $ string "<<<"
    spacing
    id2 <- getNextId
    word <- readNormalWord
    return $ T_FdRedirect id "" $ T_HereString id2 word

readNewlineList = many1 ((newline <|> carriageReturn) `thenSkip` spacing)
readLineBreak = optional readNewlineList

prop_roflol = isWarning readScript "a &; b"
prop_roflol2 = isOk readScript "a & b"
readSeparatorOp = do
    notFollowedBy (g_AND_IF <|> g_DSEMI)
    f <- (try $ do
                    char '&'
                    spacing
                    pos <- getPosition
                    char ';'
                    parseProblemAt pos ErrorC "It's not 'foo &; bar', just 'foo & bar'. "
                    return '&'
            ) <|> char ';' <|> char '&'
    spacing
    return f

readSequentialSep = (disregard $ g_Semi >> readLineBreak) <|> (disregard readNewlineList)
readSeparator =
    do
        separator <- readSeparatorOp
        readLineBreak
        return separator
     <|>
        do
            readNewlineList
            return '\n'

makeSimpleCommand id tokens =
    let (assignment, rest) = partition (\x -> case x of T_Assignment _ _ _ -> True; _ -> False) tokens
    in let (redirections, rest2) = partition (\x -> case x of T_FdRedirect _ _ _ -> True; _ -> False) rest
       in T_Redirecting id redirections $ T_SimpleCommand id assignment rest2

prop_readSimpleCommand = isOk readSimpleCommand "echo test > file"
readSimpleCommand = do
    id <- getNextId
    prefix <- option [] readCmdPrefix
    cmd <- option [] $ do { f <- readCmdName; return [f]; }
    when (null prefix && null cmd) $ fail "No command"
    if null cmd
        then return $ makeSimpleCommand id prefix
        else do
            suffix <- option [] readCmdSuffix
            return $ makeSimpleCommand id (prefix ++ cmd ++ suffix)

prop_readPipeline = isOk readPipeline "! cat /etc/issue | grep -i ubuntu"
readPipeline = do
    notFollowedBy $ try readKeyword
    do
        (T_Bang id) <- g_Bang `thenSkip` spacing
        pipe <- readPipeSequence
        return $ T_Banged id pipe
      <|> do
        readPipeSequence

prop_readAndOr = isOk readAndOr "grep -i lol foo || exit 1"
readAndOr = chainr1 readPipeline $ do
    op <- g_AND_IF <|> g_OR_IF
    readLineBreak
    return $ case op of T_AND_IF id -> T_AndIf id
                        T_OR_IF  id -> T_OrIf id

readTerm = do
    allspacing
    m <- readAndOr
    readTerm' m

readTerm' current =
    do
        id <- getNextId
        sep <- readSeparator
        more <- (option (T_EOF id)$ readAndOr)
        case more of (T_EOF _) -> return [transformWithSeparator id sep current]
                     _         -> do
                                list <- readTerm' more
                                return $ (transformWithSeparator id sep current : list)
      <|>
        return [current]

transformWithSeparator i '&' = T_Backgrounded i
transformWithSeparator i _  = id


readPipeSequence = do
    id <- getNextId
    list <- readCommand `sepBy1` (readPipe `thenSkip` (spacing >> readLineBreak))
    spacing
    return $ T_Pipeline id list

readPipe = do
    notFollowedBy g_OR_IF
    char '|' `thenSkip` spacing

readCommand = (readCompoundCommand <|> readSimpleCommand)

readCmdName = do
    f <- readNormalWord
    spacing
    return f

readCmdWord = do
    f <- readNormalWord
    spacing
    return f

prop_readIfClause = isOk readIfClause "if false; then foo; elif true; then stuff; more stuff; else cows; fi"
prop_readIfClause2 = isWarning readIfClause "if false; then; echo oo; fi"
prop_readIfClause3 = isWarning readIfClause "if false; then true; else; echo lol; fi"
readIfClause = do
    id <- getNextId
    (condition, action) <- readIfPart
    elifs <- many readElifPart
    elses <- option [] readElsePart
    g_Fi
    return $ T_IfExpression id ((condition, action):elifs) elses

readIfPart = do
    g_If
    allspacing
    condition <- readTerm
    g_Then
    acceptButWarn g_Semi ErrorC "No semicolons directly after 'then'"
    allspacing
    action <- readTerm
    return (condition, action)

readElifPart = do
    g_Elif
    allspacing
    condition <- readTerm
    g_Then
    acceptButWarn g_Semi ErrorC "No semicolons directly after 'then'"
    allspacing
    action <- readTerm
    return (condition, action)

readElsePart = do
    g_Else
    acceptButWarn g_Semi ErrorC "No semicolons directly after 'else'"
    allspacing
    readTerm

prop_readSubshell = isOk readSubshell "( cd /foo; tar cf stuff.tar * )"
readSubshell = do
    id <- getNextId
    char '('
    allspacing
    list <- readCompoundList
    allspacing
    char ')'
    return $ T_Subshell id list

prop_readBraceGroup = isOk readBraceGroup "{ a; b | c | d; e; }"
readBraceGroup = do
    id <- getNextId
    char '{'
    allspacing
    list <- readTerm
    allspacing
    char '}'
    return $ T_BraceGroup id list

prop_readWhileClause = isOk readWhileClause "while [[ -e foo ]]; do sleep 1; done"
readWhileClause = do
    (T_While id) <- g_While
    pos <- getPosition
    condition <- readTerm
    return () `attempting` (do
                                eof
                                parseProblemAt pos ErrorC "Condition missing 'do'. Did you forget it or the ; or \\n before i?"
            )
    statements <- readDoGroup
    return $ T_WhileExpression id condition statements

prop_readUntilClause = isOk readUntilClause "until kill -0 $PID; do sleep 1; done"
readUntilClause = do
    (T_Until id) <- g_Until
    condition <- readTerm
    statements <- readDoGroup
    return $ T_UntilExpression id condition statements

readDoGroup = do
    pos <- getPosition
    g_Do
    allspacing
    (eof >> return []) <|>
        do
            commands <- readCompoundList
            disregard g_Done <|> (do
                eof
                case hasFinal "done" commands of
                    Nothing -> parseProblemAt pos ErrorC "Couldn't find a 'done' for this 'do'"
                    Just (id) -> addNoteFor id $ Note ErrorC "Put a ; or \\n before the done"
                )
            return commands
          <|> do
            parseProblemAt pos ErrorC "Can't find the 'done' for this 'do'"
            fail "No done"

hasFinal s [] = Nothing
hasFinal s f = 
    case last f of 
        T_Pipeline _ m@(_:_) ->
            case last m of 
                T_Redirecting _ [] (T_SimpleCommand _ _ m@(_:_)) -> 
                    case last m of 
                        T_NormalWord _ [T_Literal id str] -> 
                            if str == s then Just id else Nothing
                        _ -> Nothing
                _ -> Nothing
        _ -> Nothing
        

prop_readForClause = isOk readForClause "for f in *; do rm \"$f\"; done"
prop_readForClause3 = isOk readForClause "for f; do foo; done"
readForClause = do
    (T_For id) <- g_For
    spacing
    name <- readVariableName
    spacing
    values <- readInClause <|> (readSequentialSep >> return [])
    group <- readDoGroup <|> (
                allspacing >> 
                eof >>
                parseProblem ErrorC "Missing 'do'" >>
                return []) 
    return $ T_ForIn id name values group

readInClause = do
    g_In
    things <- (readCmdWord) `reluctantlyTill`
                (disregard (g_Semi) <|> disregard linefeed <|> disregard g_Do)

    do {
        lookAhead (g_Do);
        parseNote ErrorC "You need a line feed or semicolon before the 'do'";
    } <|> do {
        optional $ g_Semi;
        disregard allspacing;
    }

    return things

prop_readCaseClause = isOk readCaseClause "case foo in a ) lol; cow;; b|d) fooo; esac"
readCaseClause = do
    id <- getNextId
    g_Case
    word <- readNormalWord
    spacing
    g_In
    readLineBreak
    list <- readCaseList
    g_Esac
    return $ T_CaseExpression id word list

readCaseList = many readCaseItem

readCaseItem = do
    notFollowedBy g_Esac
    optional g_Lparen
    spacing
    pattern <- readPattern
    g_Rparen
    readLineBreak
    list <- ((lookAhead g_DSEMI >> return []) <|> readCompoundList)
    (g_DSEMI <|> lookAhead (readLineBreak >> g_Esac))
    readLineBreak
    return (pattern, list)

prop_readFunctionDefinition = isOk readFunctionDefinition "foo() { command foo --lol \"$@\"; }"
prop_readFunctionDefinition2 = isWarning readFunctionDefinition "function foo() { command foo --lol \"$@\"; }"
readFunctionDefinition = do
    id <- getNextId
    name <- try readFunctionSignature
    allspacing
    (disregard (lookAhead g_Lbrace) <|> parseProblem ErrorC "Expected a { to open the function definition")
    group <- readBraceGroup
    return $ T_Function id name group


readFunctionSignature = do
    acceptButWarn (string "function" >> linewhitespace >> spacing) InfoC "Drop the keyword 'function'. It's optional in Bash but invalid in other shells."
    name <- readVariableName
    spacing
    g_Lparen
    g_Rparen
    return name


readPattern = (readNormalWord `thenSkip` spacing) `sepBy1` (char '|' `thenSkip` spacing)


readCompoundCommand = do
    id <- getNextId
    cmd <- choice [ readBraceGroup, readArithmeticExpression, readSubshell, readCondition, readWhileClause, readUntilClause, readIfClause, readForClause, readCaseClause, readFunctionDefinition]
    spacing
    redirs <- many readIoRedirect
    return $ T_Redirecting id redirs $ cmd


readCompoundList = readTerm

readCmdPrefix = many1 (readIoRedirect <|> readAssignmentWord)
readCmdSuffix = many1 (readIoRedirect <|> readCmdWord)

prop_readAssignmentWord = isOk readAssignmentWord "a=42"
prop_readAssignmentWord2 = isOk readAssignmentWord "b=(1 2 3)"
prop_readAssignmentWord3 = isWarning readAssignmentWord "$b = 13"
prop_readAssignmentWord4 = isWarning readAssignmentWord "b = $(lol)"
readAssignmentWord = try $ do
    id <- getNextId
    optional (char '$' >> parseNote ErrorC "Don't use $ on the left side of assignments")
    variable <- readVariableName
    space <- spacing
    pos <- getPosition
    char '='
    space2 <- spacing
    value <- readArray <|> readNormalWord
    spacing
    when (space ++ space2 /= "") $ parseNoteAt pos ErrorC "Don't put spaces around the = in assignments"
    return $ T_Assignment id variable value

readArray = do
    id <- getNextId
    char '('
    allspacing
    words <- (readNormalWord `thenSkip` allspacing) `reluctantlyTill` (char ')')
    char ')'
    return $ T_Array id words


tryToken s t = try $ do
    id <- getNextId
    string s
    spacing
    return $ t id

tryWordToken s t = tryParseWordToken (string s) t `thenSkip` spacing
tryParseWordToken parser t = try $ do
    id <- getNextId
    parser
    try $ lookAhead (keywordSeparator)
    return $ t id

g_AND_IF = tryToken "&&" T_AND_IF
g_OR_IF = tryToken "||" T_OR_IF
g_DSEMI = tryToken ";;" T_DSEMI
g_DLESS = tryToken "<<" T_DLESS
g_DGREAT = tryToken ">>" T_DGREAT
g_LESSAND = tryToken "<&" T_LESSAND
g_GREATAND = tryToken ">&" T_GREATAND
g_LESSGREAT = tryToken "<>" T_LESSGREAT
g_DLESSDASH = tryToken "<<-" T_DLESSDASH
g_CLOBBER = tryToken ">|" T_CLOBBER
g_OPERATOR = g_AND_IF <|> g_OR_IF <|> g_DSEMI <|> g_DLESSDASH <|> g_DLESS <|> g_DGREAT <|> g_LESSAND <|> g_GREATAND <|> g_LESSGREAT

g_If = tryWordToken "if" T_If
g_Then = tryWordToken "then" T_Then
g_Else = tryWordToken "else" T_Else
g_Elif = tryWordToken "elif" T_Elif
g_Fi = tryWordToken "fi" T_Fi
g_Do = tryWordToken "do" T_Do
g_Done = tryWordToken "done" T_Done
g_Case = tryWordToken "case" T_Case
g_Esac = tryWordToken "esac" T_Esac
g_While = tryWordToken "while" T_While
g_Until = tryWordToken "until" T_Until
g_For = tryWordToken "for" T_For
g_In = tryWordToken "in" T_In
g_Lbrace = tryWordToken "{" T_Lbrace
g_Rbrace = tryWordToken "}" T_Rbrace

g_Lparen = tryToken "(" T_Lparen
g_Rparen = tryToken ")" T_Rparen
g_Bang = tryToken "!" T_Bang

g_Semi = do
    notFollowedBy g_DSEMI
    tryToken ";" T_Semi

keywordSeparator = eof <|> disregard whitespace <|> (disregard $ oneOf ";()")

readKeyword = choice [ g_Then, g_Else, g_Elif, g_Fi, g_Do, g_Done, g_Esac, g_Rbrace, g_Rparen, g_DSEMI ]

ifParse p t f = do
    (lookAhead (try p) >> t) <|> f

wtf = do
    x <- many anyChar
    parseProblem ErrorC x

readScript = do
    id <- getNextId
    do {
        allspacing;
        commands <- readTerm;
        eof <|> (parseProblem ErrorC "Parsing stopped here because of parsing errors.");
        return $ T_Script id commands;
    } <|> do {
        parseProblem WarningC "Couldn't read any commands";
        return $ T_Script id $ [T_EOF id];
    }

rp p filename contents = Ms.runState (runParserT p initialState filename contents) []

isWarning :: (ParsecT String (Id, Map.Map Id Metadata, [ParseNote]) (Ms.State [ParseNote]) t) -> String -> Bool
isWarning p s = (fst cs) && (not . null . snd $ cs) where cs = checkString p s

isOk :: (ParsecT String (Id, Map.Map Id Metadata, [ParseNote]) (Ms.State [ParseNote]) t) -> String -> Bool
isOk p s = (fst cs) && (null . snd $ cs) where cs = checkString p s

checkString parser string =
    case rp (parser >> eof >> getState) "-" string of
        (Right (tree, map, notes), problems) -> (True, (notesFromMap map) ++ notes ++ problems)
        (Left _, n) -> (False, n)

parseWithNotes parser = do
    item <- parser
    map <- getMap
    parseNotes <- getParseNotes
    return (item, map, nub . sortNotes $ parseNotes)

toParseNotes (Metadata pos list) = map (\(Note level note) -> ParseNote pos level note) list
notesFromMap map = Map.fold (\x -> (++) (toParseNotes x)) [] map

getAllNotes result = (concatMap (notesFromMap . snd) (maybeToList . parseResult $ result)) ++ (parseNotes result)

compareNotes (ParseNote pos1 level1 s1) (ParseNote pos2 level2 s2) = compare (pos1, level1) (pos2, level2)
sortNotes = sortBy compareNotes


data ParseResult = ParseResult { parseResult :: Maybe (Token, Map.Map Id Metadata), parseNotes :: [ParseNote] } deriving (Show)

makeErrorFor parsecError =
    ParseNote (errorPos parsecError) ErrorC $ getStringFromParsec $ errorMessages parsecError

getStringFromParsec errors =
        case map snd $ sortWith fst $ map f errors of
            (s:_) -> s
            _ -> "Unknown error"
    where f err =
            case err of
                UnExpect s    -> (1, unexpected s)
                SysUnExpect s -> (2, unexpected s)
                Expect s      -> (3, "Expected " ++ s ++ "")
                Message s     -> (4, "Message: " ++ s)
          wut "" = "eof"
          wut x = x
          unexpected s = "Aborting due to unexpected " ++ (wut s) ++ ". Is this valid?"

parseShell filename contents = do
    case rp (parseWithNotes readScript) filename contents of
        (Right (script, map, notes), parsenotes) -> ParseResult (Just (script, map)) (nub $ sortNotes $ notes ++ parsenotes)
        (Left err, p) -> ParseResult Nothing (nub $ sortNotes $ p ++ ([makeErrorFor err]))

