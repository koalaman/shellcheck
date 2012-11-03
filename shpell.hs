{-# LANGUAGE NoMonomorphismRestriction #-}

-- Shpell Check, by Vidar 'koala_man' Holen
-- Sorry about the code. It was a week's worth of hacking.

import Text.Parsec
import Text.Parsec.Pos (initialPos)
import Debug.Trace
import Control.Monad
import Data.Char
import Data.List (isInfixOf, partition, sortBy, intercalate)
import qualified Control.Monad.State as Ms
import Data.Maybe
import Prelude hiding (readList)
import System.IO



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
data Annotated a = Annotated SourcePos [Note] a deriving (Show, Eq)
data Note = ParseNote SourcePos Severity String | Note Severity String deriving (Show, Eq)
data MessageStack = StackNode Note MessageStack | StackMark String SourcePos MessageStack | StackEmpty
data ParseProblem = ParseProblem SourcePos Severity String deriving (Show, Eq)
data OutputNote = OutputNote SourcePos Severity String deriving (Show, Eq)
data Severity = ErrorC | WarningC | InfoC | StyleC deriving (Show, Eq, Ord)

instance Functor Annotated where
    fmap f (Annotated p n a) = Annotated p n (f a)

markStack msg = do
    pos <- getPosition
    modifyState (StackMark msg pos)

getMessages r (StackMark _ _ s) = (r, s)
getMessages r (StackNode n s) = getMessages (n:r) s
popStack = do
    f <- getState
    let (notes, stack) = getMessages [] f
    putState stack
    return notes

-- Store potential parse problems outside of parsec
parseProblem level msg = do
    pos <- getPosition
    parseProblemAt pos level msg

parseProblemAt pos level msg = do
    Ms.modify ((ParseProblem pos level msg):)

pushNote n = modifyState (StackNode n)

parseNote l a = do
    pos <- getPosition
    parseNoteAt pos l a

parseNoteAt pos l a = pushNote $ ParseNote pos l a


annotated msg parser = do
    pos <- getPosition
    markStack msg
    result <- parser
    messages <- popStack
    return $ Annotated pos messages result

dropAnnotation (Annotated _ _ s) = s
blankAnnotation pos t = Annotated pos [] t

merge (Annotated pos messages result) = do
    mapM pushNote messages
    return result

merging p = p >>= merge

getOutputNotes (Annotated p notes _) = map (makeOutputNote p) notes

makeOutputNote _ (ParseNote p l s) = OutputNote p l s
makeOutputNote p (Note l s) = OutputNote p l s

--------- Convenient combinators

thenSkip main follow = do
    r <- main
    optional follow
    return r

disregard x = x >> return ()

reluctantlyTill p end = do -- parse p until end <|> eof matches ahead
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

-- Horrifying AST
data Token = T_AND_IF | T_OR_IF | T_DSEMI | T_Semi | T_DLESS | T_DGREAT | T_LESSAND | T_GREATAND | T_LESSGREAT | T_DLESSDASH | T_CLOBBER | T_If | T_Then | T_Else | T_Elif | T_Fi | T_Do | T_Done | T_Case | T_Esac | T_While | T_Until | T_For | T_Lbrace | T_Rbrace | T_Lparen | T_Rparen | T_Bang | T_In  | T_NEWLINE | T_EOF | T_Less | T_Greater | T_SingleQuoted String | T_Literal String | T_NormalWord [Annotated Token] | T_DoubleQuoted [Annotated Token] | T_DollarExpansion [Token] | T_DollarBraced String | T_DollarVariable String | T_DollarArithmetic String | T_BraceExpansion String | T_IoFile Token Token | T_HereDoc Bool Bool String | T_HereString Token | T_FdRedirect String Token | T_Assignment String Token | T_Redirecting [Annotated Token] Token | T_SimpleCommand [Annotated Token] [Annotated Token] | T_Pipeline [Annotated Token] | T_Banged Token | T_AndIf (Annotated Token) (Annotated Token) | T_OrIf (Annotated Token) (Annotated Token) | T_Backgrounded Token | T_IfExpression [([Token],[Token])] [Token] | T_Subshell [Token] | T_BraceGroup [Token] | T_WhileExpression [Token] [Token] | T_UntilExpression [Token] [Token] | T_ForIn String [Token] [Token] | T_CaseExpression Token [([Token],[Token])] |T_Function String Token | T_Command (Annotated Token) | T_Script [Token]
    deriving (Show)

extractNotes' list = modifyFlag ((++) $ concatMap getOutputNotes list) >> return ()
extractNotes (T_NormalWord list) = extractNotes' list
extractNotes (T_DoubleQuoted list) = extractNotes' list
extractNotes (T_Redirecting list f) = extractNotes' list
extractNotes (T_Pipeline list) = extractNotes' list
extractNotes (T_Command list) = extractNotes' [list]
extractNotes (T_SimpleCommand list1 list2) = do
    extractNotes' list1
    extractNotes' list2
extractNotes t = return ()


postMessage level s = Ms.modify $ \(x, l) -> (x, Note level s : l)
warn s = postMessage WarningC s
inform s = postMessage InfoC s
style s = postMessage StyleC s


putFlag v = modifyFlag (const v) >> return ()
getFlag = modifyFlag id
modifyFlag f = do
    Ms.modify $ \(x, l) -> (f x, l)
    v <- Ms.get
    return $ fst v


analyzeScopes f i = mapM (analyzeScope f i)
analyzeScope f i (Annotated pos notes t) = do
    v <- getFlag
    let (ret, (flag, list)) = Ms.runState (analyze f i t) (v, [])
    putFlag flag
    return $ Annotated pos (notes++list) ret

analyze f i s@(T_NormalWord list) = do
    f s
    a <- analyzeScopes f i list
    return . i $ T_NormalWord a

analyze f i s@(T_DoubleQuoted list) = do
    f s
    a <- analyzeScopes f i list
    return . i $ T_DoubleQuoted a

analyze f i s@(T_DollarExpansion l) = do
    f s
    nl <- mapM (analyze f i) l
    return . i $ T_DollarExpansion nl

analyze f i s@(T_IoFile op file) = do
    f s
    a <- analyze f i op
    b <- analyze f i file
    return . i $ T_IoFile a b

analyze f i s@(T_HereString word) = do
    f s
    a <- analyze f i word
    return . i $ T_HereString a

analyze f i s@(T_FdRedirect v t) = do
    f s
    a <- analyze f i t
    return . i $ T_FdRedirect v a

analyze f i s@(T_Assignment v t) = do
    f s
    a <- analyze f i t
    return . i $ T_Assignment v a

analyze f i s@(T_Redirecting redirs cmd) = do
    f s
    newRedirs <- analyzeScopes f i redirs
    newCmd <- analyze f i $ cmd
    return . i $ (T_Redirecting newRedirs newCmd)

analyze f i s@(T_SimpleCommand vars cmds) = do
    f s
    a <- analyzeScopes f i vars
    b <- analyzeScopes f i cmds
    return . i $ T_SimpleCommand a b

analyze f i s@(T_Pipeline l) = do
    f s
    a <- analyzeScopes f i l
    return . i $ T_Pipeline a

analyze f i s@(T_Banged l) = do
    f s
    a <- analyze f i l
    return . i $ T_Banged a

analyze f i s@(T_AndIf t u) = do
    f s
    a <- analyzeScope f i t
    b <- analyzeScope f i u
    return . i $ T_AndIf a b

analyze f i s@(T_OrIf t u) = do
    f s
    a <- analyzeScope f i t
    b <- analyzeScope f i u
    return . i $ T_OrIf a b

analyze f i s@(T_Backgrounded l) = do
    f s
    a <- analyze f i l
    return . i $ T_Backgrounded a

analyze f i s@(T_IfExpression conditions elses) = do
    f s
    newConds <- mapM (\(c, t) -> do
                x <- mapM (analyze f i) c
                y <- mapM (analyze f i) t
                return (x, y)
             ) conditions
    newElses <- mapM (analyze f i) elses
    return . i $ T_IfExpression newConds newElses

analyze f i s@(T_Subshell l) = do
    f s
    a <- mapM (analyze f i) l
    return . i $ T_Subshell a

analyze f i s@(T_BraceGroup l) = do
    f s
    a <- mapM (analyze f i) l
    return . i $ T_BraceGroup a

analyze f i s@(T_WhileExpression c l) = do
    f s
    a <- mapM (analyze f i) c
    b <- mapM (analyze f i) l
    return . i $ T_WhileExpression a b

analyze f i s@(T_UntilExpression c l) = do
    f s
    a <- mapM (analyze f i) c
    b <- mapM (analyze f i) l
    return . i $ T_UntilExpression a b

analyze f i s@(T_ForIn v w l) = do
    f s
    a <- mapM (analyze f i) w
    b <- mapM (analyze f i) l
    return . i $ T_ForIn v a b

analyze f i s@(T_CaseExpression word cases) = do
    f s
    newWord <- analyze f i word
    newCases <- mapM (\(c, t) -> do
                x <- mapM (analyze f i) c
                y <- mapM (analyze f i) t
                return (x, y)
             ) cases
    return . i $ T_CaseExpression newWord newCases

analyze f i s@(T_Script l) = do
    f s
    a <- mapM (analyze f i) l
    return . i $ T_Script a

analyze f i s@(T_Function name body) = do
    f s
    a <- analyze f i body
    return . i $ T_Function name a

analyze f i s@(T_Command c) = do
    f s
    a <- analyzeScope f i c
    return . i $ T_Command a

analyze f i t = do
    f t
    return . i $ t

doAnalysis f t = fst $ Ms.runState (analyze f id t) ((), [])
explore f d t = fst . snd $ Ms.runState (analyze f id t) (d, [])
transform i t = fst $ Ms.runState (analyze (const $ return ()) i t) ((), [])

findNotes t = explore extractNotes [] t
sortNotes l = sortBy compareNotes l
compareNotes (OutputNote pos1 level1 _) (OutputNote pos2 level2 _) = compare (pos1, level1) (pos2, level2)
findParseNotes l = map (\(ParseProblem p level s) -> OutputNote p level s) l
--   T_UntilExpression [Token] [Token] | T_ForIn String [Token] [Token]

getNotes s =
    case rp readScript s of
        (Right x, p) -> sortNotes $ (findNotes $ doAllAnalysis x) ++ (findParseNotes p)
        (Left _, p) -> sortNotes $ (OutputNote (initialPos "-") ErrorC "Parsing failed"):(findParseNotes p)

readComment = do
    char '#'
    anyChar `reluctantlyTill` linefeed

readNormalWord = do
    x <- many1 readNormalWordPart
    return $ T_NormalWord x

readNormalWordPart = readSingleQuoted <|> readDoubleQuoted <|> readDollar <|> readBraced <|> readBackTicked <|> (annotated "normal literal" $ readNormalLiteral)

readSingleQuoted = annotated "single quoted string" $ do
    singleQuote
    s <- readSingleQuotedPart `reluctantlyTill` singleQuote
    singleQuote <?> "End single quoted string"

    let string = concat s
    return (T_SingleQuoted string) `attempting` do
        x <- lookAhead anyChar
        when (isAlpha x && isAlpha (last string)) $ parseProblem WarningC "This apostrophe terminated the single quoted string."

readSingleQuotedLiteral = do
    singleQuote
    strs <- many1 readSingleQuotedPart
    singleQuote
    return $ concat strs

readSingleQuotedPart =
    readSingleEscaped
    <|> anyChar `reluctantlyTill1` (singleQuote <|> backslash)

readBackTicked = annotated "backtick expansion" $ do
    parseNote StyleC "`..` style expansion is deprecated, use $(..) instead if you want my help"
    pos <- getPosition
    char '`'
    f <- readGenericLiteral (char '`')
    char '`' `attempting` (eof >> parseProblemAt pos ErrorC "Can't find terminating backtick for this one")
    return $ T_Literal f


readDoubleQuoted = annotated "double quoted string" $ do
    doubleQuote
    x <- many doubleQuotedPart
    doubleQuote <?> "End double quoted"
    return $ T_DoubleQuoted x

doubleQuotedPart = readDoubleLiteral <|> readDollar <|> readBackTicked

readDoubleQuotedLiteral = do
    doubleQuote
    x <- readDoubleLiteral
    doubleQuote
    return $ dropAnnotation x

readDoubleLiteral = annotated "double literal" $ do
    s <- many1 readDoubleLiteralPart
    return $ T_Literal (concat s)

readDoubleLiteralPart = do
    x <- (readDoubleEscaped <|> (anyChar >>= \x -> return [x])) `reluctantlyTill1` doubleQuotable
    return $ concat x

readNormalLiteral = do
    s <- many1 readNormalLiteralPart
    return $ T_Literal (concat s)

readNormalLiteralPart = do
    readNormalEscaped <|> (anyChar `reluctantlyTill1` quotable)

readNormalEscaped = do
    backslash
    pos <- getPosition
    do
        next <- (quotable <|> oneOf "?*[]")
        return $ if next == '\n' then "" else [next]
      <|>
        do
            next <- anyChar <?> "No character after \\"
            parseNoteAt pos WarningC $ "This character doesn't need escaping here, the \\ is ignored"
            return [next]

readSingleEscaped = do
    s <- backslash
    let attempt level p msg = do { try $ parseNote level msg; x <- p; return [s,x]; }

    do {
        x <- singleQuote;
        parseProblem InfoC "Are you trying to escape a single quote? echo 'You'\\''re doing it wrong'.";
        return [s,x];
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

readBraced = annotated "{1,2..3} expression" $ try $ do
    let strip (T_Literal s) = return ("\"" ++ s ++ "\"")
    char '{'
    str <- many1 ((readDoubleQuotedLiteral >>= (strip )) <|> readGenericLiteral1 (oneOf "}" <|> whitespace))
    char '}'
    return $ T_BraceExpansion $ concat str

readDollar = readDollarArithmetic <|> readDollarBraced <|> readDollarExpansion <|> readDollarVariable <|> readDollarLonely


readParenLiteralHack = do
    strs <- ((anyChar >>= \x -> return [x]) <|> readParenHack) `reluctantlyTill1` (string "))")
    return $ concat strs

readParenHack = do
    char '('
    x <- many anyChar
    char ')'
    return $ "(" ++ x ++ ")"

readDollarArithmetic = annotated "$(( )) expression" $ do
    try (string "$((")
    -- TODO
    str <- readParenLiteralHack
    string "))"
    return (T_DollarArithmetic str)

readDollarBraced = annotated "${ } expression" $ do
    try (string "${")
    -- TODO
    str <- readGenericLiteral (char '}')
    char '}' <?> "matching }"
    return $ (T_DollarBraced str)

readDollarExpansion = annotated "$( )" $ do
    try (string "$(")
    cmds <- readCompoundList
    char ')'
    return $ (T_DollarExpansion cmds)

readDollarVariable = annotated "$variable" $ do
    let singleCharred p = do
        n <- p
        return (T_DollarVariable [n]) `attempting` do
            pos <- getPosition
            num <- lookAhead $ many1 p
            parseNoteAt pos ErrorC $ "$" ++ (n:num) ++ " is equivalent to ${" ++ [n] ++ "}"++ num

    let positional = singleCharred digit
    let special = singleCharred specialVariable

    let regular = do
        name <- readVariableName
        return $ T_DollarVariable (name)

    char '$'
    positional <|> special <|> regular

readVariableName = do
    f <- variableStart
    rest <- many variableChars
    return (f:rest)

readDollarLonely = annotated "lonely $" $ do
    parseNote ErrorC "$ is not used specially and should therefore be escaped"
    char '$'
    return $ T_Literal "$"

readHereDoc = annotated "here document" $ do
    let stripLiteral (T_Literal x) = x
        stripLiteral (T_SingleQuoted x) = x
    try $ string "<<"
    dashed <- (char '-' >> return True) <|> return False
    tokenPosition <- getPosition
    spacing
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
        return $ T_FdRedirect "" $ T_HereDoc dashed quoted hereInfo
     `attempting` (eof >> debugHereDoc tokenPosition endToken hereInfo)

verifyHereDoc dashed quoted spacing hereInfo = do
    when (not dashed && spacing /= "") $ parseNote ErrorC "When using << instead of <<-, the end tokens can't be indented"
    when (dashed && filter (/= '\t') spacing /= "" ) $ parseNote ErrorC "When using <<-, you can only indent with tabs"
    return ()

debugHereDoc pos endToken doc =
    if endToken `isInfixOf` doc
        then parseProblemAt pos ErrorC (endToken ++ " was part of the here document, but not by itself at the start of the line")
        else if (map toLower endToken) `isInfixOf` (map toLower doc)
            then parseProblemAt pos ErrorC (endToken ++ " appears in the here document, but with different case")
            else parseProblemAt pos ErrorC ("Couldn't find end token `" ++ endToken ++ "' in the here document ")


readFilename = readNormalWord
readIoFileOp = choice [g_LESSAND, g_GREATAND, g_DGREAT, g_LESSGREAT, g_CLOBBER, string "<" >> return T_Less, string ">" >> return T_Greater ]
readIoFile = do
    op <- readIoFileOp
    spacing
    file <- readFilename
    return $ T_FdRedirect "" $ T_IoFile op file
readIoNumber = try $ do
    x <- many1 digit
    lookAhead readIoFileOp
    return x
readIoNumberRedirect = annotated "fd io redirect" $ do
    n <- readIoNumber
    op <- merging readHereString <|> merging readHereDoc <|> readIoFile
    let actualOp = case op of T_FdRedirect "" x -> x
    spacing
    return $ T_FdRedirect n actualOp

readIoRedirect = annotated "io redirect" $ choice [ merging readIoNumberRedirect, merging readHereString, merging readHereDoc, readIoFile ] `thenSkip` spacing

readRedirectList = many1 readIoRedirect

readHereString = annotated "here string" $ do
    try $ string "<<<"
    spacing
    word <- readNormalWord
    return $ T_FdRedirect "" $ T_HereString word

readNewlineList = many1 ((newline <|> carriageReturn) `thenSkip` spacing)
readLineBreak = optional readNewlineList

readSeparatorOp = do
    notFollowedBy (g_AND_IF <|> g_DSEMI)
    f <- char ';' <|> char '&'
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

makeSimpleCommand tokens =
    let (assignment, rest) = partition (\x -> case dropAnnotation x of T_Assignment _ _ -> True; _ -> False) tokens
    in let (redirections, rest2) = partition (\x -> case dropAnnotation x of T_FdRedirect _ _ -> True; _ -> False) rest
       in T_Redirecting redirections $ T_SimpleCommand assignment rest2

readSimpleCommand = annotated "simple command" $ do
    prefix <- option [] readCmdPrefix
    cmd <- option [] $ do { f <- annotated "command name" readCmdName; return [f]; }
    when (null prefix && null cmd) $ fail "No command"
    if null cmd
        then return $ makeSimpleCommand prefix
        else do
            suffix <- option [] readCmdSuffix
            return $ makeSimpleCommand (prefix ++ cmd ++ suffix)

readPipeline = annotated "Pipeline" $ do
    notFollowedBy $ try readKeyword
    do
        g_Bang `thenSkip` spacing
        pipe <- readPipeSequence
        return $ T_Banged pipe
      <|> do
        readPipeSequence

readAndOr = (flip (>>=)) (return . T_Command) $ chainr1 readPipeline $ do
    pos <- getPosition
    op <- g_AND_IF <|> g_OR_IF
    readLineBreak
    return $ \a b ->
                blankAnnotation pos $
                     case op of T_AND_IF -> T_AndIf a b
                                T_OR_IF  -> T_OrIf a b

readTerm = do
    m <- readAndOr
    readTerm' m

readTerm' current =
    do
        sep <- readSeparator
        more <- (option T_EOF $ readAndOr)
        case more of T_EOF -> return [transformWithSeparator sep current]
                     _     -> do
                                list <- readTerm' more
                                return $ (transformWithSeparator sep current : list)
      <|>
        return [current]

transformWithSeparator '&' = T_Backgrounded
transformWithSeparator _  = id


readPipeSequence = do
    list <- readCommand `sepBy1` (readPipe `thenSkip` (spacing >> readLineBreak))
    spacing
    return $ T_Pipeline list

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

readIfClause = annotated "if statement" $ do
    (condition, action) <- readIfPart
    elifs <- many readElifPart
    elses <- option [] readElsePart
    g_Fi
    return $ T_IfExpression ((condition, action):elifs) elses

readIfPart = do
    g_If
    allspacing
    condition <- readTerm
    g_Then
    allspacing
    action <- readTerm
    return (condition, action)

readElifPart = do
    g_Elif
    allspacing
    condition <- readTerm
    g_Then
    allspacing
    action <- readTerm
    return (condition, action)

readElsePart = do
    g_Else
    allspacing
    readTerm

readSubshell = annotated "subshell group" $ do
    char '('
    allspacing
    list <- readCompoundList
    allspacing
    char ')'
    return $ T_Subshell list

readBraceGroup = annotated "brace group" $ do
    char '{'
    allspacing
    list <- readTerm
    allspacing
    char '}'
    return $ T_BraceGroup list

readWhileClause = annotated "while loop" $ do
    g_While
    condition <- readTerm
    statements <- readDoGroup
    return $ T_WhileExpression condition statements

readUntilClause = annotated "until loop" $ do
    g_Until
    condition <- readTerm
    statements <- readDoGroup
    return $ T_UntilExpression condition statements

readDoGroup = do
    pos <- getPosition
    g_Do
    allspacing
    (eof >> return []) <|>
        do
            commands <- readCompoundList
            disregard g_Done <|> eof -- stunted support
            return commands
          <|> do
            parseProblemAt pos ErrorC "Can't find the 'done' for this 'do'"
            fail "No done"

readForClause = annotated "for loop" $ do
    g_For
    spacing
    name <- readVariableName
    allspacing
    values <- readInClause <|> (readSequentialSep >> return [])
    group <- readDoGroup <|> (allspacing >> eof >> return []) -- stunted support
    return $ T_ForIn name values group

readInClause = do
    g_In
    things <- (readCmdWord) `reluctantlyTill`
                (disregard (g_Semi) <|> disregard linefeed <|> disregard g_Do)

    do {
        lookAhead (g_Do);
        parseNote ErrorC "You need a line feed or semicolon before the 'do' (in Bash)";
    } <|> do {
        optional $ g_Semi;
        disregard allspacing;
    }

    return things

readCaseClause = annotated "case statement" $ do
    g_Case
    word <- readNormalWord
    spacing
    g_In
    readLineBreak
    list <- readCaseList
    g_Esac
    return $ T_CaseExpression word list

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

readFunctionDefinition = annotated "function definition" $ do
    name <- try readFunctionSignature
    allspacing
    (disregard (lookAhead g_Lbrace) <|> parseProblem ErrorC "Expected a { to open the function definition")
    group <- merging readBraceGroup
    return $ T_Function name group


readFunctionSignature = do
    (optional $ try (string "function " >> parseNote StyleC "Don't use 'function' in front of function definitions"))
    name <- readVariableName
    spacing
    g_Lparen
    g_Rparen
    return name


readPattern = (readNormalWord `thenSkip` spacing) `sepBy1` (char '|' `thenSkip` spacing)


readCompoundCommand = annotated "compound command" $ do
    cmd <- merging $ choice [ readBraceGroup, readSubshell, readWhileClause, readUntilClause, readIfClause, readForClause, readCaseClause, readFunctionDefinition]
    spacing
    redirs <- many readIoRedirect
    return $ T_Redirecting redirs $ cmd


readCompoundList = readTerm

readCmdPrefix = many1 (readIoRedirect <|> readAssignmentWord)
readCmdSuffix = many1 (readIoRedirect <|> annotated "normal word" readCmdWord)

readAssignmentWord = annotated "assignment" $ try $ do
    optional (char '$' >> parseNote ErrorC "Don't use $ on the left side of assignments")
    variable <- readVariableName
    space <- spacing
    pos <- getPosition
    char '='
    space2 <- spacing
    value <- readNormalWord
    spacing
    when (space ++ space2 /= "") $ parseNoteAt pos ErrorC "Don't put spaces around the = in assignments"
    return $ T_Assignment variable value


tryToken s t = try (string s >> spacing >> return t)
tryWordToken s t = tryParseWordToken (string s) t `thenSkip` spacing
tryParseWordToken parser t = try (parser >> (lookAhead (eof <|> disregard whitespace))) >> return t

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

readKeyword = choice [ g_Then, g_Else, g_Elif, g_Fi, g_Do, g_Done, g_Esac, g_Rbrace, g_Rparen, g_DSEMI ]

ifParse p t f = do
    (lookAhead (try p) >> t) <|> f

wtf = do
    x <- many anyChar
    parseProblem ErrorC x

readScript = do
    do {
        allspacing;
        commands <- readTerm;
        eof <|> (parseProblem WarningC "Stopping here, because I can't parse this command");
        return $ T_Script commands;
    } <|> do {
        parseProblem WarningC "Couldn't read any commands";
        wtf;
        return T_EOF;
    }

shpell s = rp readScript s
rp p s = Ms.runState (runParserT p StackEmpty "-" s) []

-------- Destructively simplify AST

simplify (T_Redirecting [] t) = t
simplify (T_Pipeline [x]) = dropAnnotation x
simplify (T_NormalWord [x]) = dropAnnotation x
simplify t = t

-------- Analytics
doAllAnalysis t = foldl (\v f -> doAnalysis f v) t checks

getAst s = case rp readScript s of (Right parsed, _) -> parsed
getAst2 s = case rp readScript s of (Right parsed, _) -> transform simplify parsed
lol (Right x, _) = x

deadSimple (T_NormalWord l) = [concat (concatMap (deadSimple . dropAnnotation) l)]
deadSimple (T_DoubleQuoted l) = ["\"" ++(concat (concatMap (deadSimple . dropAnnotation) l)) ++ "\""]
deadSimple (T_SingleQuoted s) = [s]
deadSimple (T_DollarVariable _) = ["${VAR}"]
deadSimple (T_DollarBraced _) = ["${VAR}"]
deadSimple (T_DollarArithmetic _) = ["${VAR}"]
deadSimple (T_DollarExpansion _) = ["${VAR}"]
deadSimple (T_Literal x) = [x]
deadSimple (T_SimpleCommand vars words) = concatMap (deadSimple . dropAnnotation) words
deadSimple (T_Redirecting _ foo) = deadSimple foo
deadSimple _ = []


checks = [checkUuoc]
checkUuoc (T_Pipeline ((Annotated _ _ x):_:_)) = case (deadSimple x) of ["cat", _] -> style "UUOC: Instead of 'cat a | b', use 'b < a'"
                                                                        _ -> return ()
checkUuoc _ = return ()


main = do
    s <- getContents
--    case rp readScript s of (Right parsed, _) -> putStrLn . show $ transform simplify parsed
--                            (Left x, y) -> putStrLn $ "Can't parse: " ++ (show (x,y))

    mapM (putStrLn . show) $ getNotes s
