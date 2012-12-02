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
{-# LANGUAGE NoMonomorphismRestriction #-}

module ShellCheck.Parser (Note(..), Severity(..), parseShell, ParseResult(..), ParseNote(..), notesFromMap, Metadata(..), sortNotes, commonCommands) where

import ShellCheck.AST
import Text.Parsec
import Debug.Trace
import Control.Monad
import Data.Char
import Data.List (isInfixOf, isSuffixOf, partition, sortBy, intercalate, nub)
import qualified Data.Map as Map
import qualified Control.Monad.State as Ms
import Data.Maybe
import Prelude hiding (readList)
import System.IO
import Text.Parsec.Error
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
bracedQuotable = oneOf "}\"$`'"
doubleQuotable = oneOf "\"$`"
whitespace = oneOf " \t\n"
linewhitespace = oneOf " \t"
extglobStart = oneOf "?*@!+"

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
    parseNote ErrorC "Literal carriage return. Run script through tr -d '\\r' ."
    char '\r'

--------- Message/position annotation on top of user state
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

setCurrentContexts c = do
    Ms.modify (\(list, _) -> (list, c))

getCurrentContexts = do
    (_, context) <- Ms.get
    return context

popContext = do
    v <- getCurrentContexts
    if not $ null v
        then do
            let (a:r) = v
            setCurrentContexts r
            return [a]
        else
            return []

pushContext c = do
    v <- getCurrentContexts
    setCurrentContexts (c:v)

parseProblemAt pos level msg = do
    Ms.modify (\(list, current) -> ((ParseNote pos level msg):list, current))

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

unexpecting s p = try $ do
    (try p >> unexpected s) <|> return ()

notFollowedBy2 = unexpecting "keyword/token"

disregard x = x >> return ()

reluctantlyTill p end = do
    (lookAhead ((disregard $ try end) <|> eof) >> return []) <|> do
        x <- p
        more <- reluctantlyTill p end
        return $ x:more
      <|> return []

reluctantlyTill1 p end = do
    notFollowedBy2 end
    x <- p
    more <- reluctantlyTill p end
    return $ x:more

attempting rest branch = do
    ((try branch) >> rest) <|> rest

orFail parser stuff = do
    try (disregard parser) <|> (disregard stuff >> fail "nope")

wasIncluded p = option False (p >> return True)

acceptButWarn parser level note = do
    optional $ try (do
        pos <- getPosition
        parser
        parseProblemAt pos level note
      )

called s p = do
    pos <- getPosition
    pushContext (pos, s)
    do
        v <- p
        popContext
        return v
     <|> do -- p failed without consuming input, abort context
        popContext
        fail $ ""

readConditionContents single = do
    readCondContents `attempting` (lookAhead $ do
                                pos <- getPosition
                                choice (map (try . string) commonCommands)
                                parseProblemAt pos WarningC "Use 'if cmd; then ..' to check exit code, or 'if [[ $(cmd) == .. ]]' to check output.")

  where
    typ = if single then SingleBracket else DoubleBracket
    readCondBinaryOp = try $ do
        op <- choice $ (map tryOp ["-nt", "-ot", "-ef", "==", "!=", "<=", ">=", "-eq", "-ne", "-lt", "-le", "-gt", "-ge", "=~", ">", "<", "="])
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
              parseProblemAt pos ErrorC $ "Expected this to be an argument to the unary condition."
              fail "oops")

    readCondUnaryOp = try $ do
        op <- choice $ (map tryOp [ "-a", "-b", "-c", "-d", "-e", "-f", "-g", "-h", "-L", "-k", "-p", "-r", "-s", "-S", "-t", "-u", "-w", "-x", "-O", "-G", "-N",
                    "-z", "-n", "-o", "-v", "-R"
                    ])
        hardCondSpacing
        return op
      where tryOp s = try $ do
              id <- getNextId
              string s
              return $ TC_Unary id typ s

    readCondWord = do
        notFollowedBy2 (try (spacing >> (string "]")))
        x <- readNormalWord
        pos <- getPosition
        if (endedWithBracket x)
            then do
                lookAhead (try $ (many whitespace) >> (eof <|> disregard readSeparator <|> disregard (g_Then <|> g_Do)))
                parseProblemAt pos ErrorC $ "You need a space before the " ++ (if single then "]" else "]]") ++ "."
            else
                disregard spacing
        return x
      where endedWithBracket (T_NormalWord id s@(_:_)) =
                case (last s) of T_Literal id s -> "]" `isSuffixOf` s
                                 _ -> False
            endedWithBracket _ = False

    readCondAndOp = do
        id <- getNextId
        x <- try (string "&&" <|> string "-a")
        when (single && x == "&&") $ addNoteFor id $ Note ErrorC "You can't use && inside [..]. Use [[..]] instead."
        when (not single && x == "-a") $ addNoteFor id $ Note ErrorC "In [[..]], use && instead of -a."
        softCondSpacing
        return $ TC_And id typ x


    readCondOrOp = do
        id <- getNextId
        x <- try (string "||" <|> string "-o")
        when (single && x == "||") $ addNoteFor id $ Note ErrorC "You can't use || inside [..]. Use [[..]] instead."
        when (not single && x == "-o") $ addNoteFor id $ Note ErrorC "In [[..]], use && instead of -o."
        softCondSpacing
        return $ TC_Or id typ x

    readCondNoaryOrBinary = do
      id <- getNextId
      x <- readCondWord `attempting` (do
              pos <- getPosition
              lookAhead (char '[')
              parseProblemAt pos ErrorC $ if single
                  then "Don't use [] for grouping. Use \\( .. \\)."
                  else "Don't use [] for grouping. Use ()."
            )
      (do
            pos <- getPosition
            op <- readCondBinaryOp
            y <- readCondWord <|> ( (parseProblemAt pos ErrorC $ "Expected another argument for this operator.") >> mzero)
            return (x `op` y)
          ) <|> (return $ TC_Noary id typ x)

    readCondGroup = do
          id <- getNextId
          pos <- getPosition
          lparen <- string "(" <|> string "\\("
          when (single && lparen == "(") $ parseProblemAt pos ErrorC "In [..] you have to escape (). Use [[..]] instead."
          when (not single && lparen == "\\(") $ parseProblemAt pos ErrorC "In [[..]] you shouldn't escape ()."
          if single then softCondSpacing else disregard spacing
          x <- readCondContents
          cpos <- getPosition
          rparen <- string ")" <|> string "\\)"
          if single then softCondSpacing else disregard spacing
          when (single && rparen == ")") $ parseProblemAt cpos ErrorC "In [..] you have to escape (). Use [[..]] instead."
          when (not single && rparen == "\\)") $ parseProblemAt cpos ErrorC "In [[..]] you shouldn't escape ()."
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
        return $ TC_Unary id typ "!" expr

    readCondExpr =
      readCondGroup <|> readCondUnaryExp <|> readCondNoaryOrBinary

    readCondOr = chainl1 readCondAnd readCondAndOp
    readCondAnd = chainl1 readCondTerm readCondOrOp
    readCondContents = readCondOr



prop_a1 = isOk readArithmeticContents " n++ + ++c"
prop_a2 = isOk readArithmeticContents "$N*4-(3,2)"
prop_a3 = isOk readArithmeticContents "n|=2<<1"
prop_a4 = isOk readArithmeticContents "n &= 2 **3"
prop_a5 = isOk readArithmeticContents "1 |= 4 && n >>= 4"
prop_a6 = isOk readArithmeticContents " 1 | 2 ||3|4"
prop_a7 = isOk readArithmeticContents "3*2**10"
prop_a8 = isOk readArithmeticContents "3"
prop_a9 = isOk readArithmeticContents "a^!-b"
prop_aA = isOk readArithmeticContents "! $?"
readArithmeticContents =
    readSequence
  where
    spacing = many whitespace

    splitBy x ops = chainl1 x (readBinary ops)
    readBinary ops = readComboOp ops TA_Binary
    readComboOp op token = do
        id <- getNextId
        op <- choice (map (\x -> try $ do
                                        s <- string x
                                        notFollowedBy2 $ oneOf "&|<>="
                                        return s
                            ) op)
        spacing
        return $ token id op

    readVar = do
        id <- getNextId
        x <- readVariableName `thenSkip` spacing
        return $ TA_Variable id x

    readExpansion = do
        id <- getNextId
        x <- readDollar
        spacing
        return $ TA_Expansion id x

    readGroup = do
        char '('
        s <- readSequence
        char ')'
        spacing
        return s

    readNumber = do
        id <- getNextId
        num <- many1 $ oneOf "0123456789."
        return $ TA_Literal id num

    readArithTerm = readGroup <|> readExpansion <|> readNumber <|> readVar

    readSequence = do
        spacing
        id <- getNextId
        l <- readAssignment `sepBy` (char ',' >> spacing)
        return $ TA_Sequence id l

    readAssignment = readTrinary `splitBy` ["=", "*=", "/=", "%=", "+=", "-=", "<<=", ">>=", "&=", "^=", "|="]
    readTrinary = do
        let part = readLogicalOr
        x <- part
        do
            id <- getNextId
            string "?"
            spacing
            y <- part
            string ":"
            spacing
            z <- part
            return $ TA_Trinary id x y z
         <|>
          return x

    readLogicalOr  = readLogicalAnd `splitBy` ["||"]
    readLogicalAnd = readBitOr `splitBy` ["&&"]
    readBitOr  = readBitXor `splitBy` ["|"]
    readBitXor = readBitAnd `splitBy` ["^"]
    readBitAnd = readEquated `splitBy` ["&"]
    readEquated = readCompared `splitBy` ["==", "!="]
    readCompared = readShift `splitBy` ["<=", ">=", "<", ">"]
    readShift = readAddition `splitBy` ["<<", ">>"]
    readAddition = readMultiplication `splitBy` ["+", "-"]
    readMultiplication = readExponential `splitBy` ["*", "/", "%"]
    readExponential = readAnyNegated `splitBy` ["**"]

    readAnyNegated = readNegated <|> readAnySigned
    readNegated = do
        id <- getNextId
        op <- oneOf "!~"
        spacing
        x <- readAnySigned
        return $ TA_Unary id [op] x

    readAnySigned = readSigned <|> readAnycremented
    readSigned = do
        id <- getNextId
        op <- choice (map readSignOp "+-")
        spacing
        x <- readAnycremented
        return $ TA_Unary id [op] x
     where
        readSignOp c = try $ do
            char c
            notFollowedBy2 $ char c
            spacing
            return c

    readAnycremented = readNormalOrPostfixIncremented <|> readPrefixIncremented
    readPrefixIncremented = do
        id <- getNextId
        op <- try $ string "++" <|> string "--"
        spacing
        x <- readArithTerm
        return $ TA_Unary id (op ++ "|") x

    readNormalOrPostfixIncremented = do
        x <- readArithTerm
        spacing
        do
            id <- getNextId
            op <- try $ string "++" <|> string "--"
            spacing
            return $ TA_Unary id ("|" ++ op) x
         <|>
            return x



prop_readCondition = isOk readCondition "[ \\( a = b \\) -a \\( c = d \\) ]"
prop_readCondition2 = isOk readCondition "[[ (a = b) || (c = d) ]]"
readCondition = called "test expression" $ do
  opos <- getPosition
  id <- getNextId
  open <- (try $ string "[[") <|> (string "[")
  let single = open == "["
  condSpacingMsg False $ if single
        then "You need spaces after the opening [ and before the closing ]."
        else "You need spaces after the opening [[ and before the closing ]]."
  condition <- readConditionContents single

  cpos <- getPosition
  close <- (try $ string "]]") <|> (string "]") <|> do
        let match = (if single then "]" else "]]")
        parseProblemAt opos ErrorC $ "Couldn't find matching " ++ match ++ "."
        parseProblem ErrorC $ "Expected " ++ match ++ "."
        fail "condition"

  when (open == "[[" && close /= "]]") $ parseProblemAt cpos ErrorC "Did you mean ]] ?"
  when (open == "[" && close /= "]" ) $ parseProblemAt opos ErrorC "Did you mean [[ ?"
  return $ T_Condition id (if single then SingleBracket else DoubleBracket) condition


hardCondSpacing = condSpacingMsg False "You need a space here."
softCondSpacing = condSpacingMsg True "You need a space here."
condSpacingMsg soft msg = do
  pos <- getPosition
  space <- spacing
  when (null space) $ (if soft then parseNoteAt else parseProblemAt) pos ErrorC msg

readComment = do
    char '#'
    anyChar `reluctantlyTill` linefeed

prop_readNormalWord = isOk readNormalWord "'foo'\"bar\"{1..3}baz$(lol)"
prop_readNormalWord2 = isOk readNormalWord "foo**(foo)!!!(@@(bar))"
readNormalWord = do
    id <- getNextId
    pos <- getPosition
    x <- many1 readNormalWordPart
    checkPossibleTermination pos x
    return $ T_NormalWord id x


checkPossibleTermination pos [T_Literal _ x] =
    if x `elem` ["do", "done", "then", "fi", "esac", "}"]
        then parseProblemAt pos WarningC $ "Use semicolon or linefeed before '" ++ x ++ "' (or quote to make it literal)."
        else return ()
checkPossibleTermination _ _ = return ()


readNormalWordPart = readSingleQuoted <|> readDoubleQuoted <|> readGlob <|> readDollar <|> readBraced <|> readBackTicked <|> readProcSub <|> readNormalLiteral
readSpacePart = do
    id <- getNextId
    x <- many1 whitespace
    return $ T_Literal id x

readDollarBracedWord = do
    id <- getNextId
    list <- many readDollarBracedPart
    return $ T_NormalWord id list

readDollarBracedPart = readSingleQuoted <|> readDoubleQuoted <|> readExtglob <|> readDollar <|> readBackTicked <|> readDollarBracedLiteral

readDollarBracedLiteral = do
    id <- getNextId
    vars <- (readBraceEscaped <|> (anyChar >>= \x -> return [x])) `reluctantlyTill1` bracedQuotable
    return $ T_Literal id $ concat vars

prop_readProcSub1 = isOk readProcSub "<(echo test | wc -l)"
prop_readProcSub2 = isOk readProcSub "<(  if true; then true; fi )"
readProcSub = called "process substitution" $ do
    id <- getNextId
    dir <- try $ do
                    x <- oneOf "<>"
                    char '('
                    return [x]
    allspacing
    list <- readCompoundList
    allspacing
    char ')'
    return $ T_ProcSub id dir list

prop_readSingleQuoted = isOk readSingleQuoted "'foo bar'"
prop_readSingleQuoted2 = isWarning readSingleQuoted "'foo bar\\'"
readSingleQuoted = called "single quoted string" $ do
    id <- getNextId
    singleQuote
    s <- readSingleQuotedPart `reluctantlyTill` singleQuote
    pos <- getPosition
    singleQuote <?> "end of single quoted string"

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
readBackTicked = called "backtick expansion" $ do
    id <- getNextId
    parseNote WarningC "Use $(..) instead of deprecated `..` backtick expansion."
    pos <- getPosition
    char '`'
    f <- readGenericLiteral (char '`')
    char '`'
    return $ T_Backticked id f


prop_readDoubleQuoted = isOk readDoubleQuoted "\"Hello $FOO\""
readDoubleQuoted = called "double quoted string" $ do
    id <- getNextId
    doubleQuote
    x <- many doubleQuotedPart
    doubleQuote <?> "end of double quoted string"
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

prop_readGlob1 = isOk readGlob "*"
prop_readGlob2 = isOk readGlob "[^0-9]"
readGlob = readExtglob <|> readSimple <|> readClass <|> readGlobbyLiteral
    where
        readSimple = do
            id <- getNextId
            c <- oneOf "*?"
            return $ T_Glob id [c]
        -- Doesn't handle weird things like [^]a] and [$foo]. fixme?
        readClass = try $ do
            id <- getNextId
            char '['
            s <- many1 (letter <|> digit <|> oneOf "^-_:")
            char ']'
            return $ T_Glob id $ "[" ++ s ++ "]"

        readGlobbyLiteral = do
            id <- getNextId
            c <- extglobStart <|> char '['
            return $ T_Literal id [c]

readNormalLiteralPart = do
    readNormalEscaped <|> (anyChar `reluctantlyTill1` (quotable <|> extglobStart <|> char '['))

readNormalEscaped = called "escaped char" $ do
    pos <- getPosition
    backslash
    do
        next <- (quotable <|> oneOf "?*@!+[]")
        return $ if next == '\n' then "" else [next]
      <|>
        do
            next <- anyChar
            parseNoteAt pos WarningC $ "Did you mean \"$(printf \"\\" ++ [next] ++ "\")\"? The shell just ignores the \\ here."
            return [next]


prop_readExtglob1 = isOk readExtglob "!(*.mp3)"
prop_readExtglob2 = isOk readExtglob "!(*.mp3|*.wmv)"
prop_readExtglob4 = isOk readExtglob "+(foo \\) bar)"
prop_readExtglob5 = isOk readExtglob "+(!(foo *(bar)))"
readExtglob = called "extglob" $ do
    id <- getNextId
    c <- try $ do
            f <- extglobStart
            char '('
            return f
    contents <- readExtglobPart `sepBy` (char '|')
    char ')'
    return $ T_Extglob id [c] contents

readExtglobPart = do
    id <- getNextId
    x <- many1 (readNormalWordPart <|> readSpacePart)
    return $ T_NormalWord id x


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

readBraceEscaped = do
    bs <- backslash
    (linefeed >> return "")
        <|> (bracedQuotable >>= return . return)
        <|> (anyChar >>= (return . \x -> [bs, x]))


readGenericLiteral endExp = do
    strings <- (readGenericEscaped <|> (anyChar >>= \x -> return [x])) `reluctantlyTill` endExp
    return $ concat strings

readGenericLiteral1 endExp = do
    strings <- (readGenericEscaped <|> (anyChar >>= \x -> return [x])) `reluctantlyTill1` endExp
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

readDollar = readDollarArithmetic <|> readDollarBraced <|> readDollarExpansion <|> readDollarVariable <|> readDollarSingleQuote <|> readDollarLonely

prop_readDollarSingleQuote = isOk readDollarSingleQuote "$'foo\\\'lol'"
readDollarSingleQuote = called "$'..' expression" $ do
    id <- getNextId
    try $ string "$'"
    str <- readGenericLiteral (char '\'')
    char '\''
    return $ T_Literal id str

prop_readDollarArithmetic = isOk readDollarArithmetic "$(( 3 * 4 +5))"
prop_readDollarArithmetic2 = isOk readDollarArithmetic "$(((3*4)+(1*2+(3-1))))"
readDollarArithmetic = called "$((..)) expression" $ do
    id <- getNextId
    try (string "$((")
    c <- readArithmeticContents
    string "))"
    return (T_DollarArithmetic id c)

readArithmeticExpression = called "((..)) command" $ do
    id <- getNextId
    try (string "((")
    c <- readArithmeticContents
    string "))"
    return (T_Arithmetic id c)

prop_readDollarBraced1 = isOk readDollarBraced "${foo//bar/baz}"
prop_readDollarBraced2 = isOk readDollarBraced "${foo/'{cow}'}"
prop_readDollarBraced3 = isOk readDollarBraced "${foo%%$(echo cow})}"
prop_readDollarBraced4 = isOk readDollarBraced "${foo#\\}}"
readDollarBraced = called "parameter expansion" $ do
    id <- getNextId
    try (string "${")
    word <- readDollarBracedWord
    char '}'
    return $ T_DollarBraced id word

prop_readDollarExpansion = isOk readDollarExpansion "$(echo foo; ls\n)"
readDollarExpansion = called "command expansion" $ do
    id <- getNextId
    try (string "$(")
    cmds <- readCompoundList
    char ')' <?> "end of $(..) expression"
    return $ (T_DollarExpansion id cmds)

prop_readDollarVariable = isOk readDollarVariable "$@"
readDollarVariable = do
    id <- getNextId
    let singleCharred p = do
        n <- p
        value <- wrap [n]
        return (T_DollarBraced id value) `attempting` do
            pos <- getPosition
            num <- lookAhead $ many1 p
            parseNoteAt pos ErrorC $ "$" ++ (n:num) ++ " is equivalent to ${" ++ [n] ++ "}"++ num ++"."

    let positional = singleCharred digit
    let special = singleCharred specialVariable

    let regular = do
        name <- readVariableName
        value <- wrap name
        return $ T_DollarBraced id value

    try $ char '$' >> (positional <|> special <|> regular)

  where
    wrap s = do
        x <- getNextId
        y <- getNextId
        return $ T_NormalWord x [T_Literal y s]

readVariableName = do
    f <- variableStart
    rest <- many variableChars
    return (f:rest)

readDollarLonely = do
    id <- getNextId
    char '$'
    n <- lookAhead (anyChar <|> (eof >> return '_'))
    when (n /= '\'') $ parseNote StyleC "$ is not used specially and should therefore be escaped."
    return $ T_Literal id "$"

prop_readHereDoc = isOk readHereDoc "<< foo\nlol\ncow\nfoo"
prop_readHereDoc2 = isWarning readHereDoc "<<- EOF\n  cow\n  EOF"
readHereDoc = called "here document" $ do
    let stripLiteral (T_Literal _ x) = x
        stripLiteral (T_SingleQuoted _ x) = x
    fid <- getNextId
    pos <- getPosition
    try $ string "<<"
    dashed <- (char '-' >> return True) <|> return False
    tokenPosition <- getPosition
    sp <- spacing
    optional $ do
        try . lookAhead $ char '('
        let message = "Shells are space sensitive. Use '< <(cmd)', not '<<" ++ sp ++ "(cmd)'."
        parseProblemAt pos ErrorC message
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
    when (not dashed && spacing /= "") $ parseNote ErrorC "Use <<- instead of << if you want to indent the end token."
    when (dashed && filter (/= '\t') spacing /= "" ) $ parseNote ErrorC "When using <<-, you can only indent with tabs."
    return ()

debugHereDoc pos endToken doc =
    if endToken `isInfixOf` doc
        then parseProblemAt pos ErrorC ("Found " ++ endToken ++ " further down, but not by itself at the start of the line.")
        else if (map toLower endToken) `isInfixOf` (map toLower doc)
            then parseProblemAt pos ErrorC ("Found " ++ endToken ++ " further down, but with wrong casing.")
            else parseProblemAt pos ErrorC ("Couldn't find end token `" ++ endToken ++ "' in the here document.")


readFilename = readNormalWord
readIoFileOp = choice [g_LESSAND, g_GREATAND, g_DGREAT, g_LESSGREAT, g_CLOBBER, redirToken '<' T_Less, redirToken '>' T_Greater ]

prop_readIoFile = isOk readIoFile ">> \"$(date +%YYmmDD)\""
readIoFile = called "redirection" $ do
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
readHereString = called "here string" $ do
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
    notFollowedBy2 (g_AND_IF <|> g_DSEMI)
    f <- (try $ do
                    char '&'
                    spacing
                    pos <- getPosition
                    char ';'
                    parseProblemAt pos ErrorC "It's not 'foo &; bar', just 'foo & bar'."
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

makeSimpleCommand id1 id2 tokens =
    let (assignment, rest) = partition (\x -> case x of T_Assignment _ _ _ -> True; _ -> False) tokens
    in let (redirections, rest2) = partition (\x -> case x of T_FdRedirect _ _ _ -> True; _ -> False) rest
       in T_Redirecting id1 redirections $ T_SimpleCommand id2 assignment rest2

prop_readSimpleCommand = isOk readSimpleCommand "echo test > file"
readSimpleCommand = called "simple command" $ do
    id1 <- getNextId
    id2 <- getNextId
    prefix <- option [] readCmdPrefix
    cmd <- option [] $ do { f <- readCmdName; return [f]; }
    when (null prefix && null cmd) $ fail "No command"
    if null cmd
        then return $ makeSimpleCommand id1 id2 prefix
        else do
            suffix <- option [] readCmdSuffix
            return $ makeSimpleCommand id1 id2 (prefix ++ cmd ++ suffix)

prop_readPipeline = isOk readPipeline "! cat /etc/issue | grep -i ubuntu"
readPipeline = do
    unexpecting "keyword/token" readKeyword
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
    notFollowedBy2 g_OR_IF
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
readIfClause = called "if expression" $ do
    id <- getNextId
    pos <- getPosition
    (condition, action) <- readIfPart
    elifs <- many readElifPart
    elses <- option [] readElsePart

    g_Fi `orFail` do
        parseProblemAt pos ErrorC "Couldn't find 'fi' for this 'if'."
        parseProblem ErrorC "Expected 'fi' matching previously mentioned 'if'."

    return $ T_IfExpression id ((condition, action):elifs) elses

readIfPart = do
    pos <- getPosition
    g_If
    allspacing
    condition <- readTerm

    optional (do
        try . lookAhead $ g_Fi
        parseProblemAt pos ErrorC "Did you forget the 'then' for this 'if'?")

    called "then clause" $ do
        g_Then `orFail` parseProblem ErrorC "Expected 'then'."

        acceptButWarn g_Semi ErrorC "No semicolons directly after 'then'."
        allspacing
        action <- readTerm
        return (condition, action)

readElifPart = called "elif clause" $ do
    pos <- getPosition
    g_Elif
    allspacing
    condition <- readTerm
    g_Then
    acceptButWarn g_Semi ErrorC "No semicolons directly after 'then'."
    allspacing
    action <- readTerm
    return (condition, action)

readElsePart = called "else clause" $ do
    g_Else
    acceptButWarn g_Semi ErrorC "No semicolons directly after 'else'."
    allspacing
    readTerm

prop_readSubshell = isOk readSubshell "( cd /foo; tar cf stuff.tar * )"
readSubshell = called "explicit subshell" $ do
    id <- getNextId
    char '('
    allspacing
    list <- readCompoundList
    allspacing
    char ')'
    return $ T_Subshell id list

prop_readBraceGroup = isOk readBraceGroup "{ a; b | c | d; e; }"
readBraceGroup = called "brace group" $ do
    id <- getNextId
    char '{'
    allspacing
    list <- readTerm
    allspacing
    char '}'
    return $ T_BraceGroup id list

prop_readWhileClause = isOk readWhileClause "while [[ -e foo ]]; do sleep 1; done"
readWhileClause = called "while loop" $ do
    pos <- getPosition
    (T_While id) <- g_While
    condition <- readTerm
    statements <- readDoGroup pos
    return $ T_WhileExpression id condition statements

prop_readUntilClause = isOk readUntilClause "until kill -0 $PID; do sleep 1; done"
readUntilClause = called "until loop" $ do
    pos <- getPosition
    (T_Until id) <- g_Until
    condition <- readTerm
    statements <- readDoGroup pos
    return $ T_UntilExpression id condition statements

readDoGroup loopPos = do
    pos <- getPosition
    optional (do
                try . lookAhead $ g_Done
                parseProblemAt loopPos ErrorC "Did you forget the 'do' for this loop?")

    g_Do `orFail` parseProblem ErrorC "Expected 'do'."

    acceptButWarn g_Semi ErrorC "No semicolons directly after 'do'."
    allspacing

    commands <- readCompoundList
    g_Done `orFail` do
            parseProblemAt pos ErrorC "Couldn't find 'done' for this 'do'."
            parseProblem ErrorC "Expected 'done' matching previously mentioned 'do'."
    return commands


prop_readForClause = isOk readForClause "for f in *; do rm \"$f\"; done"
prop_readForClause3 = isOk readForClause "for f; do foo; done"
prop_readForClause4 = isOk readForClause "for((i=0; i<10; i++)); do echo $i; done"
prop_readForClause5 = isOk readForClause "for ((i=0;i<10 && n>x;i++,--n))\ndo \necho $i\ndone"
prop_readForClause6 = isOk readForClause "for ((;;))\ndo echo $i\ndone"
readForClause = called "for loop" $ do
    pos <- getPosition
    (T_For id) <- g_For
    spacing
    typ <- (readRegular <|> readArithmetic)
    group <- readDoGroup pos
    typ id group
  where
    readArithmetic = called "arithmetic for condition" $ do
        try $ string "(("
        x <- readArithmeticContents
        char ';' >> spacing
        y <- readArithmeticContents
        char ';' >> spacing
        z <- readArithmeticContents
        spacing
        string "))"
        readSequentialSep
        return $ \id group -> (return $ T_ForArithmetic id x y z group)

    readRegular = do
        name <- readVariableName
        spacing
        values <- readInClause <|> (readSequentialSep >> return [])
        return $ \id group -> (return $ T_ForIn id name values group)

readInClause = do
    g_In
    things <- (readCmdWord) `reluctantlyTill`
                (disregard (g_Semi) <|> disregard linefeed <|> disregard g_Do)

    do {
        lookAhead (g_Do);
        parseNote ErrorC "You need a line feed or semicolon before the 'do'.";
    } <|> do {
        optional $ g_Semi;
        disregard allspacing;
    }

    return things

prop_readCaseClause = isOk readCaseClause "case foo in a ) lol; cow;; b|d) fooo; esac"
readCaseClause = called "case expression" $ do
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

readCaseItem = called "case item" $ do
    notFollowedBy2 g_Esac
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
prop_readFunctionDefinition1 = isOk readFunctionDefinition "foo   (){ command foo --lol \"$@\"; }"
prop_readFunctionDefinition2 = isWarning readFunctionDefinition "function foo() { command foo --lol \"$@\"; }"
prop_readFunctionDefinition3 = isWarning readFunctionDefinition "function foo { lol; }"
prop_readFunctionDefinition4 = isWarning readFunctionDefinition "foo(a, b) { true; }"
readFunctionDefinition = called "function" $ do
    id <- getNextId
    name <- try readFunctionSignature
    allspacing
    (disregard (lookAhead g_Lbrace) <|> parseProblem ErrorC "Expected a { to open the function definition.")
    group <- readBraceGroup
    return $ T_Function id name group


readFunctionSignature = do
    readWithFunction <|> readWithoutFunction
  where
    readWithFunction = do
        pos <- getPosition
        try $ do
            string "function"
            whitespace
        parseProblemAt pos InfoC "Drop the keyword 'function'. It's optional in Bash but invalid in other shells."
        spacing
        name <- readVariableName
        optional spacing
        pos <- getPosition
        readParens <|> do
            parseProblemAt pos InfoC "Include '()' after the function name (in addition to dropping 'function')."
        return name

    readWithoutFunction = try $ do
        name <- readVariableName
        optional spacing
        readParens
        return name

    readParens = do
        g_Lparen
        optional spacing
        g_Rparen <|> do
            parseProblem ErrorC "Trying to declare parameters? Don't. Use () and refer to params as $1, $2.."
            anyChar `reluctantlyTill` oneOf "\n){"
            g_Rparen
        return ()



readPattern = (readNormalWord `thenSkip` spacing) `sepBy1` (char '|' `thenSkip` spacing)


readCompoundCommand = do
    id <- getNextId
    cmd <- choice [ readBraceGroup, readArithmeticExpression, readSubshell, readCondition, readWhileClause, readUntilClause, readIfClause, readForClause, readCaseClause, readFunctionDefinition]
    spacing
    redirs <- many readIoRedirect
    when (not . null $ redirs) $ optional $ do
        lookAhead $ try (spacing >> needsSeparator)
        parseProblem WarningC "Bash requires ; or \\n here, after redirecting nested compound commands."
    return $ T_Redirecting id redirs $ cmd

  where
    needsSeparator = choice [ g_Then, g_Else, g_Elif, g_Fi, g_Do, g_Done, g_Esac, g_Rbrace ]


readCompoundList = readTerm

readCmdPrefix = many1 (readIoRedirect <|> readAssignmentWord)
readCmdSuffix = many1 (readIoRedirect <|> readCmdWord)

prop_readAssignmentWord = isOk readAssignmentWord "a=42"
prop_readAssignmentWord2 = isOk readAssignmentWord "b=(1 2 3)"
prop_readAssignmentWord3 = isWarning readAssignmentWord "$b = 13"
prop_readAssignmentWord4 = isWarning readAssignmentWord "b = $(lol)"
prop_readAssignmentWord5 = isOk readAssignmentWord "b+=lol"
prop_readAssignmentWord6 = isWarning readAssignmentWord "b += (1 2 3)"
readAssignmentWord = try $ do
    id <- getNextId
    optional (char '$' >> parseNote ErrorC "Don't use $ on the left side of assignments.")
    variable <- readVariableName
    space <- spacing
    pos <- getPosition
    op <- string "+=" <|> string "="  -- analysis doesn't treat += as a reference. fixme?
    space2 <- spacing
    value <- readArray <|> readNormalWord
    spacing
    when (space ++ space2 /= "") $ parseNoteAt pos ErrorC "Don't put spaces around the = in assignments."
    when (space == "" && space2 /= "") $ parseNoteAt pos StyleC "Use var='' if you intended to assign the empty string."
    return $ T_Assignment id variable value

readArray = called "array assignment" $ do
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

redirToken c t = try $ do
    id <- getNextId
    char c
    notFollowedBy2 $ char '('
    return $ t id

tryWordToken s t = tryParseWordToken (string s) t `thenSkip` spacing
tryParseWordToken parser t = try $ do
    id <- getNextId
    parser
    optional (do
        try . lookAhead $ char '['
        parseProblem ErrorC "You need a space before the [.")
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

commonCommands = [ "admin", "alias", "ar", "asa", "at", "awk", "basename", "batch", "bc", "bg", "break", "c99", "cal", "cat", "cd", "cflow", "chgrp", "chmod", "chown", "cksum", "cmp", "colon", "comm", "command", "compress", "continue", "cp", "crontab", "csplit", "ctags", "cut", "cxref", "date", "dd", "delta", "df", "diff", "dirname", "dot", "du", "echo", "ed", "env", "eval", "ex", "exec", "exit", "expand", "export", "expr", "fc", "fg", "file", "find", "fold", "fort77", "fuser", "gencat", "get", "getconf", "getopts", "grep", "hash", "head", "iconv", "ipcrm", "ipcs", "jobs", "join", "kill", "lex", "link", "ln", "locale", "localedef", "logger", "logname", "lp", "ls", "m4", "mailx", "make", "man", "mesg", "mkdir", "mkfifo", "more", "mv", "newgrp", "nice", "nl", "nm", "nohup", "od", "paste", "patch", "pathchk", "pax", "pr", "printf", "prs", "ps", "pwd", "qalter", "qdel", "qhold", "qmove", "qmsg", "qrerun", "qrls", "qselect", "qsig", "qstat", "qsub", "read", "readonly", "renice", "return", "rm", "rmdel", "rmdir", "sact", "sccs", "sed", "set", "sh", "shift", "sleep", "sort", "split", "strings", "strip", "stty", "tabs", "tail", "talk", "tee", "test", "time", "times", "touch", "tput", "tr", "trap", "tsort", "tty", "type", "ulimit", "umask", "unalias", "uname", "uncompress", "unexpand", "unget", "uniq", "unlink", "unset", "uucp", "uudecode", "uuencode", "uustat", "uux", "val", "vi", "wait", "wc", "what", "who", "write", "xargs", "yacc", "zcat" ]

g_Semi = do
    notFollowedBy2 g_DSEMI
    tryToken ";" T_Semi

keywordSeparator = eof <|> disregard whitespace <|> (disregard $ oneOf ";()[")

readKeyword = choice [ g_Then, g_Else, g_Elif, g_Fi, g_Do, g_Done, g_Esac, g_Rbrace, g_Rparen, g_DSEMI ]

ifParse p t f = do
    (lookAhead (try p) >> t) <|> f

wtf = do
    x <- many anyChar
    parseProblem ErrorC x

readShebang = do
    try $ string "#!"
    str <- anyChar `reluctantlyTill` oneOf "\r\n"
    optional $ carriageReturn
    linefeed
    return str

readScript = do
    id <- getNextId
    sb <- option "" readShebang
    do {
        allspacing;
        commands <- readTerm;
        eof <|> (parseProblem ErrorC "Parsing stopped here because of parsing errors.");
        return $ T_Script id sb commands;
    } <|> do {
        parseProblem WarningC "Couldn't read any commands.";
        return $ T_Script id sb $ [T_EOF id];
    }

rp p filename contents = Ms.runState (runParserT p initialState filename contents) ([], [])

isWarning p s = (fst cs) && (not . null . snd $ cs) where cs = checkString p s
isOk p s = (fst cs) && (null . snd $ cs) where cs = checkString p s

checkString parser string =
    case rp (parser >> eof >> getState) "-" string of
        (Right (tree, map, notes), (problems, _)) -> (True, (notesFromMap map) ++ notes ++ problems)
        (Left _, (n, _)) -> (False, n)

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
            r -> (intercalate " " $ take 1 $ nub r)  ++ " Fix any mentioned problems and try again."
    where f err =
            case err of
                UnExpect s    -> (1, unexpected s)
                SysUnExpect s -> (2, unexpected s)
                Expect s      -> (3, "Expected " ++ s ++ ".")
                Message s     -> (4, s ++ ".")
          wut "" = "eof"
          wut x = x
          unexpected s = "Unexpected " ++ (wut s) ++ "."

parseShell filename contents = do
    case rp (parseWithNotes readScript) filename contents of
        (Right (script, map, notes), (parsenotes, _)) -> ParseResult (Just (script, map)) (nub $ sortNotes $ notes ++ parsenotes)
        (Left err, (p, context)) -> ParseResult Nothing (nub $ sortNotes $ p ++ (notesForContext context) ++ ([makeErrorFor err]))

  where
    notesForContext r = map contextNote $ take 1 r
    contextNote (pos, str) = ParseNote pos ErrorC $ "This " ++ str ++" is malformed."

lt x = trace (show x) x
ltt t x = trace (show t) x
