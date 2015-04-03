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
{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell, FlexibleContexts #-}
module ShellCheck.Parser (Note(..), Severity(..), parseShell, ParseResult(..), ParseNote(..), sortNotes, noteToParseNote, runTests, readScript) where

import ShellCheck.AST
import ShellCheck.Data
import ShellCheck.Options
import Text.Parsec
import Debug.Trace
import Control.Monad
import Control.Arrow (first)
import Data.Char
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, partition, sortBy, intercalate, nub)
import qualified Data.Map as Map
import qualified Control.Monad.State as Ms
import Data.Maybe
import Prelude hiding (readList)
import System.IO
import Text.Parsec.Error
import GHC.Exts (sortWith)
import Test.QuickCheck.All (quickCheckAll)

backslash = char '\\'
linefeed = optional carriageReturn >> char '\n'
singleQuote = char '\'' <|> unicodeSingleQuote
doubleQuote = char '"' <|> unicodeDoubleQuote
variableStart = upper <|> lower <|> oneOf "_"
variableChars = upper <|> lower <|> digit <|> oneOf "_"
functionChars = variableChars <|> oneOf ":+-.?"
specialVariable = oneOf "@*#?-$!"
tokenDelimiter = oneOf "&|;<> \t\n\r" <|> nbsp
quotableChars = "|&;<>()\\ '\t\n\r\xA0" ++ doubleQuotableChars
quotable = nbsp <|> unicodeDoubleQuote <|> oneOf quotableChars
bracedQuotable = oneOf "}\"$`'"
doubleQuotableChars = "\"$`" ++ unicodeDoubleQuoteChars
doubleQuotable = unicodeDoubleQuote <|> oneOf doubleQuotableChars
whitespace = oneOf " \t\n" <|> carriageReturn <|> nbsp
linewhitespace = oneOf " \t" <|> nbsp

suspectCharAfterQuotes = variableChars <|> char '%'

extglobStartChars = "?*@!+"
extglobStart = oneOf extglobStartChars

unicodeDoubleQuoteChars = "\x201C\x201D\x2033\x2036"

prop_spacing = isOk spacing "  \\\n # Comment"
spacing = do
    x <- many (many1 linewhitespace <|> try (string "\\\n" >> return ""))
    optional readComment
    return $ concat x

spacing1 = do
    spacing <- spacing
    when (null spacing) $ fail "Expected whitespace"
    return spacing

prop_allspacing = isOk allspacing "#foo"
prop_allspacing2 = isOk allspacing " #foo\n # bar\n#baz\n"
prop_allspacing3 = isOk allspacing "#foo\n#bar\n#baz\n"
allspacing = do
    s <- spacing
    more <- option False (linefeed >> return True)
    if more then do
        rest <- allspacing
        return $ s ++ "\n" ++ rest
      else
        return s

allspacingOrFail = do
    s <- allspacing
    when (null s) $ fail "Expected whitespace"

unicodeDoubleQuote = do
    pos <- getPosition
    oneOf unicodeDoubleQuoteChars
    parseProblemAt pos WarningC 1015 "This is a unicode double quote. Delete and retype it."
    return '"'

unicodeSingleQuote = do
    pos <- getPosition
    char '\x2018' <|> char '\x2019'
    parseProblemAt pos WarningC 1016 "This is a unicode single quote. Delete and retype it."
    return '"'

carriageReturn = do
    parseNote ErrorC 1017 "Literal carriage return. Run script through tr -d '\\r' ."
    char '\r'

nbsp = do
    parseNote ErrorC 1018 "This is a &nbsp;. Delete it and retype as space."
    char '\xA0'
    return ' '

--------- Message/position annotation on top of user state
data Note = Note Id Severity Code String deriving (Show, Eq)
data ParseNote = ParseNote SourcePos Severity Code String deriving (Show, Eq)
data Severity = ErrorC | WarningC | InfoC | StyleC deriving (Show, Eq, Ord)
data Context = ContextName SourcePos String | ContextAnnotation [Annotation] deriving (Show)
type Code = Integer

codeForParseNote (ParseNote _ _ code _) = code
noteToParseNote map (Note id severity code message) =
        ParseNote pos severity code message
    where
        pos = fromJust $ Map.lookup id map

initialState = (Id $ -1, Map.empty, [])

getLastId = do
    (id, _, _) <- getState
    return id

getNextIdAt sourcepos = do
    (id, map, notes) <- getState
    let newId = incId id
    let newMap = Map.insert newId sourcepos map
    putState (newId, newMap, notes)
    return newId
  where incId (Id n) = Id $ n+1

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
    irrelevant <- shouldIgnoreCode (codeForParseNote n)
    unless irrelevant $ do
        (a, b, notes) <- getState
        putState (a, b, n:notes)

shouldIgnoreCode code = do
    context <- getCurrentContexts
    return $ any disabling context
  where
    disabling (ContextAnnotation list) =
        any disabling' list
    disabling _ = False
    disabling' (DisableComment n) = code == n

-- Store potential parse problems outside of parsec
parseProblem level code msg = do
    pos <- getPosition
    parseProblemAt pos level code msg

setCurrentContexts c =
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
            return $ Just a
        else
            return Nothing

pushContext c = do
    v <- getCurrentContexts
    setCurrentContexts (c:v)

parseProblemAt pos level code msg = do
    irrelevant <- shouldIgnoreCode code
    unless irrelevant $
        Ms.modify (first ((:) (ParseNote pos level code msg)))

-- Store non-parse problems inside

parseNote c l a = do
    pos <- getPosition
    parseNoteAt pos c l a

parseNoteAt pos c l a = addParseNote $ ParseNote pos c l a

--------- Convenient combinators
thenSkip main follow = do
    r <- main
    optional follow
    return r

unexpecting s p = try $
    (try p >> unexpected s) <|> return ()

notFollowedBy2 = unexpecting "keyword/token"

disregard = void

reluctantlyTill p end =
    (lookAhead (disregard (try end) <|> eof) >> return []) <|> do
        x <- p
        more <- reluctantlyTill p end
        return $ x:more
      <|> return []

reluctantlyTill1 p end = do
    notFollowedBy2 end
    x <- p
    more <- reluctantlyTill p end
    return $ x:more

attempting rest branch =
    (try branch >> rest) <|> rest

orFail parser errorAction =
    try parser <|> (errorAction >>= fail)

wasIncluded p = option False (p >> return True)

acceptButWarn parser level code note =
    optional $ try (do
        pos <- getPosition
        parser
        parseProblemAt pos level code note
      )

withContext entry p = do
    pushContext entry
    do
        v <- p
        popContext
        return v
     <|> do -- p failed without consuming input, abort context
        v <- popContext
        fail ""

called s p = do
    pos <- getPosition
    withContext (ContextName pos s) p

withAnnotations anns =
    withContext (ContextAnnotation anns)

readConditionContents single =
    readCondContents `attempting` lookAhead (do
                                pos <- getPosition
                                s <- many1 letter
                                when (s `elem` commonCommands) $
                                    parseProblemAt pos WarningC 1009 "Use 'if cmd; then ..' to check exit code, or 'if [[ $(cmd) == .. ]]' to check output.")

  where
    spacingOrLf = condSpacing True
    condSpacing required = do
        pos <- getPosition
        space <- allspacing
        when (required && null space) $
            parseProblemAt pos ErrorC 1035 "You are missing a required space here."
        when (single && '\n' `elem` space) $
            parseProblemAt pos ErrorC 1080 "When breaking lines in [ ], you need \\ before the linefeed."
        return space

    typ = if single then SingleBracket else DoubleBracket
    readCondBinaryOp = try $ do
        optional guardArithmetic
        id <- getNextId
        op <- choice (map tryOp ["==", "!=", "<=", ">=", "=~", ">", "<", "=", "\\<=", "\\>=", "\\<", "\\>"]) <|> otherOp
        spacingOrLf
        return op
      where
        tryOp s = try $ do
            id <- getNextId
            string s
            return $ TC_Binary id typ s
        otherOp = try $ do
            id <- getNextId
            s <- readOp
            when (s == "-a" || s == "-o") $ fail "Unexpected operator"
            return $ TC_Binary id typ s

    guardArithmetic = do
        try . lookAhead $ disregard (oneOf "+*/%") <|> disregard (string "- ")
        parseProblem ErrorC 1076 $
            if single
            then "Trying to do math? Use e.g. [ $((i/2+7)) -ge 18 ]."
            else "Trying to do math? Use e.g. [[ $((i/2+7)) -ge 18 ]]."

    readCondUnaryExp = do
      op <- readCondUnaryOp
      pos <- getPosition
      (readCondWord >>= return . op) `orFail` do
          parseProblemAt pos ErrorC 1019 "Expected this to be an argument to the unary condition."
          return "Expected an argument for the unary operator"

    readCondUnaryOp = try $ do
        id <- getNextId
        s <- readOp
        spacingOrLf
        return $ TC_Unary id typ s

    readOp = try $ do
        char '-'
        s <- many1 letter
        return ('-':s)

    readCondWord = do
        notFollowedBy2 (try (spacing >> string "]"))
        x <- readNormalWord
        pos <- getPosition
        when (endedWith "]" x) $ do
            parseProblemAt pos ErrorC 1020 $
                "You need a space before the " ++ (if single then "]" else "]]") ++ "."
            fail "Missing space before ]"
        when (single && endedWith ")" x) $ do
            parseProblemAt pos ErrorC 1021
                "You need a space before the \\)"
            fail "Missing space before )"
        disregard spacing
        return x
      where endedWith str (T_NormalWord id s@(_:_)) =
                case last s of T_Literal id s -> str `isSuffixOf` s
                               _ -> False
            endedWith _ _ = False

    readCondAndOp = do
        id <- getNextId
        x <- try (readAndOrOp "&&" False <|> readAndOrOp "-a" True)
        return $ TC_And id typ x

    readCondOrOp = do
        optional guardArithmetic
        id <- getNextId
        x <- try (readAndOrOp "||" False <|> readAndOrOp "-o" True)
        return $ TC_Or id typ x

    readAndOrOp op requiresSpacing = do
        x <- string op
        condSpacing requiresSpacing
        return x

    readCondNoaryOrBinary = do
      id <- getNextId
      x <- readCondWord `attempting` (do
              pos <- getPosition
              lookAhead (char '[')
              parseProblemAt pos ErrorC 1026 $ if single
                  then "If grouping expressions inside [..], use \\( ..\\)."
                  else "If grouping expressions inside [[..]], use ( .. )."
            )
      (do
            pos <- getPosition
            isRegex <- regexOperatorAhead
            op <- readCondBinaryOp
            y <- if isRegex
                    then readRegex
                    else  readCondWord <|> (parseProblemAt pos ErrorC 1027 "Expected another argument for this operator." >> mzero)
            return (x `op` y)
          ) <|> return (TC_Noary id typ x)

    readCondGroup = do
          id <- getNextId
          pos <- getPosition
          lparen <- try $ string "(" <|> string "\\("
          when (single && lparen == "(") $
              parseProblemAt pos ErrorC 1028 "In [..] you have to escape (). Use [[..]] instead."
          when (not single && lparen == "\\(") $
              parseProblemAt pos ErrorC 1029 "In [[..]] you shouldn't escape ()."
          condSpacing single
          x <- readCondContents
          cpos <- getPosition
          rparen <- string ")" <|> string "\\)"
          condSpacing single
          when (single && rparen == ")") $
              parseProblemAt cpos ErrorC 1030 "In [..] you have to escape (). Use [[..]] instead."
          when (not single && rparen == "\\)") $
              parseProblemAt cpos ErrorC 1031 "In [[..]] you shouldn't escape ()."
          when (isEscaped lparen `xor` isEscaped rparen) $
              parseProblemAt pos ErrorC 1032 "Did you just escape one half of () but not the other?"
          return $ TC_Group id typ x
      where
        isEscaped ('\\':_) = True
        isEscaped _ = False
        xor x y = x && not y || not x && y

    -- Currently a bit of a hack since parsing rules are obscure
    regexOperatorAhead = lookAhead (do
        try (string "=~") <|> try (string "~=")
        return True)
          <|> return False
    readRegex = called "regex" $ do
        id <- getNextId
        parts <- many1 (
                readGroup <|>
                readSingleQuoted <|>
                readDoubleQuoted <|>
                readDollarExpression <|>
                readNormalLiteral "( " <|>
                readPipeLiteral <|>
                readGlobLiteral)
        disregard spacing
        return $ T_NormalWord id parts
      where
        readGlobLiteral = do
            id <- getNextId
            s <- many1 (extglobStart <|> oneOf "{}[]$")
            return $ T_Literal id s
        readGroup = called "regex grouping" $ do
            id <- getNextId
            char '('
            parts <- many (readGroup <|> readSingleQuoted <|> readDoubleQuoted <|> readDollarExpression <|> readRegexLiteral <|> readGlobLiteral)
            char ')'
            return $ T_NormalWord id parts
        readRegexLiteral = do
            id <- getNextId
            str <- readGenericLiteral1 (singleQuote <|> doubleQuotable <|> oneOf "()")
            return $ T_Literal id str
        readPipeLiteral = do
            id <- getNextId
            str <- string "|"
            return $ T_Literal id str

    readCondTerm = do
        term <- readCondNot <|> readCondExpr
        condSpacing False
        return term

    readCondNot = do
        id <- getNextId
        char '!'
        spacingOrLf
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
prop_a10= isOk readArithmeticContents "! $?"
prop_a11= isOk readArithmeticContents "10#08 * 16#f"
prop_a12= isOk readArithmeticContents "\"$((3+2))\" + '37'"
prop_a13= isOk readArithmeticContents "foo[9*y+x]++"
prop_a14= isOk readArithmeticContents "1+`echo 2`"
prop_a15= isOk readArithmeticContents "foo[`echo foo | sed s/foo/4/g` * 3] + 4"
prop_a16= isOk readArithmeticContents "$foo$bar"
prop_a17= isOk readArithmeticContents "i<(0+(1+1))"
prop_a18= isOk readArithmeticContents "a?b:c"
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

    readArrayIndex = do
        id <- getNextId
        char '['
        middle <- readArithmeticContents
        char ']'
        return $ TA_Index id middle

    literal s = do
        id <- getNextId
        string s
        return $ T_Literal id s

    readArithmeticLiteral =
        readArrayIndex <|> literal "#"

    readExpansion = do
        id <- getNextId
        pieces <- many1 $ choice [
            readArithmeticLiteral,
            readSingleQuoted,
            readDoubleQuoted,
            readNormalDollar,
            readBraced,
            readBackTicked,
            readNormalLiteral "+-*/=%^,]?:"
            ]
        spacing
        return $ TA_Expansion id pieces

    readGroup = do
        char '('
        s <- readSequence
        char ')'
        spacing
        return s

    readArithTerm = readGroup <|> readExpansion

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
            return $ TA_Unary id ('|':op) x
         <|>
            return x



prop_readCondition = isOk readCondition "[ \\( a = b \\) -a \\( c = d \\) ]"
prop_readCondition2 = isOk readCondition "[[ (a = b) || (c = d) ]]"
prop_readCondition3 = isOk readCondition "[[ $c = [[:alpha:].~-] ]]"
prop_readCondition4 = isOk readCondition "[[ $c =~ *foo* ]]"
prop_readCondition5 = isOk readCondition "[[ $c =~ f( ]] )* ]]"
prop_readCondition5a= isOk readCondition "[[ $c =~ a(b) ]]"
prop_readCondition5b= isOk readCondition "[[ $c =~ f( ($var ]]) )* ]]"
prop_readCondition6 = isOk readCondition "[[ $c =~ ^[yY]$ ]]"
prop_readCondition7 = isOk readCondition "[[ ${line} =~ ^[[:space:]]*# ]]"
prop_readCondition8 = isOk readCondition "[[ $l =~ ogg|flac ]]"
prop_readCondition9 = isOk readCondition "[ foo -a -f bar ]"
prop_readCondition10= isOk readCondition "[[\na == b\n||\nc == d ]]"
prop_readCondition10a= isOk readCondition "[[\na == b  ||\nc == d ]]"
prop_readCondition10b= isOk readCondition "[[ a == b\n||\nc == d ]]"
prop_readCondition11= isOk readCondition "[[ a == b ||\n c == d ]]"
prop_readCondition12= isWarning readCondition "[ a == b \n -o c == d ]"
prop_readCondition13= isOk readCondition "[[ foo =~ ^fo{1,3}$ ]]"
readCondition = called "test expression" $ do
    opos <- getPosition
    id <- getNextId
    open <- try (string "[[") <|> string "["
    let single = open == "["

    pos <- getPosition
    space <- allspacing
    when (null space) $
        parseProblemAt pos ErrorC 1035 $ "You need a space after the " ++
            if single
                then "[ and before the ]."
                else "[[ and before the ]]."
    when (single && '\n' `elem` space) $
        parseProblemAt pos ErrorC 1080 "You need \\ before line feeds to break lines in [ ]."

    condition <- readConditionContents single

    cpos <- getPosition
    close <- try (string "]]") <|> string "]"
    when (open == "[[" && close /= "]]") $ parseProblemAt cpos ErrorC 1033 "Did you mean ]] ?"
    when (open == "[" && close /= "]" ) $ parseProblemAt opos ErrorC 1034 "Did you mean [[ ?"
    spacing
    many readCmdWord -- Read and throw away remainders to get then/do warnings. Fixme?
    return $ T_Condition id (if single then SingleBracket else DoubleBracket) condition

readAnnotationPrefix = do
    char '#'
    many linewhitespace
    string "shellcheck"

prop_readAnnotation1 = isOk readAnnotation "# shellcheck disable=1234,5678\n"
prop_readAnnotation2 = isOk readAnnotation "# shellcheck disable=SC1234 disable=SC5678\n"
readAnnotation = called "shellcheck annotation" $ do
    try readAnnotationPrefix
    many1 linewhitespace
    values <- many1 readDisable
    linefeed
    many linewhitespace
    return $ concat values
  where
    readDisable = forKey "disable" $
        readCode `sepBy` char ','
      where
        readCode = do
            optional $ string "SC"
            int <- many1 digit
            return $ DisableComment (read int)
    forKey s p = do
        try $ string s
        char '='
        value <- p
        many linewhitespace
        return value

readAnnotations = do
    annotations <- many (readAnnotation `thenSkip` allspacing)
    return $ concat annotations

readComment = do
    unexpecting "shellcheck annotation" readAnnotationPrefix
    char '#'
    many $ noneOf "\r\n"

prop_readNormalWord = isOk readNormalWord "'foo'\"bar\"{1..3}baz$(lol)"
prop_readNormalWord2 = isOk readNormalWord "foo**(foo)!!!(@@(bar))"
prop_readNormalWord3 = isOk readNormalWord "foo#"
prop_readNormalWord4 = isOk readNormalWord "$\"foo\"$'foo\nbar'"
prop_readNormalWord5 = isWarning readNormalWord "${foo}}"
prop_readNormalWord6 = isOk readNormalWord "foo/{}"
readNormalWord = readNormalishWord ""

readNormalishWord end = do
    id <- getNextId
    pos <- getPosition
    x <- many1 (readNormalWordPart end)
    checkPossibleTermination pos x
    return $ T_NormalWord id x

checkPossibleTermination pos [T_Literal _ x] =
    when (x `elem` ["do", "done", "then", "fi", "esac"]) $
        parseProblemAt pos WarningC 1010 $ "Use semicolon or linefeed before '" ++ x ++ "' (or quote to make it literal)."
checkPossibleTermination _ _ = return ()

readNormalWordPart end = do
    notFollowedBy2 $ oneOf end
    checkForParenthesis
    choice [
        readSingleQuoted,
        readDoubleQuoted,
        readGlob,
        readNormalDollar,
        readBraced,
        readBackTicked,
        readProcSub,
        readNormalLiteral end,
        readLiteralCurlyBraces
      ]
  where
    checkForParenthesis =
        return () `attempting` do
            pos <- getPosition
            lookAhead $ char '('
            parseProblemAt pos ErrorC 1036 "'(' is invalid here. Did you forget to escape it?"

    readLiteralCurlyBraces = do
        id <- getNextId
        str <- findParam <|> literalBraces
        return $ T_Literal id str

    findParam = try $ string "{}"
    literalBraces = do
        pos <- getPosition
        c <- oneOf "{}"
        parseProblemAt pos WarningC 1083 $
            "This " ++ [c] ++ " is literal. Check expression (missing ;/\\n?) or quote it."
        return [c]


readSpacePart = do
    id <- getNextId
    x <- many1 whitespace
    return $ T_Literal id x

readDollarBracedWord = do
    id <- getNextId
    list <- many readDollarBracedPart
    return $ T_NormalWord id list

readDollarBracedPart = readSingleQuoted <|> readDoubleQuoted <|> readExtglob <|> readNormalDollar <|> readBackTicked <|> readDollarBracedLiteral

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
prop_readsingleQuoted3 = isWarning readSingleQuoted "\x2018hello\x2019"
prop_readSingleQuoted4 = isWarning readNormalWord "'it's"
prop_readSingleQuoted5 = isWarning readSimpleCommand "foo='bar\ncow 'arg"
prop_readSingleQuoted6 = isOk readSimpleCommand "foo='bar cow 'arg"
readSingleQuoted = called "single quoted string" $ do
    id <- getNextId
    startPos <- getPosition
    singleQuote
    s <- readSingleQuotedPart `reluctantlyTill` singleQuote
    let string = concat s
    endPos <- getPosition
    singleQuote <?> "end of single quoted string"

    optional $ do
        c <- try . lookAhead $ suspectCharAfterQuotes <|> oneOf "'"
        if not (null string) && isAlpha c && isAlpha (last string)
          then
            parseProblemAt endPos WarningC 1011
                "This apostrophe terminated the single quoted string!"
          else
            when ('\n' `elem` string && not ("\n" `isPrefixOf` string)) $
                suggestForgotClosingQuote startPos endPos "single quoted string"

    return (T_SingleQuoted id string)

readSingleQuotedLiteral = do
    singleQuote
    strs <- many1 readSingleQuotedPart
    singleQuote
    return $ concat strs

readSingleQuotedPart =
    readSingleEscaped
    <|> many1 (noneOf "'\\\x2018\x2019")

prop_readBackTicked = isOk readBackTicked "`ls *.mp3`"
prop_readBackTicked2 = isOk readBackTicked "`grep \"\\\"\"`"
prop_readBackTicked3 = isWarning readBackTicked "´grep \"\\\"\"´"
prop_readBackTicked4 = isOk readBackTicked "`echo foo\necho bar`"
prop_readBackTicked5 = isOk readSimpleCommand "echo `foo`bar"
prop_readBackTicked6 = isWarning readSimpleCommand "echo `foo\necho `bar"
prop_readBackTicked7 = isOk readSimpleCommand "`#inline comment`"
prop_readBackTicked8 = isOk readSimpleCommand "echo `#comment` \\\nbar baz"
readBackTicked = called "backtick expansion" $ do
    id <- getNextId
    startPos <- getPosition
    backtick
    subStart <- getPosition
    subString <- readGenericLiteral "`´"
    endPos <- getPosition
    backtick

    optional $ do
        c <- try . lookAhead $ suspectCharAfterQuotes
        when ('\n' `elem` subString && not ("\n" `isPrefixOf` subString)) $
            suggestForgotClosingQuote startPos endPos "backtick expansion"

    -- Result positions may be off due to escapes
    result <- subParse subStart readTermOrNone (unEscape subString)
    return $ T_Backticked id result
  where
    unEscape [] = []
    unEscape ('\\':x:rest) | x `elem` "$`\\" = x : unEscape rest
    unEscape ('\\':'\n':rest) = unEscape rest
    unEscape (c:rest) = c : unEscape rest
    backtick =
      disregard (char '`') <|> do
         pos <- getPosition
         char '´'
         parseProblemAt pos ErrorC 1077
            "For command expansion, the tick should slant left (` vs ´)."

subParse pos parser input = do
    lastPosition <- getPosition
    lastInput <- getInput
    setPosition pos
    setInput input
    result <- parser
    setInput lastInput
    setPosition lastPosition
    return result

prop_readDoubleQuoted = isOk readDoubleQuoted "\"Hello $FOO\""
prop_readDoubleQuoted2 = isOk readDoubleQuoted "\"$'\""
prop_readDoubleQuoted3 = isWarning readDoubleQuoted "\x201Chello\x201D"
prop_readDoubleQuoted4 = isWarning readSimpleCommand "\"foo\nbar\"foo"
prop_readDoubleQuoted5 = isOk readSimpleCommand "lol \"foo\nbar\" etc"
readDoubleQuoted = called "double quoted string" $ do
    id <- getNextId
    startPos <- getPosition
    doubleQuote
    x <- many doubleQuotedPart
    endPos <- getPosition
    doubleQuote <?> "end of double quoted string"
    optional $ do
        try . lookAhead $ suspectCharAfterQuotes <|> oneOf "$\""
        when (any hasLineFeed x && not (startsWithLineFeed x)) $
            suggestForgotClosingQuote startPos endPos "double quoted string"
    return $ T_DoubleQuoted id x
  where
    startsWithLineFeed (T_Literal _ ('\n':_):_) = True
    startsWithLineFeed _ = False
    hasLineFeed (T_Literal _ str) | '\n' `elem` str = True
    hasLineFeed _ = False

suggestForgotClosingQuote startPos endPos name = do
    parseProblemAt startPos WarningC 1078 $
        "Did you forget to close this " ++ name ++ "?"
    parseProblemAt endPos InfoC 1079
        "This is actually an end quote, but due to next char it looks suspect."

doubleQuotedPart = readDoubleLiteral <|> readDoubleQuotedDollar <|> readBackTicked

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
    x <- many1 (readDoubleEscaped <|> many1 (noneOf ('\\':doubleQuotableChars)))
    return $ concat x

readNormalLiteral end = do
    id <- getNextId
    s <- many1 (readNormalLiteralPart end)
    return $ T_Literal id (concat s)

prop_readGlob1 = isOk readGlob "*"
prop_readGlob2 = isOk readGlob "[^0-9]"
prop_readGlob3 = isOk readGlob "[a[:alpha:]]"
prop_readGlob4 = isOk readGlob "[[:alnum:]]"
prop_readGlob5 = isOk readGlob "[^[:alpha:]1-9]"
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
            s <- many1 (predefined <|> liftM return (letter <|> digit <|> oneOf globchars))
            char ']'
            return $ T_Glob id $ "[" ++ concat s ++ "]"
          where
           globchars = "^-_:?*.,!~@#$%=+{}/~"
           predefined = do
              try $ string "[:"
              s <- many1 letter
              string ":]"
              return $ "[:" ++ s ++ ":]"

        readGlobbyLiteral = do
            id <- getNextId
            c <- extglobStart <|> char '['
            return $ T_Literal id [c]

readNormalLiteralPart end =
    readNormalEscaped <|> many1 (noneOf (end ++ quotableChars ++ extglobStartChars ++ "[{}"))

readNormalEscaped = called "escaped char" $ do
    pos <- getPosition
    backslash
    do
        next <- quotable <|> oneOf "?*@!+[]{}.,~#"
        return $ if next == '\n' then "" else [next]
      <|>
        do
            next <- anyChar
            case escapedChar next of
                Just name -> parseNoteAt pos WarningC 1012 $ "\\" ++ [next] ++ " is just literal '" ++ [next] ++ "' here. For " ++ name ++ ", use " ++ alternative next ++ " instead."
                Nothing -> parseNoteAt pos InfoC 1001 $ "This \\" ++ [next] ++ " will be a regular '" ++ [next] ++ "' in this context."
            return [next]
  where
    alternative 'n' = "a quoted, literal line feed"
    alternative t = "\"$(printf \"\\" ++ [t] ++ "\")\""
    escapedChar 'n' = Just "line feed"
    escapedChar 't' = Just "tab"
    escapedChar 'r' = Just "carriage return"
    escapedChar _ = Nothing


prop_readExtglob1 = isOk readExtglob "!(*.mp3)"
prop_readExtglob2 = isOk readExtglob "!(*.mp3|*.wmv)"
prop_readExtglob4 = isOk readExtglob "+(foo \\) bar)"
prop_readExtglob5 = isOk readExtglob "+(!(foo *(bar)))"
prop_readExtglob6 = isOk readExtglob "*(((||))|())"
prop_readExtglob7 = isOk readExtglob "*(<>)"
prop_readExtglob8 = isOk readExtglob "@(|*())"
readExtglob = called "extglob" $ do
    id <- getNextId
    c <- try $ do
            f <- extglobStart
            char '('
            return f
    contents <- readExtglobPart `sepBy` char '|'
    char ')'
    return $ T_Extglob id [c] contents

readExtglobPart = do
    id <- getNextId
    x <- many (readExtglobGroup <|> readNormalWordPart "" <|> readSpacePart <|> readExtglobLiteral)
    return $ T_NormalWord id x
  where
    readExtglobGroup = do
        id <- getNextId
        char '('
        contents <- readExtglobPart `sepBy` char '|'
        char ')'
        return $ T_Extglob id "" contents
    readExtglobLiteral = do
        id <- getNextId
        str <- many1 (oneOf "<>#;&")
        return $ T_Literal id str


readSingleEscaped = do
    s <- backslash
    let attempt level code p msg = do { try $ parseNote level code msg; x <- p; return [s,x]; }

    do {
        x <- lookAhead singleQuote;
        parseProblem InfoC 1003 "Are you trying to escape that single quote? echo 'You'\\''re doing it wrong'.";
        return [s];
    }
        <|> attempt InfoC 1004 linefeed "You don't break lines with \\ in single quotes, it results in literal backslash-linefeed."
        <|> do
            x <- anyChar
            return [s,x]


readDoubleEscaped = do
    bs <- backslash
    (linefeed >> return "")
        <|> liftM return doubleQuotable
        <|> liftM (\ x -> [bs, x]) anyChar

readBraceEscaped = do
    bs <- backslash
    (linefeed >> return "")
        <|> liftM return bracedQuotable
        <|> liftM (\ x -> [bs, x]) anyChar


readGenericLiteral endChars = do
    strings <- many (readGenericEscaped <|> many1 (noneOf ('\\':endChars)))
    return $ concat strings

readGenericLiteral1 endExp = do
    strings <- (readGenericEscaped <|> (anyChar >>= \x -> return [x])) `reluctantlyTill1` endExp
    return $ concat strings

readGenericEscaped = do
    backslash
    x <- anyChar
    return $ if x == '\n' then [] else ['\\', x]

prop_readBraced = isOk readBraced "{1..4}"
prop_readBraced2 = isOk readBraced "{foo,bar,\"baz lol\"}"
readBraced = try $ do
    let strip (T_Literal _ s) = return ("\"" ++ s ++ "\"")
    id <- getNextId
    char '{'
    str <- many1 ((readDoubleQuotedLiteral >>= strip) <|> readGenericLiteral1 (oneOf "}\"" <|> whitespace))
    char '}'
    let result = concat str
    unless (',' `elem` result || ".." `isInfixOf` result) $
        fail "Not a brace expression"
    return $ T_BraceExpansion id result

readNormalDollar = readDollarExpression <|> readDollarDoubleQuote <|> readDollarSingleQuote <|> readDollarLonely
readDoubleQuotedDollar = readDollarExpression <|> readDollarLonely
readDollarExpression = readDollarArithmetic <|> readDollarBracket <|> readDollarBraced <|> readDollarExpansion <|> readDollarVariable

prop_readDollarSingleQuote = isOk readDollarSingleQuote "$'foo\\\'lol'"
readDollarSingleQuote = called "$'..' expression" $ do
    id <- getNextId
    try $ string "$'"
    str <- readGenericLiteral "'"
    char '\''
    return $ T_DollarSingleQuoted id str

prop_readDollarDoubleQuote = isOk readDollarDoubleQuote "$\"hello\""
readDollarDoubleQuote = do
    lookAhead . try $ string "$\""
    id <- getNextId
    char '$'
    doubleQuote
    x <- many doubleQuotedPart
    doubleQuote <?> "end of translated double quoted string"
    return $ T_DollarDoubleQuoted id x


prop_readDollarArithmetic = isOk readDollarArithmetic "$(( 3 * 4 +5))"
prop_readDollarArithmetic2 = isOk readDollarArithmetic "$(((3*4)+(1*2+(3-1))))"
readDollarArithmetic = called "$((..)) expression" $ do
    id <- getNextId
    try (string "$((")
    c <- readArithmeticContents
    string "))"
    return (T_DollarArithmetic id c)

readDollarBracket = called "$[..] expression" $ do
    id <- getNextId
    try (string "$[")
    c <- readArithmeticContents
    string "]"
    return (T_DollarBracket id c)

prop_readArithmeticExpression = isOk readArithmeticExpression "((a?b:c))"
readArithmeticExpression = called "((..)) command" $ do
    id <- getNextId
    try (string "((")
    c <- readArithmeticContents
    string "))"
    return (T_Arithmetic id c)

prop_readDollarBraced1 = isOk readDollarBraced "${foo//bar/baz}"
prop_readDollarBraced2 = isOk readDollarBraced "${foo/'{cow}'}"
prop_readDollarBraced3 = isOk readDollarBraced "${foo%%$(echo cow\\})}"
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
    return $ T_DollarExpansion id cmds

prop_readDollarVariable = isOk readDollarVariable "$@"
prop_readDollarVariable2 = isOk (readDollarVariable >> anyChar) "$?!"
prop_readDollarVariable3 = isWarning (readDollarVariable >> anyChar) "$10"
prop_readDollarVariable4 = isWarning (readDollarVariable >> string "[@]") "$arr[@]"

readDollarVariable = do
    id <- getNextId
    pos <- getPosition

    let singleCharred p = do
        n <- p
        value <- wrap [n]
        return (T_DollarBraced id value)

    let positional = do
        value <- singleCharred digit
        return value `attempting` do
            lookAhead digit
            parseNoteAt pos ErrorC 1037 "Braces are required for positionals over 9, e.g. ${10}."

    let special = singleCharred specialVariable

    let regular = do
        name <- readVariableName
        value <- wrap name
        return (T_DollarBraced id value) `attempting` do
            lookAhead $ void (string "[@]") <|> void (string "[*]") <|> void readArrayIndex
            parseNoteAt pos ErrorC 1087 "Braces are required when expanding arrays, as in ${array[idx]}."

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
    pos <- getPosition
    char '$'
    n <- lookAhead (anyChar <|> (eof >> return '_'))
    return $ T_Literal id "$"

prop_readHereDoc = isOk readHereDoc "<< foo\nlol\ncow\nfoo"
prop_readHereDoc2 = isWarning readHereDoc "<<- EOF\n  cow\n  EOF"
prop_readHereDoc3 = isOk readHereDoc "<< foo\n$\"\nfoo"
prop_readHereDoc4 = isOk readHereDoc "<< foo\n`\nfoo"
prop_readHereDoc5 = isOk readHereDoc "<<- !foo\nbar\n!foo"
prop_readHereDoc6 = isOk readHereDoc "<< foo\\ bar\ncow\nfoo bar"
prop_readHereDoc7 = isOk readHereDoc "<< foo\n\\$(f ())\nfoo"
readHereDoc = called "here document" $ do
    fid <- getNextId
    pos <- getPosition
    try $ string "<<"
    dashed <- (char '-' >> return Dashed) <|> return Undashed
    tokenPosition <- getPosition
    sp <- spacing
    optional $ do
        try . lookAhead $ char '('
        let message = "Shells are space sensitive. Use '< <(cmd)', not '<<" ++ sp ++ "(cmd)'."
        parseProblemAt pos ErrorC 1038 message
    hid <- getNextId
    (quoted, endToken) <-
            liftM (\ x -> (Quoted, stripLiteral x)) readDoubleQuotedLiteral
            <|> liftM (\ x -> (Quoted, x)) readSingleQuotedLiteral
            <|> (readToken >>= (\x -> return (Unquoted, x)))
    spacing

    startPos <- getPosition
    hereData <- anyChar `reluctantlyTill` do
                    linefeed
                    spacing
                    string endToken
                    disregard linefeed  <|> eof

    do
        linefeed
        spaces <- spacing
        verifyHereDoc dashed quoted spaces hereData
        string endToken
        parsedData <- parseHereData quoted startPos hereData
        return $ T_FdRedirect fid "" $ T_HereDoc hid dashed quoted endToken parsedData
     `attempting` (eof >> debugHereDoc tokenPosition endToken hereData)

  where
    stripLiteral (T_Literal _ x) = x
    stripLiteral (T_SingleQuoted _ x) = x

    readToken =
        liftM concat $ many1 (escaped <|> quoted <|> normal)
      where
        quoted = liftM stripLiteral readDoubleQuotedLiteral <|> readSingleQuotedLiteral
        normal = anyChar `reluctantlyTill1` (whitespace <|> oneOf ";&)'\"\\")
        escaped = do -- surely the user must be doing something wrong at this point
            char '\\'
            c <- anyChar
            return [c]

    parseHereData Quoted startPos hereData = do
        id <- getNextIdAt startPos
        return [T_Literal id hereData]

    parseHereData Unquoted startPos hereData =
        subParse startPos readHereData hereData

    readHereData = many $ try doubleQuotedPart <|> readHereLiteral

    readHereLiteral = do
        id <- getNextId
        chars <- many1 $ noneOf "`$\\"
        return $ T_Literal id chars

    verifyHereDoc dashed quoted spacing hereInfo = do
        when (dashed == Undashed && spacing /= "") $
            parseNote ErrorC 1039 "Use <<- instead of << if you want to indent the end token."
        when (dashed == Dashed && filter (/= '\t') spacing /= "" ) $
            parseNote ErrorC 1040 "When using <<-, you can only indent with tabs."
        return ()

    debugHereDoc pos endToken doc
        | endToken `isInfixOf` doc =
            let lookAt line = when (endToken `isInfixOf` line) $
                      parseProblemAt pos ErrorC 1041 ("Close matches include '" ++ line ++ "' (!= '" ++ endToken ++ "').")
            in do
                  parseProblemAt pos ErrorC 1042 ("Found '" ++ endToken ++ "' further down, but not entirely by itself.")
                  mapM_ lookAt (lines doc)
        | map toLower endToken `isInfixOf` map toLower doc =
            parseProblemAt pos ErrorC 1043 ("Found " ++ endToken ++ " further down, but with wrong casing.")
        | otherwise =
            parseProblemAt pos ErrorC 1044 ("Couldn't find end token `" ++ endToken ++ "' in the here document.")


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
    x <- many1 digit <|> string "&"
    lookAhead readIoFileOp
    return x

prop_readIoNumberRedirect = isOk readIoNumberRedirect "3>&2"
prop_readIoNumberRedirect2 = isOk readIoNumberRedirect "2> lol"
prop_readIoNumberRedirect3 = isOk readIoNumberRedirect "4>&-"
prop_readIoNumberRedirect4 = isOk readIoNumberRedirect "&> lol"
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

prop_readSeparator1 = isWarning readScript "a &; b"
prop_readSeparator2 = isOk readScript "a & b"
readSeparatorOp = do
    notFollowedBy2 (void g_AND_IF <|> void readCaseSeparator)
    notFollowedBy2 (string "&>")
    f <- try (do
                    char '&'
                    spacing
                    pos <- getPosition
                    char ';'
                    -- In case statements we might have foo & ;;
                    notFollowedBy2 $ char ';'
                    parseProblemAt pos ErrorC 1045 "It's not 'foo &; bar', just 'foo & bar'."
                    return '&'
            ) <|> char ';' <|> char '&'
    spacing
    return f

readSequentialSep = disregard (g_Semi >> readLineBreak) <|> disregard readNewlineList
readSeparator =
    do
        separator <- readSeparatorOp
        readLineBreak
        return separator
     <|>
        do
            readNewlineList
            return '\n'

makeSimpleCommand id1 id2 prefix cmd suffix =
    let
        (preAssigned, preRest) = partition assignment prefix
        (preRedirected, preRest2) = partition redirection preRest
        (postRedirected, postRest) = partition redirection suffix

        redirs = preRedirected ++ postRedirected
        assigns = preAssigned
        args = cmd ++ preRest2 ++ postRest
    in
        T_Redirecting id1 redirs $ T_SimpleCommand id2 assigns args
  where
    assignment (T_Assignment {}) = True
    assignment _ = False
    redirection (T_FdRedirect {}) = True
    redirection _ = False


prop_readSimpleCommand = isOk readSimpleCommand "echo test > file"
prop_readSimpleCommand2 = isOk readSimpleCommand "cmd &> file"
prop_readSimpleCommand3 = isOk readSimpleCommand "export foo=(bar baz)"
prop_readSimpleCommand4 = isOk readSimpleCommand "typeset -a foo=(lol)"
prop_readSimpleCommand5 = isOk readSimpleCommand "time if true; then echo foo; fi"
prop_readSimpleCommand6 = isOk readSimpleCommand "time -p ( ls -l; )"
readSimpleCommand = called "simple command" $ do
    id1 <- getNextId
    id2 <- getNextId
    prefix <- option [] readCmdPrefix
    cmd <- option Nothing $ do { f <- readCmdName; return $ Just f; }
    when (null prefix && isNothing cmd) $ fail "No command"
    case cmd of
      Nothing -> return $ makeSimpleCommand id1 id2 prefix [] []
      Just cmd -> do
            suffix <- option [] $ getParser readCmdSuffix cmd [
                        (["declare", "export", "local", "readonly", "typeset"], readModifierSuffix),
                        (["time"], readTimeSuffix),
                        (["let"], readLetSuffix)
                    ]
            return $ makeSimpleCommand id1 id2 prefix [cmd] suffix
  where
    isCommand strings (T_NormalWord _ [T_Literal _ s]) = s `elem` strings
    isCommand _ _ = False
    getParser def cmd [] = def
    getParser def cmd ((list, action):rest) =
        if isCommand list cmd
        then action
        else getParser def cmd rest

prop_readPipeline = isOk readPipeline "! cat /etc/issue | grep -i ubuntu"
prop_readPipeline2 = isWarning readPipeline "!cat /etc/issue | grep -i ubuntu"
prop_readPipeline3 = isOk readPipeline "for f; do :; done|cat"
readPipeline = do
    unexpecting "keyword/token" readKeyword
    do
        (T_Bang id) <- g_Bang
        pipe <- readPipeSequence
        return $ T_Banged id pipe
      <|>
        readPipeSequence

prop_readAndOr = isOk readAndOr "grep -i lol foo || exit 1"
prop_readAndOr1 = isOk readAndOr "# shellcheck disable=1\nfoo"
prop_readAndOr2 = isOk readAndOr "# shellcheck disable=1\n# lol\n# shellcheck disable=3\nfoo"
readAndOr = do
    aid <- getNextId
    annotations <- readAnnotations

    andOr <- withAnnotations annotations $
        chainr1 readPipeline $ do
            op <- g_AND_IF <|> g_OR_IF
            readLineBreak
            return $ case op of T_AND_IF id -> T_AndIf id
                                T_OR_IF  id -> T_OrIf id

    return $ if null annotations
                then andOr
                else T_Annotation aid annotations andOr

readTermOrNone = do
    allspacing
    readTerm <|> do
        eof
        return []

readTerm = do
    allspacing
    m <- readAndOr
    readTerm' m

readTerm' current =
    do
        id <- getNextId
        sep <- readSeparator
        more <- option (T_EOF id) readAndOr
        case more of (T_EOF _) -> return [transformWithSeparator id sep current]
                     _         -> do
                                list <- readTerm' more
                                return (transformWithSeparator id sep current : list)
      <|>
        return [current]

transformWithSeparator i '&' = T_Backgrounded i
transformWithSeparator i _  = id


readPipeSequence = do
    id <- getNextId
    (cmds, pipes) <- sepBy1WithSeparators readCommand
                        (readPipe `thenSkip` (spacing >> readLineBreak))
    spacing
    return $ T_Pipeline id pipes cmds
  where
    sepBy1WithSeparators p s = do
        let elems = p >>= \x -> return ([x], [])
        let seps = do
            separator <- s
            return $ \(a,b) (c,d) -> (a++c, b ++ d ++ [separator])
        elems `chainl1` seps

readPipe = do
    notFollowedBy2 g_OR_IF
    id <- getNextId
    char '|'
    qualifier <- string "&" <|> return ""
    spacing
    return $ T_Pipe id ('|':qualifier)

readCommand = choice [
    readCompoundCommand,
    readCoProc,
    readSimpleCommand
    ]

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
prop_readIfClause4 = isWarning readIfClause "if false; then true; else if true; then echo lol; fi"
prop_readIfClause5 = isOk readIfClause "if false; then true; else\nif true; then echo lol; fi; fi"
readIfClause = called "if expression" $ do
    id <- getNextId
    pos <- getPosition
    (condition, action) <- readIfPart
    elifs <- many readElifPart
    elses <- option [] readElsePart

    g_Fi `orFail` do
        parseProblemAt pos ErrorC 1046 "Couldn't find 'fi' for this 'if'."
        parseProblem ErrorC 1047 "Expected 'fi' matching previously mentioned 'if'."
        return "Expected 'fi'."

    return $ T_IfExpression id ((condition, action):elifs) elses


verifyNotEmptyIf s =
    optional (do
                emptyPos <- getPosition
                try . lookAhead $ (g_Fi <|> g_Elif <|> g_Else)
                parseProblemAt emptyPos ErrorC 1048 $ "Can't have empty " ++ s ++ " clauses (use 'true' as a no-op).")
readIfPart = do
    pos <- getPosition
    g_If
    allspacing
    condition <- readTerm

    ifNextToken (g_Fi <|> g_Elif) $
        parseProblemAt pos ErrorC 1049 "Did you forget the 'then' for this 'if'?"

    called "then clause" $ do
        g_Then `orFail` do
            parseProblem ErrorC 1050 "Expected 'then'."
            return "Expected 'then'."

        acceptButWarn g_Semi ErrorC 1051 "No semicolons directly after 'then'."
        allspacing
        verifyNotEmptyIf "then"

        action <- readTerm
        return (condition, action)

readElifPart = called "elif clause" $ do
    pos <- getPosition
    correctElif <- elif
    unless correctElif $
        parseProblemAt pos ErrorC 1075 "Use 'elif' instead of 'else if'."
    allspacing
    condition <- readTerm

    ifNextToken (g_Fi <|> g_Elif) $
        parseProblemAt pos ErrorC 1049 "Did you forget the 'then' for this 'elif'?"

    g_Then
    acceptButWarn g_Semi ErrorC 1052 "No semicolons directly after 'then'."
    allspacing
    verifyNotEmptyIf "then"
    action <- readTerm
    return (condition, action)
  where
    elif = (g_Elif >> return True) <|>
        try (g_Else >> g_If >> return False)

readElsePart = called "else clause" $ do
    pos <- getPosition
    g_Else
    acceptButWarn g_Semi ErrorC 1053 "No semicolons directly after 'else'."
    allspacing
    verifyNotEmptyIf "else"
    readTerm

ifNextToken parser action =
    optional $ do
        try . lookAhead $ parser
        action

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
prop_readBraceGroup2 = isWarning readBraceGroup "{foo;}"
readBraceGroup = called "brace group" $ do
    id <- getNextId
    char '{'
    allspacingOrFail <|> parseProblem ErrorC 1054 "You need a space after the '{'."
    optional $ do
        pos <- getPosition
        lookAhead $ char '}'
        parseProblemAt pos ErrorC 1055 "You need at least one command here. Use 'true;' as a no-op."
    list <- readTerm
    char '}' <|> do
        parseProblem ErrorC 1056 "Expected a '}'. If you have one, try a ; or \\n in front of it."
        fail "Missing '}'"
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
                parseProblemAt loopPos ErrorC 1057 "Did you forget the 'do' for this loop?")

    g_Do `orFail` do
        parseProblem ErrorC 1058 "Expected 'do'."
        return "Expected 'do'."

    acceptButWarn g_Semi ErrorC 1059 "No semicolons directly after 'do'."
    allspacing

    optional (do
                try . lookAhead $ g_Done
                parseProblemAt loopPos ErrorC 1060 "Can't have empty do clauses (use 'true' as a no-op).")

    commands <- readCompoundList
    g_Done `orFail` do
            parseProblemAt pos ErrorC 1061 "Couldn't find 'done' for this 'do'."
            parseProblem ErrorC 1062 "Expected 'done' matching previously mentioned 'do'."
            return "Expected 'done'."
    return commands


prop_readForClause = isOk readForClause "for f in *; do rm \"$f\"; done"
prop_readForClause3 = isOk readForClause "for f; do foo; done"
prop_readForClause4 = isOk readForClause "for((i=0; i<10; i++)); do echo $i; done"
prop_readForClause5 = isOk readForClause "for ((i=0;i<10 && n>x;i++,--n))\ndo \necho $i\ndone"
prop_readForClause6 = isOk readForClause "for ((;;))\ndo echo $i\ndone"
prop_readForClause7 = isOk readForClause "for ((;;)) do echo $i\ndone"
prop_readForClause8 = isOk readForClause "for ((;;)) ; do echo $i\ndone"
prop_readForClause9 = isOk readForClause "for i do true; done"
prop_readForClause10= isOk readForClause "for ((;;)) { true; }"
prop_readForClause12= isWarning readForClause "for $a in *; do echo \"$a\"; done"
readForClause = called "for loop" $ do
    pos <- getPosition
    (T_For id) <- g_For
    spacing
    readArithmetic id pos <|> readRegular id pos
  where
    readArithmetic id pos = called "arithmetic for condition" $ do
        try $ string "(("
        x <- readArithmeticContents
        char ';' >> spacing
        y <- readArithmeticContents
        char ';' >> spacing
        z <- readArithmeticContents
        spacing
        string "))"
        spacing
        optional $ readSequentialSep >> spacing
        group <- readBraced <|> readDoGroup pos
        return $ T_ForArithmetic id x y z group

    readBraced = do
        (T_BraceGroup _ list) <- readBraceGroup
        return list

    readRegular id pos = do
        acceptButWarn (char '$') ErrorC 1086
            "Don't use $ on the iterator name in for loops."
        name <- readVariableName `thenSkip` spacing
        values <- readInClause <|> (optional readSequentialSep >> return [])
        group <- readDoGroup pos
        return $ T_ForIn id name values group

prop_readSelectClause1 = isOk readSelectClause "select foo in *; do echo $foo; done"
prop_readSelectClause2 = isOk readSelectClause "select foo; do echo $foo; done"
readSelectClause = called "select loop" $ do
    pos <- getPosition
    (T_Select id) <- g_Select
    spacing
    typ <- readRegular
    group <- readDoGroup pos
    typ id group
  where
    readRegular = do
        name <- readVariableName
        spacing
        values <- readInClause <|> (readSequentialSep >> return [])
        return $ \id group -> (return $ T_SelectIn id name values group)

readInClause = do
    g_In
    things <- readCmdWord `reluctantlyTill`
                (disregard g_Semi <|> disregard linefeed <|> disregard g_Do)

    do {
        lookAhead g_Do;
        parseNote ErrorC 1063 "You need a line feed or semicolon before the 'do'.";
    } <|> do {
        optional g_Semi;
        disregard allspacing;
    }

    return things

prop_readCaseClause = isOk readCaseClause "case foo in a ) lol; cow;; b|d) fooo; esac"
prop_readCaseClause2 = isOk readCaseClause "case foo\n in * ) echo bar;; esac"
prop_readCaseClause3 = isOk readCaseClause "case foo\n in * ) echo bar & ;; esac"
prop_readCaseClause4 = isOk readCaseClause "case foo\n in *) echo bar ;& bar) foo; esac"
prop_readCaseClause5 = isOk readCaseClause "case foo\n in *) echo bar;;& foo) baz;; esac"
readCaseClause = called "case expression" $ do
    id <- getNextId
    g_Case
    word <- readNormalWord
    allspacing
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
    void g_Rparen <|> do
        parseProblem ErrorC 1085
            "Did you forget to move the ;; after extending this case item?"
        fail "Expected ) to open a new case item"
    readLineBreak
    list <- (lookAhead readCaseSeparator >> return []) <|> readCompoundList
    separator <- readCaseSeparator `attempting` do
        pos <- getPosition
        lookAhead g_Rparen
        parseProblemAt pos ErrorC 1074
            "Did you forget the ;; after the previous case item?"
    readLineBreak
    return (separator, pattern, list)

readCaseSeparator = choice [
    tryToken ";;&" (const ()) >> return CaseContinue,
    tryToken ";&" (const ()) >> return CaseFallThrough,
    g_DSEMI >> return CaseBreak,
    lookAhead (readLineBreak >> g_Esac) >> return CaseBreak
    ]

prop_readFunctionDefinition = isOk readFunctionDefinition "foo() { command foo --lol \"$@\"; }"
prop_readFunctionDefinition1 = isOk readFunctionDefinition "foo   (){ command foo --lol \"$@\"; }"
prop_readFunctionDefinition4 = isWarning readFunctionDefinition "foo(a, b) { true; }"
prop_readFunctionDefinition5 = isOk readFunctionDefinition ":(){ :|:;}"
prop_readFunctionDefinition6 = isOk readFunctionDefinition "?(){ foo; }"
prop_readFunctionDefinition7 = isOk readFunctionDefinition "..(){ cd ..; }"
prop_readFunctionDefinition8 = isOk readFunctionDefinition "foo() (ls)"
readFunctionDefinition = called "function" $ do
    functionSignature <- try readFunctionSignature
    allspacing
    disregard (lookAhead $ oneOf "{(") <|> parseProblem ErrorC 1064 "Expected a { to open the function definition."
    group <- readBraceGroup <|> readSubshell
    return $ functionSignature group
  where
    readFunctionSignature =
        readWithFunction <|> readWithoutFunction
      where
        readWithFunction = do
            id <- getNextId
            try $ do
                string "function"
                whitespace
            spacing
            name <- readFunctionName
            optional spacing
            hasParens <- wasIncluded readParens
            return $ T_Function id (FunctionKeyword True) (FunctionParentheses hasParens) name

        readWithoutFunction = try $ do
            id <- getNextId
            name <- readFunctionName
            optional spacing
            readParens
            return $ T_Function id (FunctionKeyword False) (FunctionParentheses True) name

        readParens = do
            g_Lparen
            optional spacing
            g_Rparen <|> do
                parseProblem ErrorC 1065 "Trying to declare parameters? Don't. Use () and refer to params as $1, $2.."
                many $ noneOf "\n){"
                g_Rparen
            return ()

        readFunctionName = many1 functionChars

prop_readCoProc1 = isOk readCoProc "coproc foo { echo bar; }"
prop_readCoProc2 = isOk readCoProc "coproc { echo bar; }"
prop_readCoProc3 = isOk readCoProc "coproc echo bar"
readCoProc = called "coproc" $ do
    id <- getNextId
    try $ do
        string "coproc"
        whitespace
    choice [ try $ readCompoundCoProc id, readSimpleCoProc id ]
  where
    readCompoundCoProc id = do
        var <- optionMaybe $
            readVariableName `thenSkip` whitespace
        body <- readBody readCompoundCommand
        return $ T_CoProc id var body
    readSimpleCoProc id = do
        body <- readBody readSimpleCommand
        return $ T_CoProc id Nothing body
    readBody parser = do
        id <- getNextId
        body <- parser
        return $ T_CoProcBody id body


readPattern = (readNormalWord `thenSkip` spacing) `sepBy1` (char '|' `thenSkip` spacing)

prop_readCompoundCommand = isOk readCompoundCommand "{ echo foo; }>/dev/null"
readCompoundCommand = do
    id <- getNextId
    cmd <- choice [ readBraceGroup, readArithmeticExpression, readSubshell, readCondition, readWhileClause, readUntilClause, readIfClause, readForClause, readSelectClause, readCaseClause, readFunctionDefinition]
    optional spacing
    redirs <- many readIoRedirect
    unless (null redirs) $ optional $ do
        lookAhead $ try (spacing >> needsSeparator)
        parseProblem WarningC 1013 "Bash requires ; or \\n here, after redirecting nested compound commands."
    return $ T_Redirecting id redirs cmd
  where
    needsSeparator = choice [ g_Then, g_Else, g_Elif, g_Fi, g_Do, g_Done, g_Esac, g_Rbrace ]


readCompoundList = readTerm

readCmdPrefix = many1 (readIoRedirect <|> readAssignmentWord)
readCmdSuffix = many1 (readIoRedirect <|> readCmdWord)
readModifierSuffix = many1 (readIoRedirect <|> readAssignmentWord <|> readCmdWord)
readTimeSuffix = do
    flags <- many readFlag
    pipeline <- readPipeline
    return $ flags ++ [pipeline]
  where
    -- This fails for quoted variables and such. Fixme?
    readFlag = do
        lookAhead $ char '-'
        readCmdWord

-- Fixme: this is a hack that doesn't handle let '++c' or let a\>b
readLetSuffix = many1 (readIoRedirect <|> try readLetExpression <|> readCmdWord)
  where
    readLetExpression = do
        startPos <- getPosition
        expression <- readStringForParser readCmdWord
        subParse startPos readArithmeticContents expression

-- Get whatever a parser would parse as a string
readStringForParser parser = do
    pos <- lookAhead (parser >> getPosition)
    readUntil pos
  where
    readUntil endPos = anyChar `reluctantlyTill` (getPosition >>= guard . (== endPos))

prop_readAssignmentWord = isOk readAssignmentWord "a=42"
prop_readAssignmentWord2 = isOk readAssignmentWord "b=(1 2 3)"
prop_readAssignmentWord3 = isWarning readAssignmentWord "$b = 13"
prop_readAssignmentWord4 = isWarning readAssignmentWord "b = $(lol)"
prop_readAssignmentWord5 = isOk readAssignmentWord "b+=lol"
prop_readAssignmentWord6 = isWarning readAssignmentWord "b += (1 2 3)"
prop_readAssignmentWord7 = isOk readAssignmentWord "a[3$n'']=42"
prop_readAssignmentWord8 = isOk readAssignmentWord "a[4''$(cat foo)]=42"
prop_readAssignmentWord9 = isOk readAssignmentWord "IFS= "
prop_readAssignmentWord9a= isOk readAssignmentWord "foo="
prop_readAssignmentWord10= isWarning readAssignmentWord "foo$n=42"
prop_readAssignmentWord11= isOk readAssignmentWord "foo=([a]=b [c] [d]= [e f )"
prop_readAssignmentWord12= isOk readAssignmentWord "a[b <<= 3 + c]='thing'"
readAssignmentWord = try $ do
    id <- getNextId
    pos <- getPosition
    optional (char '$' >> parseNote ErrorC 1066 "Don't use $ on the left side of assignments.")
    variable <- readVariableName
    optional (readNormalDollar >> parseNoteAt pos ErrorC
                                1067 "For indirection, use (associative) arrays or 'read \"var$n\" <<< \"value\"'")
    index <- optionMaybe readArrayIndex
    hasLeftSpace <- liftM (not . null) spacing
    pos <- getPosition
    op <- readAssignmentOp
    hasRightSpace <- liftM (not . null) spacing
    isEndOfCommand <- liftM isJust $ optionMaybe (try . lookAhead $ (disregard (oneOf "\r\n;&|)") <|> eof))
    if not hasLeftSpace && (hasRightSpace || isEndOfCommand)
      then do
        when (variable /= "IFS" && hasRightSpace) $
            parseNoteAt pos WarningC 1007
                "Remove space after = if trying to assign a value (for empty string, use var='' ... )."
        value <- readEmptyLiteral
        return $ T_Assignment id op variable index value
      else do
        when (hasLeftSpace || hasRightSpace) $
            parseNoteAt pos ErrorC 1068 "Don't put spaces around the = in assignments."
        value <- readArray <|> readNormalWord
        spacing
        return $ T_Assignment id op variable index value
  where
    readAssignmentOp =
        (string "+=" >> return Append) <|> (string "=" >> return Assign)
    readEmptyLiteral = do
        id <- getNextId
        return $ T_Literal id ""

readArrayIndex = do
    char '['
    optional space
    x <- readArithmeticContents
    char ']'
    return x

readArray = called "array assignment" $ do
    id <- getNextId
    char '('
    allspacing
    words <- readElement `reluctantlyTill` char ')'
    char ')'
    return $ T_Array id words
  where
    readElement = (readIndexed <|> readRegular) `thenSkip` allspacing
    readIndexed = do
        id <- getNextId
        index <- try $ do
            x <- readArrayIndex
            char '='
            return x
        value <- readNormalWord <|> nothing
        return $ T_IndexedElement id index value
    readRegular = readNormalWord

    nothing = do
        id <- getNextId
        return $ T_Literal id ""

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

tryWordToken s t = tryParseWordToken s t `thenSkip` spacing
tryParseWordToken keyword t = try $ do
    id <- getNextId
    str <- anycaseString keyword
    optional (do
        try . lookAhead $ char '['
        parseProblem ErrorC 1069 "You need a space before the [.")
    try $ lookAhead keywordSeparator
    when (str /= keyword) $
        parseProblem ErrorC 1081 $
            "Scripts are case sensitive. Use '" ++ keyword ++ "', not '" ++ str ++ "'."
    return $ t id

anycaseString str =
    mapM anycaseChar str <?> str
  where
    anycaseChar c = char (toLower c) <|> char (toUpper c)

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
g_Select = tryWordToken "select" T_Select
g_In = tryWordToken "in" T_In
g_Lbrace = tryWordToken "{" T_Lbrace
g_Rbrace = tryWordToken "}" T_Rbrace

g_Lparen = tryToken "(" T_Lparen
g_Rparen = tryToken ")" T_Rparen
g_Bang = do
    id <- getNextId
    char '!'
    void spacing1 <|> do
        pos <- getPosition
        parseProblemAt pos ErrorC 1035
            "You are missing a required space after the !."
    return $ T_Bang id

g_Semi = do
    notFollowedBy2 g_DSEMI
    tryToken ";" T_Semi

keywordSeparator =
    eof <|> disregard whitespace <|> disregard (oneOf ";()[<>&|")

readKeyword = choice [ g_Then, g_Else, g_Elif, g_Fi, g_Do, g_Done, g_Esac, g_Rbrace, g_Rparen, g_DSEMI ]

ifParse p t f =
    (lookAhead (try p) >> t) <|> f

prop_readShebang1 = isOk readShebang "#!/bin/sh\n"
prop_readShebang2 = isWarning readShebang "!# /bin/sh\n"
readShebang = do
    try readCorrect <|> try readSwapped
    str <- many $ noneOf "\r\n"
    optional carriageReturn
    optional linefeed
    return str
  where
    readCorrect = void $ string "#!"
    readSwapped = do
        pos <- getPosition
        string "!#"
        parseProblemAt pos ErrorC 1084
            "Use #!, not !#, for the shebang."

prop_readScript1 = isOk readScript "#!/bin/bash\necho hello world\n"
prop_readScript2 = isWarning readScript "#!/bin/bash\r\necho hello world\n"
prop_readScript3 = isWarning readScript "#!/bin/bash\necho hello\xA0world"
prop_readScript4 = isWarning readScript "#!/usr/bin/perl\nfoo=("
readScript = do
    id <- getNextId
    pos <- getPosition
    optional $ do
        readUtf8Bom
        parseProblem ErrorC 1082
            "This file has a UTF-8 BOM. Remove it with: LC_CTYPE=C sed '1s/^...//' < yourscript ."
    sb <- option "" readShebang
    verifyShell pos (getShell sb)
    if isValidShell (getShell sb) /= Just False
      then
        do {
            allspacing;
            commands <- readTerm;
            eof <|> parseProblem ErrorC 1070 "Parsing stopped here because of parsing errors.";
            return $ T_Script id sb commands;
        } <|> do {
            parseProblem WarningC 1014 "Couldn't read any commands.";
            return $ T_Script id sb []
        }
      else do
        many anyChar
        return $ T_Script id sb [];

  where
    basename s = reverse . takeWhile (/= '/') . reverse $ s
    getShell sb =
        case words sb of
            [] -> ""
            [x] -> basename x
            (first:second:_) ->
                if basename first == "env"
                    then second
                    else basename first

    verifyShell pos s =
        case isValidShell s of
            Just True -> return ()
            Just False -> parseProblemAt pos ErrorC 1071 "ShellCheck only supports sh/bash/ksh scripts. Sorry!"
            Nothing -> parseProblemAt pos InfoC 1008 "This shebang was unrecognized. Note that ShellCheck only handles sh/bash/ksh."

    isValidShell s =
        let good = s == "" || any (`isPrefixOf` s) goodShells
            bad = any (`isPrefixOf` s) badShells
        in
            if good
                then Just True
                else if bad
                        then Just False
                        else Nothing

    goodShells = [
        "sh",
        "ash",
        "dash",
        "bash",
        "ksh"
        ]
    badShells = [
        "awk",
        "csh",
        "expect",
        "perl",
        "python",
        "ruby",
        "tcsh",
        "zsh"
        ]

    readUtf8Bom = called "Byte Order Mark" $ string "\xFEFF"

rp p filename contents = Ms.runState (runParserT p initialState filename contents) ([], [])

isWarning p s = fst cs && (not . null . snd $ cs) where cs = checkString p s
isOk p s = fst cs && (null . snd $ cs) where cs = checkString p s

checkString parser string =
    case rp (parser >> eof >> getState) "-" string of
        (Right (tree, map, notes), (problems, _)) -> (True, notes ++ problems)
        (Left _, (n, _)) -> (False, n)

parseWithNotes parser = do
    item <- parser
    map <- getMap
    parseNotes <- getParseNotes
    return (item, map, nub . sortNotes $ parseNotes)

compareNotes (ParseNote pos1 level1 _ s1) (ParseNote pos2 level2 _ s2) = compare (pos1, level1) (pos2, level2)
sortNotes = sortBy compareNotes


data ParseResult = ParseResult { parseResult :: Maybe (Token, Map.Map Id SourcePos), parseNotes :: [ParseNote] } deriving (Show)

makeErrorFor parsecError =
    ParseNote (errorPos parsecError) ErrorC 1072 $
        getStringFromParsec $ errorMessages parsecError

getStringFromParsec errors =
        case map f errors of
            r -> unwords (take 1 $ catMaybes $ reverse r)  ++
                " Fix any mentioned problems and try again."
    where
        f err =
            case err of
                UnExpect s    ->  return $ unexpected s
                SysUnExpect s ->  return $ unexpected s
                Expect s      ->  return $ "Expected " ++ s ++ "."
                Message s     ->  if null s then Nothing else return $ s ++ "."
        unexpected s = "Unexpected " ++ (if null s then "eof" else s) ++ "."

parseShell options filename contents =
    case rp (parseWithNotes readScript) filename contents of
        (Right (script, map, notes), (parsenotes, _)) ->
            ParseResult (Just (script, map)) (nub . sortNotes . excludeNotes $ notes ++ parsenotes)
        (Left err, (p, context)) ->
            ParseResult Nothing
                (nub . sortNotes . excludeNotes $ p ++ notesForContext context ++ [makeErrorFor err])
  where
    isName (ContextName _ _) = True
    isName _ = False
    notesForContext list = zipWith ($) [first, second] $ filter isName list
    first (ContextName pos str) = ParseNote pos ErrorC 1073 $
        "Couldn't parse this " ++ str ++ "."
    second (ContextName pos str) = ParseNote pos InfoC 1009 $
        "The mentioned parser error was in this " ++ str ++ "."
    excludeNotes = filter (\c -> codeForParseNote c `notElem` optionExcludes options)

lt x = trace (show x) x
ltt t = trace (show t)

return []
runTests = $quickCheckAll

