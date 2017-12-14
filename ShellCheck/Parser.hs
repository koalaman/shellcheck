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
{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell, FlexibleContexts #-}
module ShellCheck.Parser (parseScript, runTests) where

import ShellCheck.AST
import ShellCheck.ASTLib
import ShellCheck.Data
import ShellCheck.Interface

import Control.Applicative ((<*))
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
import Data.Char
import Data.Functor
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, partition, sortBy, intercalate, nub)
import Data.Maybe
import Data.Monoid
import Debug.Trace
import GHC.Exts (sortWith)
import Prelude hiding (readList)
import System.IO
import Text.Parsec hiding (runParser, (<?>))
import Text.Parsec.Error
import Text.Parsec.Pos
import qualified Control.Monad.Reader as Mr
import qualified Control.Monad.State as Ms
import qualified Data.Map as Map

import Test.QuickCheck.All (quickCheckAll)

type SCBase m = Mr.ReaderT (Environment m) (Ms.StateT SystemState m)
type SCParser m v = ParsecT String UserState (SCBase m) v

backslash :: Monad m => SCParser m Char
backslash = char '\\'
linefeed :: Monad m => SCParser m Char
linefeed = do
    optional carriageReturn
    c <- char '\n'
    readPendingHereDocs
    return c
singleQuote = char '\''
doubleQuote = char '"'
variableStart = upper <|> lower <|> oneOf "_"
variableChars = upper <|> lower <|> digit <|> oneOf "_"
-- Chars to allow in function names
functionChars = variableChars <|> oneOf ":+?-./^"
-- Chars to allow in functions using the 'function' keyword
extendedFunctionChars = functionChars <|> oneOf "[]*=!"
specialVariable = oneOf "@*#?-$!"
paramSubSpecialChars = oneOf "/:+-=%"
quotableChars = "|&;<>()\\ '\t\n\r\xA0" ++ doubleQuotableChars
quotable = almostSpace <|> oneOf quotableChars
bracedQuotable = oneOf "}\"$`'"
doubleQuotableChars = "\\\"$`"
doubleQuotable = oneOf doubleQuotableChars
whitespace = oneOf " \t" <|> carriageReturn <|> almostSpace <|> linefeed
linewhitespace = oneOf " \t" <|> almostSpace

suspectCharAfterQuotes = variableChars <|> char '%'

extglobStartChars = "?*@!+"
extglobStart = oneOf extglobStartChars

unicodeDoubleQuotes = "\x201C\x201D\x2033\x2036"
unicodeSingleQuotes = "\x2018\x2019"

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

readUnicodeQuote = do
    pos <- getPosition
    c <- oneOf (unicodeSingleQuotes ++ unicodeDoubleQuotes)
    parseProblemAt pos WarningC 1110 "This is a unicode quote. Delete and retype it (or quote to make literal)."
    id <- getNextIdAt pos
    return $ T_Literal id [c]

carriageReturn = do
    parseNote ErrorC 1017 "Literal carriage return. Run script through tr -d '\\r' ."
    char '\r'

almostSpace =
    choice [
        check '\xA0' "unicode non-breaking space",
        check '\x200B' "unicode zerowidth space"
    ]
  where
    check c name = do
        parseNote ErrorC 1018 $ "This is a " ++ name ++ ". Delete and retype it."
        char c
        return ' '

--------- Message/position annotation on top of user state
data Note = Note Id Severity Code String deriving (Show, Eq)
data ParseNote = ParseNote SourcePos SourcePos Severity Code String deriving (Show, Eq)
data Context =
        ContextName SourcePos String
        | ContextAnnotation [Annotation]
        | ContextSource String
    deriving (Show)

data HereDocContext =
        HereDocPending Token -- on linefeed, read this T_HereDoc
        | HereDocBoundary -- but don't consider heredocs before this
    deriving (Show)

data UserState = UserState {
    lastId :: Id,
    positionMap :: Map.Map Id SourcePos,
    parseNotes :: [ParseNote],
    hereDocMap :: Map.Map Id [Token],
    pendingHereDocs :: [HereDocContext]
}
initialUserState = UserState {
    lastId = Id $ -1,
    positionMap = Map.empty,
    parseNotes = [],
    hereDocMap = Map.empty,
    pendingHereDocs = []
}

codeForParseNote (ParseNote _ _ _ code _) = code
noteToParseNote map (Note id severity code message) =
        ParseNote pos pos severity code message
    where
        pos = fromJust $ Map.lookup id map

getLastId = lastId <$> getState

getNextIdAt sourcepos = do
    state <- getState
    let newId = incId (lastId state)
    let newMap = Map.insert newId sourcepos (positionMap state)
    putState $ state {
        lastId = newId,
        positionMap = newMap
    }
    return newId
  where incId (Id n) = Id $ n+1

getNextId :: Monad m => SCParser m Id
getNextId = do
    pos <- getPosition
    getNextIdAt pos

addToHereDocMap id list = do
    state <- getState
    let map = hereDocMap state
    putState $ state {
        hereDocMap = Map.insert id list map
    }

withHereDocBoundary p = do
    pushBoundary
    do
        v <- p
        popBoundary
        return v
     <|> do
        popBoundary
        fail ""
  where
    pushBoundary = do
        state <- getState
        let docs = pendingHereDocs state
        putState $ state {
            pendingHereDocs = HereDocBoundary : docs
        }
    popBoundary = do
        state <- getState
        let docs = tail $ dropWhile (not . isHereDocBoundary) $
                    pendingHereDocs state
        putState $ state {
            pendingHereDocs = docs
        }

addPendingHereDoc t = do
    state <- getState
    let docs = pendingHereDocs state
    putState $ state {
        pendingHereDocs = HereDocPending t : docs
    }

popPendingHereDocs = do
    state <- getState
    let (pending, boundary) = break isHereDocBoundary $ pendingHereDocs state
    putState $ state {
        pendingHereDocs = boundary
    }
    return . map extract . reverse $ pendingHereDocs state
  where
    extract (HereDocPending t) = t

isHereDocBoundary x = case x of
    HereDocBoundary -> True
    otherwise -> False

getMap = positionMap <$> getState
getParseNotes = parseNotes <$> getState

addParseNote n = do
    irrelevant <- shouldIgnoreCode (codeForParseNote n)
    unless irrelevant $ do
        state <- getState
        putState $ state {
            parseNotes = n : parseNotes state
        }

shouldIgnoreCode code = do
    context <- getCurrentContexts
    checkSourced <- Mr.asks checkSourced
    return $ any (disabling checkSourced) context
  where
    disabling checkSourced item =
        case item of
            ContextAnnotation list -> any disabling' list
            ContextSource _ -> not $ checkSourced
            _ -> False
    disabling' (DisableComment n) = code == n
    disabling' _ = False

shouldFollow file = do
    context <- getCurrentContexts
    if any isThisFile context
      then return False
      else
        if length (filter isSource context) >= 100
          then do
            parseProblem ErrorC 1092 "Stopping at 100 'source' frames :O"
            return False
          else
            return True
  where
    isSource (ContextSource _) = True
    isSource _ = False
    isThisFile (ContextSource name) | name == file = True
    isThisFile _= False

getSourceOverride = do
    context <- getCurrentContexts
    return . msum . map findFile $ takeWhile isSameFile context
  where
    isSameFile (ContextSource _) = False
    isSameFile _ = True

    findFile (ContextAnnotation list) = msum $ map getFile list
    findFile _ = Nothing
    getFile (SourceOverride str) = Just str
    getFile _ = Nothing

-- Store potential parse problems outside of parsec

data SystemState = SystemState {
    contextStack :: [Context],
    parseProblems :: [ParseNote]
}
initialSystemState = SystemState {
    contextStack = [],
    parseProblems = []
}

data Environment m = Environment {
    systemInterface :: SystemInterface m,
    checkSourced :: Bool
}

parseProblem level code msg = do
    pos <- getPosition
    parseProblemAt pos level code msg

setCurrentContexts c = Ms.modify (\state -> state { contextStack = c })
getCurrentContexts = contextStack <$> Ms.get

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

parseProblemAtWithEnd start end level code msg = do
    irrelevant <- shouldIgnoreCode code
    unless irrelevant $
        Ms.modify (\state -> state {
            parseProblems = note:parseProblems state
        })
  where
    note = ParseNote start end level code msg

parseProblemAt pos = parseProblemAtWithEnd pos pos

parseProblemAtId :: Monad m => Id -> Severity -> Integer -> String -> SCParser m ()
parseProblemAtId id level code msg = do
    map <- getMap
    let pos = Map.findWithDefault
                (error "Internal error (no position for id). Please report.") id map
    parseProblemAt pos level code msg

-- Store non-parse problems inside

parseNote c l a = do
    pos <- getPosition
    parseNoteAt pos c l a

parseNoteAt pos c l a = addParseNote $ ParseNote pos pos c l a

parseNoteAtWithEnd start end c l a = addParseNote $ ParseNote start end c l a

--------- Convenient combinators
thenSkip main follow = do
    r <- main
    optional follow
    return r

unexpecting s p = try $
    (try p >> fail ("Unexpected " ++ s)) <|> return ()

notFollowedBy2 = unexpecting ""

reluctantlyTill p end =
    (lookAhead (void (try end) <|> eof) >> return []) <|> do
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

-- Construct a node with a parser, e.g. T_Literal `withParser` (readGenericLiteral ",")
withParser node parser = do
    id <- getNextId
    contents <- parser
    return $ node id contents

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
                                    parseProblemAt pos WarningC 1014 "Use 'if cmd; then ..' to check exit code, or 'if [[ $(cmd) == .. ]]' to check output.")

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
        op <- getOp
        spacingOrLf
        return op
      where
        flaglessOps = [ "==", "!=", "<=", ">=", "=~", ">", "<", "=" ]

        getOp = do
            id <- getNextId
            op <- readRegularOrEscaped anyOp
            return $ TC_Binary id typ op

        anyOp = flagOp <|> flaglessOp <|> fail
                    "Expected comparison operator (don't wrap commands in []/[[]])"
        flagOp = try $ do
            s <- readOp
            when (s == "-a" || s == "-o") $ fail "Unexpected operator"
            return s
        flaglessOp =
            choice $ map (try . string) flaglessOps

        -- hacks to read quoted operators without having to read a shell word
    readEscaped p = try $ withEscape <|> withQuotes
      where
        withEscape = do
            char '\\'
            escaped <$> p
        withQuotes = do
            c <- oneOf "'\""
            s <- p
            char c
            return $ escaped s
        escaped s = if any (`elem` s) "<>()" then '\\':s else s

    readRegularOrEscaped p = readEscaped p <|> p


    guardArithmetic = do
        try . lookAhead $ void (oneOf "+*/%") <|> void (string "- ")
        parseProblem ErrorC 1076 $
            if single
            then "Trying to do math? Use e.g. [ $((i/2+7)) -ge 18 ]."
            else "Trying to do math? Use e.g. [[ $((i/2+7)) -ge 18 ]]."

    readCondUnaryExp = do
      op <- readCondUnaryOp
      pos <- getPosition
      liftM op readCondWord `orFail` do
          parseProblemAt pos ErrorC 1019 "Expected this to be an argument to the unary condition."
          return "Expected an argument for the unary operator"

    readCondUnaryOp = try $ do
        id <- getNextId
        s <- readOp
        spacingOrLf
        return $ TC_Unary id typ s

    readOp = try $ do
        char '-' <|> weirdDash
        s <- many1 letter <|> fail "Expected a test operator"
        return ('-':s)

    weirdDash = do
        pos <- getPosition
        oneOf "\x058A\x05BE\x2010\x2011\x2012\x2013\x2014\x2015\xFE63\xFF0D"
        parseProblemAt pos ErrorC 1100
            "This is a unicode dash. Delete and retype as ASCII minus."
        return '-'

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
        void spacing
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
        optional $ lookAhead weirdDash
        x <- string op
        condSpacing requiresSpacing
        return x

    readCondNullaryOrBinary = do
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
          ) <|> ( do
            checkTrailingOp x
            return $ TC_Nullary id typ x
          )

    checkTrailingOp x = fromMaybe (return ()) $ do
        (T_Literal id str) <- getTrailingUnquotedLiteral x
        trailingOp <- listToMaybe (filter (`isSuffixOf` str) binaryTestOps)
        return $ parseProblemAtId id ErrorC 1108 $
            "You need a space before and after the " ++ trailingOp ++ " ."

    readCondGroup = do
        id <- getNextId
        pos <- getPosition
        lparen <- try $ readRegularOrEscaped (string "(")
        when (single && lparen == "(") $
            singleWarning pos
        when (not single && lparen == "\\(") $
            doubleWarning pos
        condSpacing single
        x <- readCondContents
        cpos <- getPosition
        rparen <- readRegularOrEscaped (string ")")
        condSpacing single
        when (single && rparen == ")") $
            singleWarning cpos
        when (not single && rparen == "\\)") $
            doubleWarning cpos
        return $ TC_Group id typ x

      where
        singleWarning pos =
            parseProblemAt pos ErrorC 1028 "In [..] you have to escape \\( \\) or preferably combine [..] expressions."
        doubleWarning pos =
            parseProblemAt pos ErrorC 1029 "In [[..]] you shouldn't escape ( or )."


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
        void spacing
        return $ T_NormalWord id parts
      where
        readGlobLiteral = do
            id <- getNextId
            s <- extglobStart <|> oneOf "{}[]$"
            return $ T_Literal id [s]
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
      readCondGroup <|> readCondUnaryExp <|> readCondNullaryOrBinary

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
prop_a19= isOk readArithmeticContents "\\\n3 +\\\n  2"
prop_a20= isOk readArithmeticContents "a ? b ? c : d : e"
prop_a21= isOk readArithmeticContents "a ? b : c ? d : e"
prop_a22= isOk readArithmeticContents "!!a"
readArithmeticContents :: Monad m => SCParser m Token
readArithmeticContents =
    readSequence
  where
    spacing =
        let lf = try (string "\\\n") >> return '\n'
        in many (whitespace <|> lf)

    splitBy x ops = chainl1 x (readBinary ops)
    readBinary ops = readComboOp ops TA_Binary
    readComboOp op token = do
        id <- getNextId
        op <- choice (map (\x -> try $ do
                                        s <- string x
                                        failIfIncompleteOp
                                        return s
                            ) op)
        spacing
        return $ token id op

    failIfIncompleteOp = notFollowedBy2 $ oneOf "&|<>="

    -- Read binary minus, but also check for -lt, -gt and friends:
    readMinusOp = do
        id <- getNextId
        pos <- getPosition
        try $ do
            char '-'
            failIfIncompleteOp
        optional $ do
            (str, alt) <- lookAhead . choice $ map tryOp [
                ("lt", "<"),
                ("gt", ">"),
                ("le", "<="),
                ("ge", ">="),
                ("eq", "=="),
                ("ne", "!=")
              ]
            parseProblemAt pos ErrorC 1106 $ "In arithmetic contexts, use " ++ alt ++ " instead of -" ++ str
        spacing
        return $ TA_Binary id "-"
      where
        tryOp (str, alt) = try $ do
            string str
            spacing1
            return (str, alt)


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
            readUnquotedBackTicked,
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

    readAssignment = chainr1 readTrinary readAssignmentOp
    readAssignmentOp = readComboOp ["=", "*=", "/=", "%=", "+=", "-=", "<<=", ">>=", "&=", "^=", "|="] TA_Assignment

    readTrinary = do
        x <- readLogicalOr
        do
            id <- getNextId
            string "?"
            spacing
            y <- readTrinary
            string ":"
            spacing
            z <- readTrinary
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
    readAddition = chainl1 readMultiplication (readBinary ["+"] <|> readMinusOp)
    readMultiplication = readExponential `splitBy` ["*", "/", "%"]
    readExponential = readAnyNegated `splitBy` ["**"]

    readAnyNegated = readNegated <|> readAnySigned
    readNegated = do
        id <- getNextId
        op <- oneOf "!~"
        spacing
        x <- readAnyNegated
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
prop_readCondition14= isOk readCondition "[ foo '>' bar ]"
prop_readCondition15= isOk readCondition "[ foo \">=\" bar ]"
prop_readCondition16= isOk readCondition "[ foo \\< bar ]"
prop_readCondition17= isOk readCondition "[[ ${file::1} = [-.\\|/\\\\] ]]"
prop_readCondition18= isOk readCondition "[ ]"
prop_readCondition19= isOk readCondition "[ '(' x \")\" ]"
readCondition = called "test expression" $ do
    opos <- getPosition
    id <- getNextId
    open <- try (string "[[") <|> string "["
    let single = open == "["
    let typ = if single then SingleBracket else DoubleBracket

    pos <- getPosition
    space <- allspacing
    when (null space) $
        parseProblemAtWithEnd opos pos ErrorC 1035 $ "You need a space after the " ++
            if single
                then "[ and before the ]."
                else "[[ and before the ]]."
    when (single && '\n' `elem` space) $
        parseProblemAt pos ErrorC 1080 "You need \\ before line feeds to break lines in [ ]."

    condition <- readConditionContents single <|> do
        guard . not . null $ space
        lookAhead $ string "]"
        id <- getNextIdAt pos
        return $ TC_Empty id typ

    cpos <- getPosition
    close <- try (string "]]") <|> string "]" <|> fail "Expected test to end here (don't wrap commands in []/[[]])"
    when (open == "[[" && close /= "]]") $ parseProblemAt cpos ErrorC 1033 "Did you mean ]] ?"
    when (open == "[" && close /= "]" ) $ parseProblemAt opos ErrorC 1034 "Did you mean [[ ?"
    spacing
    many readCmdWord -- Read and throw away remainders to get then/do warnings. Fixme?
    return $ T_Condition id typ condition

readAnnotationPrefix = do
    char '#'
    many linewhitespace
    string "shellcheck"

prop_readAnnotation1 = isOk readAnnotation "# shellcheck disable=1234,5678\n"
prop_readAnnotation2 = isOk readAnnotation "# shellcheck disable=SC1234 disable=SC5678\n"
prop_readAnnotation3 = isOk readAnnotation "# shellcheck disable=SC1234 source=/dev/null disable=SC5678\n"
prop_readAnnotation4 = isWarning readAnnotation "# shellcheck cats=dogs disable=SC1234\n"
prop_readAnnotation5 = isOk readAnnotation "# shellcheck disable=SC2002 # All cats are precious\n"
prop_readAnnotation6 = isOk readAnnotation "# shellcheck disable=SC1234 # shellcheck foo=bar\n"
readAnnotation = called "shellcheck directive" $ do
    try readAnnotationPrefix
    many1 linewhitespace
    values <- many1 readKey
    optional readAnyComment
    void linefeed <|> do
        parseNote ErrorC 1125 "Invalid key=value pair? Ignoring the rest of this directive starting here."
        many (noneOf "\n")
        void linefeed <|> eof
    many linewhitespace
    return $ concat values
  where
    readKey = do
        keyPos <- getPosition
        key <- many1 letter
        char '=' <|> fail "Expected '=' after directive key"
        annotations <- case key of
            "disable" -> readCode `sepBy` char ','
              where
                readCode = do
                    optional $ string "SC"
                    int <- many1 digit
                    return $ DisableComment (read int)

            "source" -> do
                filename <- many1 $ noneOf " \n"
                return [SourceOverride filename]

            "shell" -> do
                pos <- getPosition
                shell <- many1 $ noneOf " \n"
                when (isNothing $ shellForExecutable shell) $
                    parseNoteAt pos ErrorC 1103
                        "This shell type is unknown. Use e.g. sh or bash."
                return [ShellOverride shell]

            _ -> do
                parseNoteAt keyPos WarningC 1107 "This directive is unknown. It will be ignored."
                anyChar `reluctantlyTill` whitespace
                return []

        many linewhitespace
        return annotations

readAnnotations = do
    annotations <- many (readAnnotation `thenSkip` allspacing)
    return $ concat annotations

readComment = do
    unexpecting "shellcheck annotation" readAnnotationPrefix
    readAnyComment

readAnyComment = do
    char '#'
    many $ noneOf "\r\n"

prop_readNormalWord = isOk readNormalWord "'foo'\"bar\"{1..3}baz$(lol)"
prop_readNormalWord2 = isOk readNormalWord "foo**(foo)!!!(@@(bar))"
prop_readNormalWord3 = isOk readNormalWord "foo#"
prop_readNormalWord4 = isOk readNormalWord "$\"foo\"$'foo\nbar'"
prop_readNormalWord5 = isWarning readNormalWord "${foo}}"
prop_readNormalWord6 = isOk readNormalWord "foo/{}"
prop_readNormalWord7 = isOk readNormalWord "foo\\\nbar"
prop_readNormalWord8 = isWarning readSubshell "(foo\\ \nbar)"
prop_readNormalWord9 = isOk readSubshell "(foo\\ ;\nbar)"
prop_readNormalWord10 = isWarning readNormalWord "\x201Chello\x201D"
prop_readNormalWord11 = isWarning readNormalWord "\x2018hello\x2019"
prop_readNormalWord12 = isWarning readNormalWord "hello\x2018"
readNormalWord = readNormalishWord ""

readNormalishWord end = do
    id <- getNextId
    pos <- getPosition
    x <- many1 (readNormalWordPart end)
    checkPossibleTermination pos x
    return $ T_NormalWord id x

readIndexSpan = do
    id <- getNextId
    x <- many (readNormalWordPart "]" <|> someSpace <|> otherLiteral)
    return $ T_NormalWord id x
  where
    someSpace = do
        id <- getNextId
        str <- spacing1
        return $ T_Literal id str
    otherLiteral = do
        id <- getNextId
        str <- many1 $ oneOf quotableChars
        return $ T_Literal id str

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
        readUnquotedBackTicked,
        readProcSub,
        readUnicodeQuote,
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

readDollarBracedPart = readSingleQuoted <|> readDoubleQuoted <|>
                       readParamSubSpecialChar <|> readExtglob <|> readNormalDollar <|>
                       readUnquotedBackTicked <|> readDollarBracedLiteral

readDollarBracedLiteral = do
    id <- getNextId
    vars <- (readBraceEscaped <|> (anyChar >>= \x -> return [x])) `reluctantlyTill1` bracedQuotable
    return $ T_Literal id $ concat vars

readParamSubSpecialChar = do
    id <- getNextId
    T_ParamSubSpecialChar id <$> many1 paramSubSpecialChars

prop_readProcSub1 = isOk readProcSub "<(echo test | wc -l)"
prop_readProcSub2 = isOk readProcSub "<(  if true; then true; fi )"
prop_readProcSub3 = isOk readProcSub "<( # nothing here \n)"
readProcSub = called "process substitution" $ do
    id <- getNextId
    dir <- try $ do
                    x <- oneOf "<>"
                    char '('
                    return [x]
    list <- readCompoundListOrEmpty
    allspacing
    char ')'
    return $ T_ProcSub id dir list

prop_readSingleQuoted = isOk readSingleQuoted "'foo bar'"
prop_readSingleQuoted2 = isWarning readSingleQuoted "'foo bar\\'"
prop_readSingleQuoted4 = isWarning readNormalWord "'it's"
prop_readSingleQuoted5 = isWarning readSimpleCommand "foo='bar\ncow 'arg"
prop_readSingleQuoted6 = isOk readSimpleCommand "foo='bar cow 'arg"
prop_readSingleQuoted7 = isOk readSingleQuoted "'foo\x201C\&bar'"
prop_readSingleQuoted8 = isWarning readSingleQuoted "'foo\x2018\&bar'"
readSingleQuoted = called "single quoted string" $ do
    id <- getNextId
    startPos <- getPosition
    singleQuote
    s <- many readSingleQuotedPart
    let string = concat s
    endPos <- getPosition
    singleQuote <|> fail "Expected end of single quoted string"

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
    <|> many1 (noneOf $ "'\\" ++ unicodeSingleQuotes)
    <|> readUnicodeQuote
   where
    readUnicodeQuote = do
        pos <- getPosition
        x <- oneOf unicodeSingleQuotes
        parseProblemAt pos WarningC 1112
            "This is a unicode quote. Delete and retype it (or ignore/doublequote for literal)."
        return [x]


prop_readBackTicked = isOk (readBackTicked False) "`ls *.mp3`"
prop_readBackTicked2 = isOk (readBackTicked False) "`grep \"\\\"\"`"
prop_readBackTicked3 = isWarning (readBackTicked False) "´grep \"\\\"\"´"
prop_readBackTicked4 = isOk readSimpleCommand "`echo foo\necho bar`"
prop_readBackTicked5 = isOk readSimpleCommand "echo `foo`bar"
prop_readBackTicked6 = isWarning readSimpleCommand "echo `foo\necho `bar"
prop_readBackTicked7 = isOk readSimpleCommand "`#inline comment`"
prop_readBackTicked8 = isOk readSimpleCommand "echo `#comment` \\\nbar baz"
readQuotedBackTicked = readBackTicked True
readUnquotedBackTicked = readBackTicked False
readBackTicked quoted = called "backtick expansion" $ do
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
    result <- subParse subStart subParser (unEscape subString)
    return $ T_Backticked id result
  where
    unEscape [] = []
    unEscape ('\\':'"':rest) | quoted = '"' : unEscape rest
    unEscape ('\\':x:rest) | x `elem` "$`\\" = x : unEscape rest
    unEscape ('\\':'\n':rest) = unEscape rest
    unEscape (c:rest) = c : unEscape rest
    subParser = do
        cmds <- readCompoundListOrEmpty
        verifyEof
        return cmds
    backtick =
      void (char '`') <|> do
         pos <- getPosition
         char '´'
         parseProblemAt pos ErrorC 1077
            "For command expansion, the tick should slant left (` vs ´). Use $(..) instead."

subParse pos parser input = do
    lastPosition <- getPosition
    lastInput <- getInput
    setPosition pos
    setInput input
    result <- parser
    setInput lastInput
    setPosition lastPosition
    return result

-- Parse something, but forget all parseProblems
inSeparateContext = parseForgettingContext True
-- Parse something, but forget all parseProblems on failure
forgetOnFailure = parseForgettingContext False

parseForgettingContext alsoOnSuccess parser = do
    context <- Ms.get
    success context <|> failure context
  where
    success c = do
        res <- try parser
        when alsoOnSuccess $ Ms.put c
        return res
    failure c = do
        Ms.put c
        fail ""

prop_readDoubleQuoted = isOk readDoubleQuoted "\"Hello $FOO\""
prop_readDoubleQuoted2 = isOk readDoubleQuoted "\"$'\""
prop_readDoubleQuoted3 = isOk readDoubleQuoted "\"\x2018hello\x2019\""
prop_readDoubleQuoted4 = isWarning readSimpleCommand "\"foo\nbar\"foo"
prop_readDoubleQuoted5 = isOk readSimpleCommand "lol \"foo\nbar\" etc"
prop_readDoubleQuoted6 = isOk readSimpleCommand "echo \"${ ls; }\""
prop_readDoubleQuoted7 = isOk readSimpleCommand "echo \"${ ls;}bar\""
prop_readDoubleQuoted8 = isWarning readDoubleQuoted "\"\x201Chello\x201D\""
prop_readDoubleQuoted9 = isWarning readDoubleQuoted "\"foo\\n\""
prop_readDoubleQuoted10 = isOk readDoubleQuoted "\"foo\\\\n\""
readDoubleQuoted = called "double quoted string" $ do
    id <- getNextId
    startPos <- getPosition
    doubleQuote
    x <- many doubleQuotedPart
    endPos <- getPosition
    doubleQuote <|> fail "Expected end of double quoted string"
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

doubleQuotedPart = readDoubleLiteral <|> readDoubleQuotedDollar <|> readQuotedBackTicked <|> readUnicodeQuote
  where
    readUnicodeQuote = do
        pos <- getPosition
        id <- getNextId
        c <- oneOf unicodeDoubleQuotes
        parseProblemAt pos WarningC 1111
            "This is a unicode quote. Delete and retype it (or ignore/singlequote for literal)."
        return $ T_Literal id [c]

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
    x <- many1 (readDoubleEscaped <|> many1 (noneOf (doubleQuotableChars ++ unicodeDoubleQuotes)))
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
prop_readGlob6 = isOk readGlob "[\\|]"
prop_readGlob7 = isOk readGlob "[^[]"
prop_readGlob8 = isOk readGlob "[*?]"
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
            s <- many1 (predefined <|> readNormalLiteralPart "]" <|> globchars)
            char ']'
            return $ T_Glob id $ "[" ++ concat s ++ "]"
          where
           globchars = liftM return . oneOf $ "!$[" ++ extglobStartChars
           predefined = do
              try $ string "[:"
              s <- many1 letter
              string ":]"
              return $ "[:" ++ s ++ ":]"

        readGlobbyLiteral = do
            id <- getNextId
            c <- extglobStart <|> char '['
            return $ T_Literal id [c]

readNormalLiteralPart customEnd =
    readNormalEscaped <|>
        many1 (noneOf (customEnd ++ standardEnd))
  where
    standardEnd = "[{}"
        ++ quotableChars
        ++ extglobStartChars
        ++ unicodeDoubleQuotes
        ++ unicodeSingleQuotes

readNormalEscaped = called "escaped char" $ do
    pos <- getPosition
    backslash
    do
        next <- quotable <|> oneOf "?*@!+[]{}.,~#"
        when (next == ' ') $ checkTrailingSpaces pos <|> return ()
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

    checkTrailingSpaces pos = lookAhead . try $ do
        many linewhitespace
        void linefeed <|> eof
        parseProblemAt pos ErrorC 1101 "Delete trailing spaces after \\ to break line (or use quotes for literal space)."


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
    pos <- getPosition
    s <- backslash
    x <- lookAhead anyChar

    case x of
        '\'' -> parseProblemAt pos InfoC 1003 "Want to escape a single quote? echo 'This is how it'\\''s done'.";
        '\n' -> parseProblemAt pos InfoC 1004 "This backslash+linefeed is literal. Break outside single quotes if you just want to break the line."
        _ -> return ()

    return [s]

readDoubleEscaped = do
    pos <- getPosition
    bs <- backslash
    (linefeed >> return "")
        <|> liftM return doubleQuotable
        <|> do
            c <- anyChar
            parseNoteAt pos StyleC 1117 $
                "Backslash is literal in \"\\" ++ [c] ++ "\". Prefer explicit escaping: \"\\\\" ++ [c] ++ "\"."
            return [bs, c]

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
prop_readBraced3 = isOk readBraced "{1,\\},2}"
prop_readBraced4 = isOk readBraced "{1,{2,3}}"
prop_readBraced5 = isOk readBraced "{JP{,E}G,jp{,e}g}"
prop_readBraced6 = isOk readBraced "{foo,bar,$((${var}))}"
prop_readBraced7 = isNotOk readBraced "{}"
prop_readBraced8 = isNotOk readBraced "{foo}"
readBraced = try braceExpansion
  where
    braceExpansion =
        T_BraceExpansion `withParser` do
            char '{'
            elements <- bracedElement `sepBy1` char ','
            guard $
                case elements of
                    (_:_:_) -> True
                    [t] -> ".." `isInfixOf` onlyLiteralString t
                    [] -> False
            char '}'
            return elements
    bracedElement =
        T_NormalWord `withParser` do
            many $ choice [
                braceExpansion,
                readDollarExpression,
                readSingleQuoted,
                readDoubleQuoted,
                braceLiteral
                ]
    braceLiteral =
        T_Literal `withParser` readGenericLiteral1 (oneOf "{}\"$'," <|> whitespace)

ensureDollar =
    -- The grammar should have been designed along the lines of readDollarExpr = char '$' >> stuff, but
    -- instead, each subunit parses its own $. This results in ~7 1-3 char lookaheads instead of one 1-char.
    -- Instead of optimizing the grammar, here's a green cut that decreases shellcheck runtime by 10%:
    lookAhead $ char '$'

readNormalDollar = do
    ensureDollar
    readDollarExp <|> readDollarDoubleQuote <|> readDollarSingleQuote <|> readDollarLonely
readDoubleQuotedDollar = do
    ensureDollar
    readDollarExp <|> readDollarLonely


prop_readDollarExpression1 = isOk readDollarExpression "$(((1) && 3))"
prop_readDollarExpression2 = isWarning readDollarExpression "$(((1)) && 3)"
prop_readDollarExpression3 = isWarning readDollarExpression "$((\"$@\" &); foo;)"
readDollarExpression :: Monad m => SCParser m Token
readDollarExpression = do
    ensureDollar
    readDollarExp

readDollarExp = arithmetic <|> readDollarExpansion <|> readDollarBracket <|> readDollarBraceCommandExpansion <|> readDollarBraced <|> readDollarVariable
  where
    arithmetic = readAmbiguous "$((" readDollarArithmetic readDollarExpansion (\pos ->
        parseNoteAt pos WarningC 1102 "Shells disambiguate $(( differently or not at all. For $(command substition), add space after $( . For $((arithmetics)), fix parsing errors.")

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
    doubleQuote <|> fail "Expected end of translated double quoted string"
    return $ T_DollarDoubleQuoted id x

prop_readDollarArithmetic = isOk readDollarArithmetic "$(( 3 * 4 +5))"
prop_readDollarArithmetic2 = isOk readDollarArithmetic "$(((3*4)+(1*2+(3-1))))"
readDollarArithmetic = called "$((..)) expression" $ do
    id <- getNextId
    try (string "$((")
    c <- readArithmeticContents
    pos <- getPosition
    char ')'
    char ')' <|> fail "Expected a double )) to end the $((..))"
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

-- If the next characters match prefix, try two different parsers and warn if the alternate parser had to be used
readAmbiguous :: Monad m => String -> SCParser m p -> SCParser m p -> (SourcePos -> SCParser m ()) -> SCParser m p
readAmbiguous prefix expected alternative warner = do
    pos <- getPosition
    try . lookAhead $ string prefix
    -- If the expected parser fails, try the alt.
    -- If the alt fails, run the expected one again for the errors.
    try expected <|> try (withAlt pos) <|> expected
  where
    withAlt pos = do
        t <- forgetOnFailure alternative
        warner pos
        return t

prop_readDollarBraceCommandExpansion1 = isOk readDollarBraceCommandExpansion "${ ls; }"
prop_readDollarBraceCommandExpansion2 = isOk readDollarBraceCommandExpansion "${\nls\n}"
readDollarBraceCommandExpansion = called "ksh ${ ..; } command expansion" $ do
    id <- getNextId
    try $ do
        string "${"
        whitespace
    allspacing
    term <- readTerm
    char '}' <|> fail "Expected } to end the ksh ${ ..; } command expansion"
    return $ T_DollarBraceCommandExpansion id term

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

prop_readDollarExpansion1= isOk readDollarExpansion "$(echo foo; ls\n)"
prop_readDollarExpansion2= isOk readDollarExpansion "$(  )"
prop_readDollarExpansion3= isOk readDollarExpansion "$( command \n#comment \n)"
readDollarExpansion = called "command expansion" $ do
    id <- getNextId
    try (string "$(")
    cmds <- readCompoundListOrEmpty
    char ')' <|> fail "Expected end of $(..) expression"
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

prop_readHereDoc = isOk readScript "cat << foo\nlol\ncow\nfoo"
prop_readHereDoc2 = isWarning readScript "cat <<- EOF\n  cow\n  EOF"
prop_readHereDoc3 = isOk readScript "cat << foo\n$\"\nfoo"
prop_readHereDoc4 = isOk readScript "cat << foo\n`\nfoo"
prop_readHereDoc5 = isOk readScript "cat <<- !foo\nbar\n!foo"
prop_readHereDoc6 = isOk readScript "cat << foo\\ bar\ncow\nfoo bar"
prop_readHereDoc7 = isOk readScript "cat << foo\n\\$(f ())\nfoo"
prop_readHereDoc8 = isOk readScript "cat <<foo>>bar\netc\nfoo"
prop_readHereDoc9 = isOk readScript "if true; then cat << foo; fi\nbar\nfoo\n"
prop_readHereDoc10= isOk readScript "if true; then cat << foo << bar; fi\nfoo\nbar\n"
prop_readHereDoc11= isOk readScript "cat << foo $(\nfoo\n)lol\nfoo\n"
prop_readHereDoc12= isOk readScript "cat << foo|cat\nbar\nfoo"
prop_readHereDoc13= isOk readScript "cat <<'#!'\nHello World\n#!\necho Done"
prop_readHereDoc14= isWarning readScript "cat << foo\nbar\nfoo \n"
prop_readHereDoc15= isWarning readScript "cat <<foo\nbar\nfoo bar\n"
prop_readHereDoc16= isOk readScript "cat <<- ' foo'\nbar\n foo\n"
prop_readHereDoc17= isWarning readScript "cat <<- ' foo'\nbar\n  foo\n"
readHereDoc = called "here document" $ do
    fid <- getNextId
    pos <- getPosition
    try $ string "<<"
    dashed <- (char '-' >> return Dashed) <|> return Undashed
    sp <- spacing
    optional $ do
        try . lookAhead $ char '('
        let message = "Shells are space sensitive. Use '< <(cmd)', not '<<" ++ sp ++ "(cmd)'."
        parseProblemAt pos ErrorC 1038 message
    hid <- getNextId
    (quoted, endToken) <- readToken

    -- add empty tokens for now, read the rest in readPendingHereDocs
    let doc = T_HereDoc hid dashed quoted endToken []
    addPendingHereDoc doc
    return doc
  where
    quotes = "\"'\\"
    -- Fun fact: bash considers << foo"" quoted, but not << <("foo").
    -- Instead of replicating this, just read a token and strip quotes.
    readToken = do
        str <- readStringForParser readNormalWord
        return (if any (`elem` quotes) str then Quoted else Unquoted,
                filter (not . (`elem` quotes)) str)


readPendingHereDocs = do
    docs <- popPendingHereDocs
    mapM_ readDoc docs
  where
    readDoc (T_HereDoc id dashed quoted endToken _) = do
        pos <- getPosition
        hereData <- concat <$> rawLine `reluctantlyTill` do
                        linewhitespace `reluctantlyTill` string endToken
                        string endToken
                        void linewhitespace <|> void (oneOf "\n;&#)") <|> eof
        do
            spaces <- linewhitespace `reluctantlyTill` string endToken
            verifyHereDoc dashed quoted spaces hereData
            string endToken
            trailingPos <- getPosition
            trailers <- lookAhead $ many (noneOf "\n")
            let ppt = parseProblemAt trailingPos ErrorC
            unless (null trailers) $
                if all isSpace trailers
                then ppt 1118 "Delete whitespace after the here-doc end token."
                else case (head $ dropWhile isSpace trailers) of
                    ')' -> ppt 1119 $ "Add a linefeed between end token and terminating ')'."
                    '#' -> ppt 1120 "No comments allowed after here-doc token. Comment the next line instead."
                    c | c `elem` ";&" ->
                        ppt 1121 "Add ;/& terminators (and other syntax) on the line with the <<, not here."
                    _ -> ppt 1122 "Nothing allowed after end token. To continue a command, put it on the line with the <<."
            parsedData <- parseHereData quoted pos hereData
            list <- parseHereData quoted pos hereData
            addToHereDocMap id list

         `attempting` (eof >> debugHereDoc pos endToken hereData)

    rawLine = do
        c <- many $ noneOf "\n"
        void (char '\n') <|> eof
        return $ c ++ "\n"

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
                      parseProblemAt pos ErrorC 1042 ("Close matches include '" ++ line ++ "' (!= '" ++ endToken ++ "').")
            in do
                  parseProblemAt pos ErrorC 1041 ("Found '" ++ endToken ++ "' further down, but not on a separate line.")
                  mapM_ lookAt (lines doc)
        | map toLower endToken `isInfixOf` map toLower doc =
            parseProblemAt pos ErrorC 1043 ("Found " ++ endToken ++ " further down, but with wrong casing.")
        | otherwise =
            parseProblemAt pos ErrorC 1044 ("Couldn't find end token `" ++ endToken ++ "' in the here document.")


readFilename = readNormalWord
readIoFileOp = choice [g_DGREAT, g_LESSGREAT, g_GREATAND, g_LESSAND, g_CLOBBER, redirToken '<' T_Less, redirToken '>' T_Greater ]

readIoDuplicate = try $ do
    id <- getNextId
    op <- g_GREATAND <|> g_LESSAND
    target <- readIoVariable <|> many1 digit <|> string "-"
    return $ T_IoDuplicate id op target

prop_readIoFile = isOk readIoFile ">> \"$(date +%YYmmDD)\""
readIoFile = called "redirection" $ do
    id <- getNextId
    op <- readIoFileOp
    spacing
    file <- readFilename
    return $ T_IoFile id op file

readIoVariable = try $ do
    char '{'
    x <- readVariableName
    char '}'
    return $ "{" ++ x ++ "}"

readIoSource = try $ do
    x <- string "&" <|> readIoVariable <|> many digit
    lookAhead $ void readIoFileOp <|> void (string "<<")
    return x

prop_readIoRedirect = isOk readIoRedirect "3>&2"
prop_readIoRedirect2 = isOk readIoRedirect "2> lol"
prop_readIoRedirect3 = isOk readIoRedirect "4>&-"
prop_readIoRedirect4 = isOk readIoRedirect "&> lol"
prop_readIoRedirect5 = isOk readIoRedirect "{foo}>&2"
prop_readIoRedirect6 = isOk readIoRedirect "{foo}<&-"
readIoRedirect = do
    id <- getNextId
    n <- readIoSource
    redir <- readHereString <|> readHereDoc <|> readIoDuplicate <|> readIoFile
    skipAnnotationAndWarn
    spacing
    return $ T_FdRedirect id n redir

readRedirectList = many1 readIoRedirect

prop_readHereString = isOk readHereString "<<< \"Hello $world\""
readHereString = called "here string" $ do
    id <- getNextId
    try $ string "<<<"
    spacing
    id2 <- getNextId
    word <- readNormalWord
    return $ T_HereString id2 word

readNewlineList = many1 ((linefeed <|> carriageReturn) `thenSkip` spacing)
readLineBreak = optional readNewlineList

prop_readSeparator1 = isWarning readScript "a &; b"
prop_readSeparator2 = isOk readScript "a & b"
prop_readSeparator3 = isWarning readScript "a &amp; b"
prop_readSeparator4 = isWarning readScript "a &gt; file; b"
readSeparatorOp = do
    notFollowedBy2 (void g_AND_IF <|> void readCaseSeparator)
    notFollowedBy2 (string "&>")
    f <- try (do
                    pos <- getPosition
                    char '&'
                    optional $ do
                        s <- lookAhead . choice . map (try . string) $
                            ["amp;", "gt;", "lt;"]
                        parseProblemAt pos ErrorC 1109 "This is an unquoted HTML entity. Replace with corresponding character."

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

readSequentialSep = void (g_Semi >> readLineBreak) <|> void readNewlineList
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
prop_readSimpleCommand7 = isOk readSimpleCommand "\\ls"
readSimpleCommand = called "simple command" $ do
    pos <- getPosition
    id1 <- getNextId
    id2 <- getNextId
    prefix <- option [] readCmdPrefix
    skipAnnotationAndWarn
    cmd <- option Nothing $ do { f <- readCmdName; return $ Just f; }
    when (null prefix && isNothing cmd) $ fail "Expected a command"
    case cmd of
      Nothing -> return $ makeSimpleCommand id1 id2 prefix [] []
      Just cmd -> do
            suffix <- option [] $ getParser readCmdSuffix cmd [
                        (["declare", "export", "local", "readonly", "typeset"], readModifierSuffix),
                        (["time"], readTimeSuffix),
                        (["let"], readLetSuffix),
                        (["eval"], readEvalSuffix)
                    ]

            let result = makeSimpleCommand id1 id2 prefix [cmd] suffix
            if isCommand ["source", "."] cmd
                then readSource pos result
                else return result
  where
    isCommand strings (T_NormalWord _ [T_Literal _ s]) = s `elem` strings
    isCommand _ _ = False
    getParser def cmd [] = def
    getParser def cmd ((list, action):rest) =
        if isCommand list cmd
        then action
        else getParser def cmd rest


readSource :: Monad m => SourcePos -> Token -> SCParser m Token
readSource pos t@(T_Redirecting _ _ (T_SimpleCommand _ _ (cmd:file:_))) = do
    override <- getSourceOverride
    let literalFile = do
        name <- override `mplus` getLiteralString file
        -- Hack to avoid 'source ~/foo' trying to read from literal tilde
        guard . not $ "~/" `isPrefixOf` name
        return name
    case literalFile of
        Nothing -> do
            parseNoteAt pos WarningC 1090
                "Can't follow non-constant source. Use a directive to specify location."
            return t
        Just filename -> do
            proceed <- shouldFollow filename
            if not proceed
              then do
                parseNoteAt pos InfoC 1093
                    "This file appears to be recursively sourced. Ignoring."
                return t
              else do
                sys <- Mr.asks systemInterface
                input <-
                    if filename == "/dev/null" -- always allow /dev/null
                    then return (Right "")
                    else system $ siReadFile sys filename
                case input of
                    Left err -> do
                        parseNoteAt pos InfoC 1091 $
                            "Not following: " ++ err
                        return t
                    Right script -> do
                        id <- getNextIdAt pos

                        let included = do
                            src <- subRead filename script
                            return $ T_Include id t src

                        let failed = do
                            parseNoteAt pos WarningC 1094
                                "Parsing of sourced file failed. Ignoring it."
                            return t

                        included <|> failed
  where
    subRead name script =
        withContext (ContextSource name) $
            inSeparateContext $
                subParse (initialPos name) readScript script
readSource _ t = return t


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
    apos <- getPosition
    annotations <- readAnnotations

    unless (null annotations) $ optional $ do
        try . lookAhead $ readKeyword
        parseProblemAt apos ErrorC 1123 "ShellCheck directives are only valid in front of complete compound commands, like 'if', not e.g. individual 'elif' branches."

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

prop_readTerm = isOk readTerm "time ( foo; bar; )"
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
    -- Ignore alias suppression
    optional . try $ do
        char '\\'
        lookAhead $ variableChars
    readCmdWord

readCmdWord = do
    skipAnnotationAndWarn
    readNormalWord <* spacing

-- Due to poor planning, annotations after commands isn't handled well.
-- At the time this function is used, it's usually too late to skip
-- comments, so you end up with a parse failure instead.
skipAnnotationAndWarn = optional $ do
        try . lookAhead $ readAnnotationPrefix
        parseProblem ErrorC 1126 "Place shellcheck directives before commands, not after."
        readAnyComment

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
        return "Expected 'fi'"

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

    ifNextToken (g_Fi <|> g_Elif <|> g_Else) $
        parseProblemAt pos ErrorC 1049 "Did you forget the 'then' for this 'if'?"

    called "then clause" $ do
        g_Then `orFail` do
            parseProblem ErrorC 1050 "Expected 'then'."
            return "Expected 'then'"

        acceptButWarn g_Semi ErrorC 1051 "Semicolons directly after 'then' are not allowed. Just remove it."
        allspacing
        verifyNotEmptyIf "then"

        action <- readTerm
        return (condition, action)

readElifPart = called "elif clause" $ do
    pos <- getPosition
    correctElif <- elif
    unless correctElif $
        parseProblemAt pos ErrorC 1075 "Use 'elif' instead of 'else if' (or put 'if' on new line if nesting)."
    allspacing
    condition <- readTerm

    ifNextToken (g_Fi <|> g_Elif <|> g_Else) $
        parseProblemAt pos ErrorC 1049 "Did you forget the 'then' for this 'elif'?"

    g_Then
    acceptButWarn g_Semi ErrorC 1052 "Semicolons directly after 'then' are not allowed. Just remove it."
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
    acceptButWarn g_Semi ErrorC 1053 "Semicolons directly after 'else' are not allowed. Just remove it."
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
    char ')' <|> fail ") closing the subshell"
    return $ T_Subshell id list

prop_readBraceGroup = isOk readBraceGroup "{ a; b | c | d; e; }"
prop_readBraceGroup2 = isWarning readBraceGroup "{foo;}"
prop_readBraceGroup3 = isOk readBraceGroup "{(foo)}"
readBraceGroup = called "brace group" $ do
    id <- getNextId
    char '{'
    void allspacingOrFail <|> optional (do
        lookAhead $ noneOf "(" -- {( is legal
        parseProblem ErrorC 1054 "You need a space after the '{'.")
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
        return "Expected 'do'"

    acceptButWarn g_Semi ErrorC 1059 "No semicolons directly after 'do'."
    allspacing

    optional (do
                try . lookAhead $ g_Done
                parseProblemAt loopPos ErrorC 1060 "Can't have empty do clauses (use 'true' as a no-op).")

    commands <- readCompoundList
    g_Done `orFail` do
            parseProblemAt pos ErrorC 1061 "Couldn't find 'done' for this 'do'."
            parseProblem ErrorC 1062 "Expected 'done' matching previously mentioned 'do'."
            return "Expected 'done'"
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
prop_readForClause13= isOk readForClause "for foo\nin\\\n  bar\\\n  baz\ndo true; done"
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
        name <- readVariableName `thenSkip` allspacing
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
                (void g_Semi <|> void linefeed <|> void g_Do)

    do {
        lookAhead g_Do;
        parseNote ErrorC 1063 "You need a line feed or semicolon before the 'do'.";
    } <|> do {
        optional g_Semi;
        void allspacing;
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
    g_In <|> fail "Expected 'in'"
    readLineBreak
    list <- readCaseList
    g_Esac <|> fail "Expected 'esac' to close the case statement"
    return $ T_CaseExpression id word list

readCaseList = many readCaseItem

readCaseItem = called "case item" $ do
    notFollowedBy2 g_Esac
    optional $ do
        try . lookAhead $ readAnnotationPrefix
        parseProblem ErrorC 1124 "ShellCheck directives are only valid in front of complete commands like 'case' statements, not individual case branches."
    optional g_Lparen
    spacing
    pattern' <- readPattern
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
    return (separator, pattern', list)

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
prop_readFunctionDefinition9 = isOk readFunctionDefinition "function foo { true; }"
prop_readFunctionDefinition10= isOk readFunctionDefinition "function foo () { true; }"
prop_readFunctionDefinition11= isWarning readFunctionDefinition "function foo{\ntrue\n}"
prop_readFunctionDefinition12= isOk readFunctionDefinition "function []!() { true; }"
readFunctionDefinition = called "function" $ do
    functionSignature <- try readFunctionSignature
    allspacing
    void (lookAhead $ oneOf "{(") <|> parseProblem ErrorC 1064 "Expected a { to open the function definition."
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
            name <- many1 extendedFunctionChars
            spaces <- spacing
            hasParens <- wasIncluded readParens
            when (not hasParens && null spaces) $
                acceptButWarn (lookAhead (oneOf "{("))
                    ErrorC 1095 "You need a space or linefeed between the function name and body."
            return $ T_Function id (FunctionKeyword True) (FunctionParentheses hasParens) name

        readWithoutFunction = try $ do
            id <- getNextId
            name <- many1 functionChars
            guard $ name /= "time"  -- Interfers with time ( foo )
            spacing
            readParens
            return $ T_Function id (FunctionKeyword False) (FunctionParentheses True) name

        readParens = do
            g_Lparen
            spacing
            g_Rparen <|> do
                parseProblem ErrorC 1065 "Trying to declare parameters? Don't. Use () and refer to params as $1, $2.."
                many $ noneOf "\n){"
                g_Rparen
            return ()

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
    cmd <- choice [
        readBraceGroup,
        readAmbiguous "((" readArithmeticExpression readSubshell (\pos ->
            parseNoteAt pos WarningC 1105 "Shells disambiguate (( differently or not at all. For subshell, add spaces around ( . For ((, fix parsing errors."),
        readSubshell,
        readCondition,
        readWhileClause,
        readUntilClause,
        readIfClause,
        readForClause,
        readSelectClause,
        readCaseClause,
        readFunctionDefinition
        ]
    spacing
    redirs <- many readIoRedirect
    unless (null redirs) $ optional $ do
        lookAhead $ try (spacing >> needsSeparator)
        parseProblem WarningC 1013 "Bash requires ; or \\n here, after redirecting nested compound commands."
    return $ T_Redirecting id redirs cmd
  where
    needsSeparator = choice [ g_Then, g_Else, g_Elif, g_Fi, g_Do, g_Done, g_Esac, g_Rbrace ]


readCompoundList = readTerm
readCompoundListOrEmpty = do
    allspacing
    readTerm <|> return []

readCmdPrefix = many1 (readIoRedirect <|> readAssignmentWord)
readCmdSuffix = many1 (readIoRedirect <|> readCmdWord)
readModifierSuffix = many1 (readIoRedirect <|> readWellFormedAssignment <|> readCmdWord)
readTimeSuffix = do
    flags <- many readFlag
    pipeline <- readPipeline
    return $ flags ++ [pipeline]
  where
    -- This fails for quoted variables and such. Fixme?
    readFlag = do
        lookAhead $ char '-'
        readCmdWord

-- Fixme: this is a hack that doesn't handle let c='4'"5" or let a\>b
readLetSuffix :: Monad m => SCParser m [Token]
readLetSuffix = many1 (readIoRedirect <|> try readLetExpression <|> readCmdWord)
  where
    readLetExpression :: Monad m => SCParser m Token
    readLetExpression = do
        startPos <- getPosition
        expression <- readStringForParser readCmdWord
        let (unQuoted, newPos) = kludgeAwayQuotes expression startPos
        subParse newPos readArithmeticContents unQuoted

    kludgeAwayQuotes :: String -> SourcePos -> (String, SourcePos)
    kludgeAwayQuotes s p =
        case s of
            first:rest@(_:_) ->
                let (last:backwards) = reverse rest
                    middle = reverse backwards
                in
                    if first `elem` "'\"" && first == last
                    then (middle, updatePosChar p first)
                    else (s, p)
            x -> (s, p)


-- bash allows a=(b), ksh allows $a=(b). dash allows neither. Let's warn.
readEvalSuffix = many1 (readIoRedirect <|> readCmdWord <|> evalFallback)
  where
    evalFallback = do
        pos <- getPosition
        lookAhead $ char '('
        parseProblemAt pos WarningC 1098 "Quote/escape special characters when using eval, e.g. eval \"a=(b)\"."
        fail "Unexpected parentheses. Make sure to quote when eval'ing as shell parsers differ."

-- Get whatever a parser would parse as a string
readStringForParser parser = do
    pos <- inSeparateContext $ lookAhead (parser >> getPosition)
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
prop_readAssignmentWord9b= isOk readAssignmentWord "foo=  "
prop_readAssignmentWord9c= isOk readAssignmentWord "foo=  #bar"
prop_readAssignmentWord10= isWarning readAssignmentWord "foo$n=42"
prop_readAssignmentWord11= isOk readAssignmentWord "foo=([a]=b [c] [d]= [e f )"
prop_readAssignmentWord12= isOk readAssignmentWord "a[b <<= 3 + c]='thing'"
prop_readAssignmentWord13= isOk readAssignmentWord "var=( (1 2) (3 4) )"
prop_readAssignmentWord14= isOk readAssignmentWord "var=( 1 [2]=(3 4) )"
prop_readAssignmentWord15= isOk readAssignmentWord "var=(1 [2]=(3 4))"
readAssignmentWord = readAssignmentWordExt True
readWellFormedAssignment = readAssignmentWordExt False
readAssignmentWordExt lenient = try $ do
    id <- getNextId
    pos <- getPosition
    when lenient $
        optional (char '$' >> parseNote ErrorC 1066 "Don't use $ on the left side of assignments.")
    variable <- readVariableName
    when lenient $
        optional (readNormalDollar >> parseNoteAt pos ErrorC
                                1067 "For indirection, use (associative) arrays or 'read \"var$n\" <<< \"value\"'")
    indices <- many readArrayIndex
    hasLeftSpace <- liftM (not . null) spacing
    pos <- getPosition
    op <- readAssignmentOp
    hasRightSpace <- liftM (not . null) spacing
    isEndOfCommand <- liftM isJust $ optionMaybe (try . lookAhead $ (void (oneOf "\r\n;&|)") <|> eof))
    if not hasLeftSpace && (hasRightSpace || isEndOfCommand)
      then do
        when (variable /= "IFS" && hasRightSpace && not isEndOfCommand) $
            parseNoteAt pos WarningC 1007
                "Remove space after = if trying to assign a value (for empty string, use var='' ... )."
        value <- readEmptyLiteral
        return $ T_Assignment id op variable indices value
      else do
        when (hasLeftSpace || hasRightSpace) $
            parseNoteAt pos ErrorC 1068 $
                "Don't put spaces around the "
                ++ if op == Append
                    then "+= when appending."
                    else "= in assignments."
        value <- readArray <|> readNormalWord
        spacing
        return $ T_Assignment id op variable indices value
  where
    readAssignmentOp = do
        pos <- getPosition
        unexpecting "" $ string "==="
        choice [
            string "+=" >> return Append,
            do
                try (string "==")
                parseProblemAt pos ErrorC 1097
                    "Unexpected ==. For assignment, use =. For comparison, use [/[[."
                return Assign,

            string "=" >> return Assign
            ]
    readEmptyLiteral = do
        id <- getNextId
        return $ T_Literal id ""

readArrayIndex = do
    id <- getNextId
    char '['
    pos <- getPosition
    str <- readStringForParser readIndexSpan
    char ']'
    return $ T_UnparsedIndex id pos str

readArray :: Monad m => SCParser m Token
readArray = called "array assignment" $ do
    id <- getNextId
    opening <- getPosition
    char '('
    optional $ do
        lookAhead $ char '('
        parseProblemAt opening ErrorC 1116 "Missing $ on a $((..)) expression? (or use ( ( for arrays)."
    allspacing
    words <- readElement `reluctantlyTill` char ')'
    char ')' <|> fail "Expected ) to close array assignment"
    return $ T_Array id words
  where
    readElement = (readIndexed <|> readRegular) `thenSkip` allspacing
    readIndexed = do
        id <- getNextId
        index <- try $ do
            x <- many1 readArrayIndex
            char '='
            return x
        value <- readRegular <|> nothing
        return $ T_IndexedElement id index value
    readRegular = readArray <|> readNormalWord

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

    optional $ do
        try . lookAhead $ char '['
        parseProblem ErrorC 1069 "You need a space before the [."
    optional $ do
        try . lookAhead $ char '#'
        parseProblem ErrorC 1099 "You need a space before the #."

    lookAhead keywordSeparator
    when (str /= keyword) $
        parseProblem ErrorC 1081 $
            "Scripts are case sensitive. Use '" ++ keyword ++ "', not '" ++ str ++ "'."
    return $ t id

anycaseString =
    mapM anycaseChar
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
g_In = tryWordToken "in" T_In <* skipAnnotationAndWarn
g_Lbrace = tryWordToken "{" T_Lbrace
g_Rbrace = do -- handled specially due to ksh echo "${ foo; }bar"
    id <- getNextId
    char '}'
    return $ T_Rbrace id

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
    eof <|> void (try allspacingOrFail) <|> void (oneOf ";()[<>&|")

readKeyword = choice [ g_Then, g_Else, g_Elif, g_Fi, g_Do, g_Done, g_Esac, g_Rbrace, g_Rparen, g_DSEMI ]

ifParse p t f =
    (lookAhead (try p) >> t) <|> f

prop_readShebang1 = isOk readShebang "#!/bin/sh\n"
prop_readShebang2 = isWarning readShebang "!# /bin/sh\n"
prop_readShebang3 = isNotOk readShebang "#shellcheck shell=/bin/sh\n"
prop_readShebang4 = isWarning readShebang "! /bin/sh"
readShebang = do
    choice $ map try [
        readCorrect,
        readSwapped,
        readTooManySpaces,
        readMissingHash,
        readMissingBang
        ]
    many linewhitespace
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

    skipSpaces = liftM (not . null) $ many linewhitespace
    readTooManySpaces = do
        startPos <- getPosition
        startSpaces <- skipSpaces
        char '#'
        middlePos <- getPosition
        middleSpaces <- skipSpaces
        char '!'
        when startSpaces $
            parseProblemAt startPos ErrorC 1114
                "Remove leading spaces before the shebang."
        when middleSpaces $
            parseProblemAt middlePos ErrorC 1115
                "Remove spaces between # and ! in the shebang."

    readMissingHash = do
        pos <- getPosition
        char '!'
        ensurePathAhead
        parseProblemAt pos ErrorC 1104
            "Use #!, not just !, for the shebang."

    readMissingBang = do
        char '#'
        pos <- getPosition
        ensurePathAhead
        parseProblemAt pos ErrorC 1113
            "Use #!, not just #, for the shebang."


    ensurePathAhead = lookAhead $ do
        many linewhitespace
        char '/'

verifyEof = eof <|> choice [
        ifParsable g_Lparen $
            parseProblem ErrorC 1088 "Parsing stopped here. Invalid use of parentheses?",

        ifParsable readKeyword $
            parseProblem ErrorC 1089 "Parsing stopped here. Is this keyword correctly matched up?",

        parseProblem ErrorC 1070 "Parsing stopped here. Mismatched keywords or invalid parentheses?"
    ]
  where
    ifParsable p action = do
        try (lookAhead p)
        action

prop_readScript1 = isOk readScriptFile "#!/bin/bash\necho hello world\n"
prop_readScript2 = isWarning readScriptFile "#!/bin/bash\r\necho hello world\n"
prop_readScript3 = isWarning readScriptFile "#!/bin/bash\necho hello\xA0world"
prop_readScript4 = isWarning readScriptFile "#!/usr/bin/perl\nfoo=("
prop_readScript5 = isOk readScriptFile "#!/bin/bash\n#This is an empty script\n\n"
readScriptFile = do
    id <- getNextId
    pos <- getPosition
    optional $ do
        readUtf8Bom
        parseProblem ErrorC 1082
            "This file has a UTF-8 BOM. Remove it with: LC_CTYPE=C sed '1s/^...//' < yourscript ."
    sb <- option "" readShebang
    verifyShell pos (getShell sb)
    if isValidShell (getShell sb) /= Just False
      then do
            allspacing
            annotationId <- getNextId
            annotations <- readAnnotations
            commands <- withAnnotations annotations readCompoundListOrEmpty
            verifyEof
            let script = T_Annotation annotationId annotations $  T_Script id sb commands
            reparseIndices script
        else do
            many anyChar
            return $ T_Script id sb []

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
            Just False -> parseProblemAt pos ErrorC 1071 "ShellCheck only supports sh/bash/dash/ksh scripts. Sorry!"
            Nothing -> parseProblemAt pos InfoC 1008 "This shebang was unrecognized. Note that ShellCheck only handles sh/bash/dash/ksh."

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

readScript = do
    script <- readScriptFile
    reparseIndices script


-- Interactively run a parser in ghci:
-- debugParse readScript "echo 'hello world'"
debugParse p string = runIdentity $ do
    (res, _) <- runParser testEnvironment p "-" string
    return res

testEnvironment =
    Environment {
        systemInterface = (mockedSystemInterface []),
        checkSourced = False
    }


isOk p s =      parsesCleanly p s == Just True   -- The string parses with no warnings
isWarning p s = parsesCleanly p s == Just False  -- The string parses with warnings
isNotOk p s =   parsesCleanly p s == Nothing     -- The string does not parse

parsesCleanly parser string = runIdentity $ do
    (res, sys) <- runParser testEnvironment
                    (parser >> eof >> getState) "-" string
    case (res, sys) of
        (Right userState, systemState) ->
            return $ Just . null $ parseNotes userState ++ parseProblems systemState
        (Left _, _) -> return Nothing

-- For printf debugging: print the value of an expression
-- Example: return $ dump $ T_Literal id [c]
dump :: Show a => a -> a
dump x = trace (show x) x

-- Like above, but print a specific expression:
-- Example: return $ dumps ("Returning: " ++ [c])  $ T_Literal id [c]
dumps :: Show x => x -> a -> a
dumps t = trace (show t)

parseWithNotes parser = do
    item <- parser
    state <- getState
    return (item, state)

compareNotes (ParseNote pos1 pos1' level1 _ s1) (ParseNote pos2 pos2' level2 _ s2) = compare (pos1, pos1', level1) (pos2, pos2', level2)
sortNotes = sortBy compareNotes


makeErrorFor parsecError =
    ParseNote pos pos ErrorC 1072 $
        getStringFromParsec $ errorMessages parsecError
    where
      pos = errorPos parsecError

getStringFromParsec errors =
        case map f errors of
            r -> unwords (take 1 $ catMaybes $ reverse r)  ++
                " Fix any mentioned problems and try again."
    where
        f err =
            case err of
                UnExpect s    ->  Nothing -- Due to not knowing Parsec, none of these
                SysUnExpect s ->  Nothing -- are actually helpful. <?> has been hidden
                Expect s      ->  Nothing -- and we only show explicit fail statements.
                Message s     ->  if null s then Nothing else return $ s ++ "."

runParser :: Monad m =>
    Environment m ->
    SCParser m v ->
    String ->
    String ->
    m (Either ParseError v, SystemState)

runParser env p filename contents =
    Ms.runStateT
        (Mr.runReaderT
            (runParserT p initialUserState filename contents)
            env)
        initialSystemState
system = lift . lift . lift

parseShell env name contents = do
    (result, state) <- runParser env (parseWithNotes readScript) name contents
    case result of
        Right (script, userstate) ->
            return ParseResult {
                prComments = map toPositionedComment $ nub $ parseNotes userstate ++ parseProblems state,
                prTokenPositions = Map.map posToPos (positionMap userstate),
                prRoot = Just $
                    reattachHereDocs script (hereDocMap userstate)
            }
        Left err ->
            return ParseResult {
                prComments =
                    map toPositionedComment $
                        notesForContext (contextStack state)
                        ++ [makeErrorFor err]
                        ++ parseProblems state,
                prTokenPositions = Map.empty,
                prRoot = Nothing
            }
  where
    isName (ContextName _ _) = True
    isName _ = False
    notesForContext list = zipWith ($) [first, second] $ filter isName list
    first (ContextName pos str) = ParseNote pos pos ErrorC 1073 $
        "Couldn't parse this " ++ str ++ "."
    second (ContextName pos str) = ParseNote pos pos InfoC 1009 $
        "The mentioned parser error was in this " ++ str ++ "."

-- Go over all T_UnparsedIndex and reparse them as either arithmetic or text
-- depending on declare -A statements.
reparseIndices root =
   analyze blank blank f root
  where
    associative = getAssociativeArrays root
    isAssociative s = s `elem` associative
    f (T_Assignment id mode name indices value) = do
        newIndices <- mapM (fixAssignmentIndex name) indices
        newValue <- case value of
            (T_Array id2 words) -> do
                newWords <- mapM (fixIndexElement name) words
                return $ T_Array id2 newWords
            x -> return x
        return $ T_Assignment id mode name newIndices newValue
    f t = return t

    fixIndexElement name word =
        case word of
            T_IndexedElement id indices value -> do
                new <- mapM (fixAssignmentIndex name) indices
                return $ T_IndexedElement id new value
            otherwise -> return word

    fixAssignmentIndex name word =
        case word of
            T_UnparsedIndex id pos src -> do
                parsed name pos src
            otherwise -> return word

    parsed name pos src =
        if isAssociative name
        then subParse pos (called "associative array index" $ readIndexSpan) src
        else subParse pos (called "arithmetic array index expression" $ optional space >> readArithmeticContents) src

reattachHereDocs root map =
    doTransform f root
  where
    f t@(T_HereDoc id dash quote string []) = fromMaybe t $ do
        list <- Map.lookup id map
        return $ T_HereDoc id dash quote string list
    f t = t

toPositionedComment :: ParseNote -> PositionedComment
toPositionedComment (ParseNote start end severity code message) =
    PositionedComment (posToPos start) (posToPos end) $ Comment severity code message

posToPos :: SourcePos -> Position
posToPos sp = Position {
    posFile = sourceName sp,
    posLine = fromIntegral $ sourceLine sp,
    posColumn = fromIntegral $ sourceColumn sp
}

-- TODO: Clean up crusty old code that this is layered on top of
parseScript :: Monad m =>
        SystemInterface m -> ParseSpec -> m ParseResult
parseScript sys spec =
    parseShell env (psFilename spec) (psScript spec)
  where
    env = Environment {
        systemInterface = sys,
        checkSourced = psCheckSourced spec
    }


return []
runTests = $quickCheckAll

