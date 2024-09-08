{-
    Copyright 2012-2022 Vidar Holen

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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
module ShellCheck.Parser (parseScript, runTests) where

import ShellCheck.AST
import ShellCheck.ASTLib hiding (runTests)
import ShellCheck.Data
import ShellCheck.Interface
import ShellCheck.Prelude

import Control.Applicative ((<*), (*>))
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
import Data.Char
import Data.Functor
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, partition, sortBy, intercalate, nub, find)
import Data.Maybe
import Data.Monoid
import GHC.Exts (sortWith)
import Prelude hiding (readList)
import System.IO
import Text.Parsec hiding (runParser, (<?>))
import Text.Parsec.Error
import Text.Parsec.Pos
import qualified Control.Monad.Reader as Mr
import qualified Control.Monad.State as Ms
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

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
functionChars = variableChars <|> oneOf ":+?-./^@,"
-- Chars to allow in functions using the 'function' keyword
extendedFunctionChars = functionChars <|> oneOf "[]*=!"
specialVariable = oneOf (concat specialVariables)
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

prop_spacing1 = isOk spacing "  \\\n # Comment"
prop_spacing2 = isOk spacing "# We can continue lines with \\"
prop_spacing3 = isWarning spacing "   \\\n #  --verbose=true \\"
spacing = do
    x <- many (many1 linewhitespace <|> continuation)
    optional readComment
    return $ concat x
  where
    continuation = do
        try (string "\\\n")
        -- The line was continued. Warn if this next line is a comment with a trailing \
        whitespace <- many linewhitespace
        optional $ do
            x <- readComment
            when ("\\" `isSuffixOf` x) $
                parseProblem ErrorC 1143 "This backslash is part of a comment and does not continue the line."
        return whitespace

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
    return s

readUnicodeQuote = do
    start <- startSpan
    c <- oneOf (unicodeSingleQuotes ++ unicodeDoubleQuotes)
    id <- endSpan start
    parseProblemAtId id WarningC 1110 "This is a unicode quote. Delete and retype it (or quote to make literal)."
    return $ T_Literal id [c]

carriageReturn = do
    pos <- getPosition
    char '\r'
    parseProblemAt pos ErrorC 1017 "Literal carriage return. Run script through tr -d '\\r' ."
    return '\r'

almostSpace = do
        parseNote ErrorC 1018 $ "This is a unicode space. Delete and retype it."
        oneOf "\xA0\x2002\x2003\x2004\x2005\x2006\x2007\x2008\x2009\x200B\x202F"
        return ' '

--------- Message/position annotation on top of user state
data ParseNote = ParseNote SourcePos SourcePos Severity Code String deriving (Show, Eq)
data Context =
        ContextName SourcePos String
        | ContextAnnotation [Annotation]
        | ContextSource String
    deriving (Show)

data HereDocContext =
        HereDocPending Id Dashed Quoted String [Context] -- on linefeed, read this T_HereDoc
    deriving (Show)

data UserState = UserState {
    lastId :: Id,
    positionMap :: Map.Map Id (SourcePos, SourcePos),
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

getLastId = lastId <$> getState

getNextIdBetween startPos endPos = do
    state <- getState
    let newId = incId (lastId state)
    let newMap = Map.insert newId (startPos, endPos) (positionMap state)
    putState $ state {
        lastId = newId,
        positionMap = newMap
    }
    return newId
  where incId (Id n) = Id $ n+1

getNextIdSpanningTokens startTok endTok = do
    (start, _) <- getSpanForId (getId startTok)
    (_, end)   <- getSpanForId (getId endTok)
    getNextIdBetween start end

-- Get an ID starting from the first token of the list, and ending after the last
getNextIdSpanningTokenList list =
    case list of
    [] -> do
        pos <- getPosition
        getNextIdBetween pos pos
    (h:_) ->
        getNextIdSpanningTokens h (last list)

-- Get the span covered by an id
getSpanForId :: Monad m => Id -> SCParser m (SourcePos, SourcePos)
getSpanForId id =
    Map.findWithDefault (error $ pleaseReport "no parser span for id") id <$>
        getMap

-- Create a new id with the same span as an existing one
getNewIdFor :: Monad m => Id -> SCParser m Id
getNewIdFor id = getSpanForId id >>= uncurry getNextIdBetween

data IncompleteInterval = IncompleteInterval SourcePos

startSpan = IncompleteInterval <$> getPosition

endSpan (IncompleteInterval start) = do
    endPos <- getPosition
    getNextIdBetween start endPos

getSpanPositionsFor m = do
    start <- getPosition
    m
    end <- getPosition
    return (start, end)

addToHereDocMap id list = do
    state <- getState
    let map = hereDocMap state
    putState $ state {
        hereDocMap = Map.insert id list map
    }

addPendingHereDoc id d q str = do
    state <- getState
    context <- getCurrentContexts
    let docs = pendingHereDocs state
    putState $ state {
        pendingHereDocs = HereDocPending id d q str context : docs
    }

popPendingHereDocs = do
    state <- getState
    let pending = pendingHereDocs state
    putState $ state {
        pendingHereDocs = []
    }
    return . reverse $ pendingHereDocs state

getMap = positionMap <$> getState
getParseNotes = parseNotes <$> getState

addParseNote n = do
    irrelevant <- shouldIgnoreCode (codeForParseNote n)
    unless irrelevant $ do
        state <- getState
        putState $ state {
            parseNotes = n : parseNotes state
        }

ignoreProblemsOf p = do
    systemState <- lift . lift $ Ms.get
    p <* (lift . lift . Ms.put $ systemState)

shouldIgnoreCode code = do
    context <- getCurrentContexts
    checkSourced <- Mr.asks checkSourced
    return $ any (contextItemDisablesCode checkSourced code) context

-- Does this item on the context stack disable warnings for 'code'?
contextItemDisablesCode :: Bool -> Integer -> Context -> Bool
contextItemDisablesCode alsoCheckSourced code = disabling alsoCheckSourced
  where
    disabling checkSourced item =
        case item of
            ContextAnnotation list -> any disabling' list
            ContextSource _ -> not $ checkSourced
            _ -> False
    disabling' (DisableComment n m) = code >= n && code < m
    disabling' _ = False



getCurrentAnnotations includeSource =
    concatMap get . takeWhile (not . isBoundary) <$> getCurrentContexts
  where
    get (ContextAnnotation list) = list
    get _ = []
    isBoundary (ContextSource _) = not includeSource
    isBoundary _ = False


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
    checkSourced :: Bool,
    ignoreRC :: Bool,
    currentFilename :: String,
    shellTypeOverride :: Maybe Shell
}

parseProblem level code msg = do
    pos <- getPosition
    parseProblemAt pos level code msg

setCurrentContexts c = Ms.modify (\state -> state { contextStack = c })
getCurrentContexts = Ms.gets contextStack

popContext = do
    v <- getCurrentContexts
    case v of
        (a:r) -> do
            setCurrentContexts r
            return $ Just a
        [] ->
            return Nothing

pushContext c = do
    v <- getCurrentContexts
    setCurrentContexts (c:v)

parseProblemAtWithEnd start end level code msg = do
    irrelevant <- shouldIgnoreCode code
    unless irrelevant $
        addParseProblem note
  where
    note = ParseNote start end level code msg

addParseProblem note =
    Ms.modify (\state -> state {
        parseProblems = note:parseProblems state
    })

parseProblemAt pos = parseProblemAtWithEnd pos pos

parseProblemAtId :: Monad m => Id -> Severity -> Integer -> String -> SCParser m ()
parseProblemAtId id level code msg = do
    (start, end) <- getSpanForId id
    parseProblemAtWithEnd start end level code msg

-- Store non-parse problems inside

parseNote c l a = do
    pos <- getPosition
    parseNoteAt pos c l a

parseNoteAt pos c l a = addParseNote $ ParseNote pos pos c l a
parseNoteAtId id c l a = do
    (start, end) <- getSpanForId id
    addParseNote $ ParseNote start end c l a

parseNoteAtWithEnd start end c l a = addParseNote $ ParseNote start end c l a

--------- Convenient combinators
thenSkip main follow = main <* optional follow

unexpecting s p = try $
    (try p >> fail ("Unexpected " ++ s)) <|> return ()

notFollowedBy2 = unexpecting ""

isFollowedBy p = (lookAhead . try $ p $> True) <|> return False

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
    start <- startSpan
    contents <- parser
    id <- endSpan start
    return $ node id contents

wasIncluded p = option False (p >> return True)

acceptButWarn parser level code note =
    optional $ try (do
        pos <- getPosition
        parser
        parseProblemAt pos level code note
      )

parsecBracket before after op = do
    val <- before
    op val `thenSkip` after val <|> (after val *> fail "")

swapContext contexts p =
    parsecBracket (getCurrentContexts <* setCurrentContexts contexts)
                  setCurrentContexts
                  (const p)

withContext entry p = parsecBracket (pushContext entry) (const popContext) (const p)

called s p = do
    pos <- getPosition
    withContext (ContextName pos s) p

withAnnotations anns p =
    if null anns then p else withContext (ContextAnnotation anns) p

readConditionContents single =
    readCondContents `attempting` lookAhead (do
                                pos <- getPosition
                                s <- readVariableName
                                spacing1
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
        op <- getOp
        spacingOrLf
        return op
      where
        flaglessOps = [ "==", "!=", "<=", ">=", "=~", ">", "<", "=" ]

        getOp = do
            start <- startSpan
            op <- readRegularOrEscaped anyOp
            id <- endSpan start
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
        start <- startSpan
        s <- readOp
        id <- endSpan start
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
        when (notArrayIndex x && endedWith "]" x && not (x `containsLiteral` "[")) $ do
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
            notArrayIndex (T_NormalWord id s@(_:T_Literal _ t:_)) = t /= "["
            notArrayIndex _ = True
            containsLiteral x s = s `isInfixOf` onlyLiteralString x

    readCondAndOp = readAndOrOp TC_And "&&" False <|> readAndOrOp TC_And "-a" True

    readCondOrOp = do
        optional guardArithmetic
        readAndOrOp TC_Or "||" False <|> readAndOrOp TC_Or "-o" True

    readAndOrOp node op requiresSpacing = do
        optional $ lookAhead weirdDash
        start <- startSpan
        x <- try $ string op
        id <- endSpan start
        condSpacing requiresSpacing
        return $ node id typ x

    readCondNullaryOrBinary = do
      start <- startSpan
      x <- readCondWord `attempting` (do
              pos <- getPosition
              lookAhead (char '[')
              parseProblemAt pos ErrorC 1026 $ if single
                  then "If grouping expressions inside [..], use \\( ..\\)."
                  else "If grouping expressions inside [[..]], use ( .. )."
            )
      id <- endSpan start
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

    checkTrailingOp x = sequence_ $ do
        (T_Literal id str) <- getTrailingUnquotedLiteral x
        trailingOp <- find (`isSuffixOf` str) binaryTestOps
        return $ parseProblemAtId id ErrorC 1108 $
            "You need a space before and after the " ++ trailingOp ++ " ."

    readCondGroup = do
        start <- startSpan
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
        id <- endSpan start
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
        start <- startSpan
        parts <- many1 readPart
        id <- endSpan start
        void spacing
        return $ T_NormalWord id parts
      where
        readPart = choice [
            readGroup,
            readSingleQuoted,
            readDoubleQuoted,
            readDollarExpression,
            readLiteralForParser $ readNormalLiteral "( ",
            readLiteralString "|",
            readGlobLiteral
            ]
        readGlobLiteral = do
            start <- startSpan
            s <- extglobStart <|> oneOf "{}[]$"
            id <- endSpan start
            return $ T_Literal id [s]
        readGroup = called "regex grouping" $ do
            start <- startSpan
            p1 <- readLiteralString "("
            parts <- many (readPart <|> readRegexLiteral)
            p2 <- readLiteralString ")"
            id <- endSpan start
            return $ T_NormalWord id (p1:(parts ++ [p2]))
        readRegexLiteral = do
            start <- startSpan
            str <- readGenericLiteral1 (singleQuote <|> doubleQuotable <|> oneOf "()")
            id <- endSpan start
            return $ T_Literal id str
        readLiteralString s = do
            start <- startSpan
            str <- string s
            id <- endSpan start
            return $ T_Literal id str

    readCondTerm = do
        term <- readCondNot <|> readCondExpr
        condSpacing False
        return term

    readCondNot = do
        start <- startSpan
        char '!'
        id <- endSpan start
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
prop_a10 = isOk readArithmeticContents "! $?"
prop_a11 = isOk readArithmeticContents "10#08 * 16#f"
prop_a12 = isOk readArithmeticContents "\"$((3+2))\" + '37'"
prop_a13 = isOk readArithmeticContents "foo[9*y+x]++"
prop_a14 = isOk readArithmeticContents "1+`echo 2`"
prop_a15 = isOk readArithmeticContents "foo[`echo foo | sed s/foo/4/g` * 3] + 4"
prop_a16 = isOk readArithmeticContents "$foo$bar"
prop_a17 = isOk readArithmeticContents "i<(0+(1+1))"
prop_a18 = isOk readArithmeticContents "a?b:c"
prop_a19 = isOk readArithmeticContents "\\\n3 +\\\n  2"
prop_a20 = isOk readArithmeticContents "a ? b ? c : d : e"
prop_a21 = isOk readArithmeticContents "a ? b : c ? d : e"
prop_a22 = isOk readArithmeticContents "!!a"
prop_a23 = isOk readArithmeticContents "~0"
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
        start <- startSpan
        op <- choice (map (\x -> try $ do
                                        s <- string x
                                        failIfIncompleteOp
                                        return s
                            ) op)
        id <- endSpan start
        spacing
        return $ token id op

    failIfIncompleteOp = notFollowedBy2 $ oneOf "&|<>="

    -- Read binary minus, but also check for -lt, -gt and friends:
    readMinusOp = do
        start <- startSpan
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
        id <- endSpan start
        spacing
        return $ TA_Binary id "-"
      where
        tryOp (str, alt) = try $ do
            string str
            spacing1
            return (str, alt)

    readArrayIndex = do
        start <- startSpan
        char '['
        pos <- getPosition
        middle <- readStringForParser readArithmeticContents
        char ']'
        id <- endSpan start
        return $ T_UnparsedIndex id pos middle

    literal s = do
        start <- startSpan
        string s
        id <- endSpan start
        return $ T_Literal id s

    readVariable = do
        start <- startSpan
        name <- readVariableName
        indices <- many readArrayIndex
        id <- endSpan start
        spacing
        return $ TA_Variable id name indices

    readExpansion = do
        start <- startSpan
        pieces <- many1 $ choice [
            readSingleQuoted,
            readDoubleQuoted,
            readNormalDollar,
            readBraced,
            readUnquotedBackTicked,
            literal "#",
            readNormalLiteral "+-*/=%^,]?:"
            ]
        id <- endSpan start
        spacing
        return $ TA_Expansion id pieces

    readGroup = do
        start <- startSpan
        char '('
        s <- readSequence
        char ')'
        id <- endSpan start
        spacing
        return $ TA_Parenthesis id s

    readArithTerm = readGroup <|> readVariable <|> readExpansion

    readSequence = do
        spacing
        start <- startSpan
        l <- readAssignment `sepBy` (char ',' >> spacing)
        id <- endSpan start
        return $ TA_Sequence id l

    readAssignment = chainr1 readTrinary readAssignmentOp
    readAssignmentOp = readComboOp ["=", "*=", "/=", "%=", "+=", "-=", "<<=", ">>=", "&=", "^=", "|="] TA_Assignment

    readTrinary = do
        x <- readLogicalOr
        do
            start <- startSpan
            string "?"
            spacing
            y <- readTrinary
            string ":"
            spacing
            z <- readTrinary
            id <- endSpan start
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
        start <- startSpan
        op <- oneOf "!~"
        id <- endSpan start
        spacing
        x <- readAnyNegated
        return $ TA_Unary id [op] x

    readAnySigned = readSigned <|> readAnycremented
    readSigned = do
        start <- startSpan
        op <- choice (map readSignOp "+-")
        id <- endSpan start
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
        start <- startSpan
        op <- try $ string "++" <|> string "--"
        id <- endSpan start
        spacing
        x <- readArithTerm
        return $ TA_Unary id (op ++ "|") x

    readNormalOrPostfixIncremented = do
        x <- readArithTerm
        spacing
        do
            start <- startSpan
            op <- try $ string "++" <|> string "--"
            id <- endSpan start
            spacing
            return $ TA_Unary id ('|':op) x
         <|>
            return x



prop_readCondition   = isOk readCondition "[ \\( a = b \\) -a \\( c = d \\) ]"
prop_readCondition2  = isOk readCondition "[[ (a = b) || (c = d) ]]"
prop_readCondition3  = isOk readCondition "[[ $c = [[:alpha:].~-] ]]"
prop_readCondition4  = isOk readCondition "[[ $c =~ *foo* ]]"
prop_readCondition5  = isOk readCondition "[[ $c =~ f( ]] )* ]]"
prop_readCondition5a = isOk readCondition "[[ $c =~ a(b) ]]"
prop_readCondition5b = isOk readCondition "[[ $c =~ f( ($var ]]) )* ]]"
prop_readCondition6  = isOk readCondition "[[ $c =~ ^[yY]$ ]]"
prop_readCondition7  = isOk readCondition "[[ ${line} =~ ^[[:space:]]*# ]]"
prop_readCondition8  = isOk readCondition "[[ $l =~ ogg|flac ]]"
prop_readCondition9  = isOk readCondition "[ foo -a -f bar ]"
prop_readCondition10 = isOk readCondition "[[\na == b\n||\nc == d ]]"
prop_readCondition10a = isOk readCondition "[[\na == b  ||\nc == d ]]"
prop_readCondition10b = isOk readCondition "[[ a == b\n||\nc == d ]]"
prop_readCondition11 = isOk readCondition "[[ a == b ||\n c == d ]]"
prop_readCondition12 = isWarning readCondition "[ a == b \n -o c == d ]"
prop_readCondition13 = isOk readCondition "[[ foo =~ ^fo{1,3}$ ]]"
prop_readCondition14 = isOk readCondition "[ foo '>' bar ]"
prop_readCondition15 = isOk readCondition "[ foo \">=\" bar ]"
prop_readCondition16 = isOk readCondition "[ foo \\< bar ]"
prop_readCondition17 = isOk readCondition "[[ ${file::1} = [-.\\|/\\\\] ]]"
prop_readCondition18 = isOk readCondition "[ ]"
prop_readCondition19 = isOk readCondition "[ '(' x \")\" ]"
prop_readCondition20 = isOk readCondition "[[ echo_rc -eq 0 ]]"
prop_readCondition21 = isOk readCondition "[[ $1 =~ ^(a\\ b)$ ]]"
prop_readCondition22 = isOk readCondition "[[ $1 =~ \\.a\\.(\\.b\\.)\\.c\\. ]]"
prop_readCondition23 = isOk readCondition "[[ -v arr[$var] ]]"
prop_readCondition25 = isOk readCondition "[[ lex.yy.c -ot program.l ]]"
prop_readCondition26 = isOk readScript "[[ foo ]]\\\n && bar"
prop_readCondition27 = not $ isOk readConditionCommand "[[ x ]] foo"
prop_readCondition28 = isOk readCondition "[[ x = [\"$1\"] ]]"
prop_readCondition29 = isOk readCondition "[[ x = [*] ]]"

readCondition = called "test expression" $ do
    opos <- getPosition
    start <- startSpan
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
        id <- endSpan start
        return $ TC_Empty id typ

    cpos <- getPosition
    close <- try (string "]]") <|> string "]" <|> fail "Expected test to end here (don't wrap commands in []/[[]])"
    id <- endSpan start
    when (open == "[[" && close /= "]]") $ parseProblemAt cpos ErrorC 1033 "Test expression was opened with double [[ but closed with single ]. Make sure they match."
    when (open == "[" && close /= "]" ) $ parseProblemAt opos ErrorC 1034 "Test expression was opened with single [ but closed with double ]]. Make sure they match."
    spacing
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
prop_readAnnotation7 = isOk readAnnotation "# shellcheck disable=SC1000,SC2000-SC3000,SC1001\n"
prop_readAnnotation8 = isOk readAnnotation "# shellcheck disable=all\n"
prop_readAnnotation9 = isOk readAnnotation "# shellcheck source='foo bar' source-path=\"baz etc\"\n"
prop_readAnnotation10 = isOk readAnnotation "# shellcheck disable='SC1234,SC2345' enable=\"foo\" shell='bash'\n"
prop_readAnnotation11 = isOk (readAnnotationWithoutPrefix False) "external-sources='true'"

readAnnotation = called "shellcheck directive" $ do
    try readAnnotationPrefix
    many1 linewhitespace
    readAnnotationWithoutPrefix True

readAnnotationWithoutPrefix sandboxed = do
    values <- many1 readKey
    optional readAnyComment
    void linefeed <|> eof <|> do
        parseNote ErrorC 1125 "Invalid key=value pair? Ignoring the rest of this directive starting here."
        many (noneOf "\n")
        void linefeed <|> eof
    many linewhitespace
    return $ concat values
  where
    plainOrQuoted p = quoted p <|> p
    quoted p = do
        c <- oneOf "'\""
        start <- getPosition
        str <- many1 $ noneOf (c:"\n")
        char c <|> fail "Missing terminating quote for directive."
        subParse start p str
    readKey = do
        keyPos <- getPosition
        key <- many1 (letter <|> char '-')
        char '=' <|> fail "Expected '=' after directive key"
        annotations <- case key of
            "disable" -> plainOrQuoted $ readElement `sepBy` char ','
              where
                readElement = readRange <|> readAll
                readAll = do
                    string "all"
                    return $ DisableComment 0 1000000
                readRange = do
                    from <- readCode
                    to <- choice [ char '-' *> readCode, return $ from+1 ]
                    return $ DisableComment from to
                readCode = do
                    optional $ string "SC"
                    int <- many1 digit
                    return $ read int

            "enable" -> plainOrQuoted $ readName `sepBy` char ','
              where
                readName = EnableComment <$> many1 (letter <|> char '-')

            "source" -> do
                filename <- quoted (many1 anyChar) <|> (many1 $ noneOf " \n")
                return [SourceOverride filename]

            "source-path" -> do
                dirname <- quoted (many1 anyChar) <|> (many1 $ noneOf " \n")
                return [SourcePath dirname]

            "shell" -> do
                pos <- getPosition
                shell <- quoted (many1 anyChar) <|> (many1 $ noneOf " \n")
                when (isNothing $ shellForExecutable shell) $
                    parseNoteAt pos ErrorC 1103
                        "This shell type is unknown. Use e.g. sh or bash."
                return [ShellOverride shell]

            "extended-analysis" -> do
                pos <- getPosition
                value <- plainOrQuoted $ many1 letter
                case value of
                    "true" -> return [ExtendedAnalysis True]
                    "false" -> return [ExtendedAnalysis False]
                    _ -> do
                        parseNoteAt pos ErrorC 1146 "Unknown extended-analysis value. Expected true/false."
                        return []

            "external-sources" -> do
                pos <- getPosition
                value <- plainOrQuoted $ many1 letter
                case value of
                    "true" ->
                        if sandboxed
                        then do
                            parseNoteAt pos ErrorC 1144 "external-sources can only be enabled in .shellcheckrc, not in individual files."
                            return []
                        else return [ExternalSources True]
                    "false" -> return [ExternalSources False]
                    _ -> do
                        parseNoteAt pos ErrorC 1145 "Unknown external-sources value. Expected true/false."
                        return []

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

prop_readAnyComment = isOk readAnyComment "# Comment"
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
readNormalWord = readNormalishWord "" ["do", "done", "then", "fi", "esac"]

readPatternWord = readNormalishWord "" ["esac"]

readNormalishWord end terms = do
    start <- startSpan
    pos <- getPosition
    x <- many1 (readNormalWordPart end)
    id <- endSpan start
    checkPossibleTermination pos x terms
    return $ T_NormalWord id x

readIndexSpan = do
    start <- startSpan
    x <- many (readNormalWordPart "]" <|> someSpace <|> otherLiteral)
    id <- endSpan start
    return $ T_NormalWord id x
  where
    someSpace = do
        start <- startSpan
        str <- spacing1
        id <- endSpan start
        return $ T_Literal id str
    otherLiteral = do
        start <- startSpan
        str <- many1 $ oneOf quotableChars
        id <- endSpan start
        return $ T_Literal id str

checkPossibleTermination pos [T_Literal _ x] terminators =
    when (x `elem` terminators) $
        parseProblemAt pos WarningC 1010 $ "Use semicolon or linefeed before '" ++ x ++ "' (or quote to make it literal)."
checkPossibleTermination _ _ _ = return ()

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
        start <- startSpan
        str <- findParam <|> literalBraces
        id <- endSpan start
        return $ T_Literal id str

    findParam = try $ string "{}"
    literalBraces = do
        pos <- getPosition
        c <- oneOf "{}"
        parseProblemAt pos WarningC 1083 $
            "This " ++ [c] ++ " is literal. Check expression (missing ;/\\n?) or quote it."
        return [c]


readSpacePart = do
    start <- startSpan
    x <- many1 whitespace
    id <- endSpan start
    return $ T_Literal id x

readDollarBracedWord = do
    start <- startSpan
    list <- many readDollarBracedPart
    id <- endSpan start
    return $ T_NormalWord id list

readDollarBracedPart = readSingleQuoted <|> readDoubleQuoted <|>
                       readParamSubSpecialChar <|> readExtglob <|> readNormalDollar <|>
                       readUnquotedBackTicked <|> readDollarBracedLiteral

readDollarBracedLiteral = do
    start <- startSpan
    vars <- (readBraceEscaped <|> ((\x -> [x]) <$> anyChar)) `reluctantlyTill1` bracedQuotable
    id <- endSpan start
    return $ T_Literal id $ concat vars

readParamSubSpecialChar = do
    start <- startSpan
    x <- many1 paramSubSpecialChars
    id <- endSpan start
    return $ T_ParamSubSpecialChar id x

prop_readProcSub1 = isOk readProcSub "<(echo test | wc -l)"
prop_readProcSub2 = isOk readProcSub "<(  if true; then true; fi )"
prop_readProcSub3 = isOk readProcSub "<( # nothing here \n)"
readProcSub = called "process substitution" $ do
    start <- startSpan
    dir <- try $ do
                    x <- oneOf "<>"
                    char '('
                    return [x]
    list <- readCompoundListOrEmpty
    allspacing
    char ')'
    id <- endSpan start
    return $ T_ProcSub id dir list

prop_readSingleQuoted = isOk readSingleQuoted "'foo bar'"
prop_readSingleQuoted2 = isWarning readSingleQuoted "'foo bar\\'"
prop_readSingleQuoted4 = isWarning readNormalWord "'it's"
prop_readSingleQuoted5 = isWarning readSimpleCommand "foo='bar\ncow 'arg"
prop_readSingleQuoted6 = isOk readSimpleCommand "foo='bar cow 'arg"
prop_readSingleQuoted7 = isOk readSingleQuoted "'foo\x201C\&bar'"
prop_readSingleQuoted8 = isWarning readSingleQuoted "'foo\x2018\&bar'"
readSingleQuoted = called "single quoted string" $ do
    start <- startSpan
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

    id <- endSpan start
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
    start <- startSpan
    startPos <- getPosition
    backtick
    subStart <- getPosition
    subString <- readGenericLiteral "`´"
    endPos <- getPosition
    backtick
    id <- endSpan start

    optional $ do
        c <- try . lookAhead $ suspectCharAfterQuotes
        when ('\n' `elem` subString && not ("\n" `isPrefixOf` subString)) $
            suggestForgotClosingQuote startPos endPos "backtick expansion"

    -- Result positions may be off due to escapes
    result <- subParse subStart (tryWithErrors subParser <|> return []) (unEscape subString)
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

-- Run a parser on a new input, such as for `..` or here documents.
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
prop_readDoubleQuoted10 = isOk readDoubleQuoted "\"foo\\\\n\""
readDoubleQuoted = called "double quoted string" $ do
    start <- startSpan
    startPos <- getPosition
    doubleQuote
    x <- many doubleQuotedPart
    endPos <- getPosition
    doubleQuote <|> fail "Expected end of double quoted string"
    id <- endSpan start
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
        start <- startSpan
        c <- oneOf unicodeDoubleQuotes
        id <- endSpan start
        parseProblemAt pos WarningC 1111
            "This is a unicode quote. Delete and retype it (or ignore/singlequote for literal)."
        return $ T_Literal id [c]

readDoubleLiteral = do
    start <- startSpan
    s <- many1 readDoubleLiteralPart
    id <- endSpan start
    return $ T_Literal id (concat s)

readDoubleLiteralPart = do
    x <- many1 (readDoubleEscaped <|> many1 (noneOf (doubleQuotableChars ++ unicodeDoubleQuotes)))
    return $ concat x

readNormalLiteral end = do
    start <- startSpan
    s <- many1 (readNormalLiteralPart end)
    id <- endSpan start
    return $ T_Literal id (concat s)

prop_readGlob1 = isOk readGlob "*"
prop_readGlob2 = isOk readGlob "[^0-9]"
prop_readGlob3 = isOk readGlob "[a[:alpha:]]"
prop_readGlob4 = isOk readGlob "[[:alnum:]]"
prop_readGlob5 = isOk readGlob "[^[:alpha:]1-9]"
prop_readGlob6 = isOk readGlob "[\\|]"
prop_readGlob7 = isOk readGlob "[^[]"
prop_readGlob8 = isOk readGlob "[*?]"
prop_readGlob9 = isOk readGlob "[!]^]"
prop_readGlob10 = isOk readGlob "[]]"
readGlob = readExtglob <|> readSimple <|> readClass <|> readGlobbyLiteral
    where
        readSimple = do
            start <- startSpan
            c <- oneOf "*?"
            id <- endSpan start
            return $ T_Glob id [c]
        readClass = try $ do
            start <- startSpan
            char '['
            negation <- charToString (oneOf "!^") <|> return ""
            leadingBracket <- charToString (oneOf "]") <|> return ""
            s <- many (predefined <|> readNormalLiteralPart "]" <|> globchars)
            guard $ not (null leadingBracket) || not (null s)
            char ']'
            id <- endSpan start
            return $ T_Glob id $ "[" ++ concat (negation:leadingBracket:s) ++ "]"
          where
           globchars = charToString $ oneOf $ "![" ++ extglobStartChars
           predefined = do
              try $ string "[:"
              s <- many1 letter
              string ":]"
              return $ "[:" ++ s ++ ":]"

        charToString = fmap return
        readGlobbyLiteral = do
            start <- startSpan
            c <- extglobStart <|> char '['
            id <- endSpan start
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
        -- Check if this line is followed by a commented line with a trailing backslash
        when (next == '\n') $ try . lookAhead $ void spacing
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
    alternative t = "\"$(printf '\\" ++ [t] ++ "')\""
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
    start <- startSpan
    c <- try $ do
            f <- extglobStart
            char '('
            return f
    contents <- readExtglobPart `sepBy` char '|'
    char ')'
    id <- endSpan start
    return $ T_Extglob id [c] contents

readExtglobPart = do
    start <- startSpan
    x <- many (readExtglobGroup <|> readNormalWordPart "" <|> readSpacePart <|> readExtglobLiteral)
    id <- endSpan start
    return $ T_NormalWord id x
  where
    readExtglobGroup = do
        char '('
        start <- startSpan
        contents <- readExtglobPart `sepBy` char '|'
        id <- endSpan start
        char ')'
        return $ T_Extglob id "" contents
    readExtglobLiteral = do
        start <- startSpan
        str <- many1 (oneOf "<>#;&")
        id <- endSpan start
        return $ T_Literal id str


readSingleEscaped = do
    pos <- getPosition
    s <- backslash
    x <- lookAhead anyChar

    case x of
        '\'' -> parseProblemAt pos InfoC 1003 "Want to escape a single quote? echo 'This is how it'\\''s done'.";
        _ -> return ()

    return [s]

readDoubleEscaped = do
    pos <- getPosition
    bs <- backslash
    (linefeed >> return "")
        <|> fmap return doubleQuotable
        <|> do
            c <- anyChar
            -- This is an invalid escape sequence where the \ is literal.
            -- Previously this caused a SC1117, which may be re-enabled as
            -- as a pedantic warning.
            return [bs, c]

readBraceEscaped = do
    bs <- backslash
    (linefeed >> return "")
        <|> fmap return bracedQuotable
        <|> fmap (\ x -> [bs, x]) anyChar


readGenericLiteral endChars = do
    strings <- many (readGenericEscaped <|> many1 (noneOf ('\\':endChars)))
    return $ concat strings

readGenericLiteral1 endExp = do
    strings <- (readGenericEscaped <|> ((\x -> [x]) <$> anyChar)) `reluctantlyTill1` endExp
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
    readDollarExp <|> readDollarDoubleQuote <|> readDollarSingleQuote <|> readDollarLonely False
readDoubleQuotedDollar = do
    ensureDollar
    readDollarExp <|> readDollarLonely True


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
        parseNoteAt pos ErrorC 1102 "Shells disambiguate $(( differently or not at all. For $(command substitution), add space after $( . For $((arithmetics)), fix parsing errors.")

prop_readDollarSingleQuote = isOk readDollarSingleQuote "$'foo\\\'lol'"
readDollarSingleQuote = called "$'..' expression" $ do
    start <- startSpan
    try $ string "$'"
    str <- readGenericLiteral "'"
    char '\''
    id <- endSpan start
    return $ T_DollarSingleQuoted id str

prop_readDollarDoubleQuote = isOk readDollarDoubleQuote "$\"hello\""
readDollarDoubleQuote = do
    lookAhead . try $ string "$\""
    start <- startSpan
    char '$'
    doubleQuote
    x <- many doubleQuotedPart
    doubleQuote <|> fail "Expected end of translated double quoted string"
    id <- endSpan start
    return $ T_DollarDoubleQuoted id x

prop_readDollarArithmetic = isOk readDollarArithmetic "$(( 3 * 4 +5))"
prop_readDollarArithmetic2 = isOk readDollarArithmetic "$(((3*4)+(1*2+(3-1))))"
readDollarArithmetic = called "$((..)) expression" $ do
    start <- startSpan
    try (string "$((")
    c <- readArithmeticContents
    pos <- getPosition
    char ')'
    char ')' <|> fail "Expected a double )) to end the $((..))"
    id <- endSpan start
    return (T_DollarArithmetic id c)

readDollarBracket = called "$[..] expression" $ do
    start <- startSpan
    try (string "$[")
    c <- readArithmeticContents
    string "]"
    id <- endSpan start
    return (T_DollarBracket id c)

prop_readArithmeticExpression = isOk readArithmeticExpression "((a?b:c))"
readArithmeticExpression = called "((..)) command" $ do
    start <- startSpan
    try (string "((")
    c <- readArithmeticContents
    string "))"
    id <- endSpan start
    spacing
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
    start <- startSpan
    try $ do
        string "${"
        whitespace
    allspacing
    term <- readTerm
    char '}' <|> fail "Expected } to end the ksh ${ ..; } command expansion"
    id <- endSpan start
    return $ T_DollarBraceCommandExpansion id term

prop_readDollarBraced1 = isOk readDollarBraced "${foo//bar/baz}"
prop_readDollarBraced2 = isOk readDollarBraced "${foo/'{cow}'}"
prop_readDollarBraced3 = isOk readDollarBraced "${foo%%$(echo cow\\})}"
prop_readDollarBraced4 = isOk readDollarBraced "${foo#\\}}"
readDollarBraced = called "parameter expansion" $ do
    start <- startSpan
    try (string "${")
    word <- readDollarBracedWord
    char '}'
    id <- endSpan start
    return $ T_DollarBraced id True word

prop_readDollarExpansion1 = isOk readDollarExpansion "$(echo foo; ls\n)"
prop_readDollarExpansion2 = isOk readDollarExpansion "$(  )"
prop_readDollarExpansion3 = isOk readDollarExpansion "$( command \n#comment \n)"
readDollarExpansion = called "command expansion" $ do
    start <- startSpan
    try (string "$(")
    cmds <- readCompoundListOrEmpty
    char ')' <|> fail "Expected end of $(..) expression"
    id <- endSpan start
    return $ T_DollarExpansion id cmds

prop_readDollarVariable = isOk readDollarVariable "$@"
prop_readDollarVariable2 = isOk (readDollarVariable >> anyChar) "$?!"
prop_readDollarVariable3 = isWarning (readDollarVariable >> anyChar) "$10"
prop_readDollarVariable4 = isWarning (readDollarVariable >> string "[@]") "$arr[@]"
prop_readDollarVariable5 = isWarning (readDollarVariable >> string "[f") "$arr[f"

readDollarVariable :: Monad m => SCParser m Token
readDollarVariable = do
    start <- startSpan
    pos <- getPosition

    let singleCharred p = do
        value <- wrapString ((:[]) <$> p)
        id <- endSpan start
        return $ (T_DollarBraced id False value)

    let positional = do
        value <- singleCharred digit
        return value `attempting` do
            lookAhead digit
            parseNoteAt pos ErrorC 1037 "Braces are required for positionals over 9, e.g. ${10}."

    let special = singleCharred specialVariable

    let regular = do
        value <- wrapString readVariableName
        id <- endSpan start
        return (T_DollarBraced id False value) `attempting` do
            lookAhead $ char '['
            parseNoteAt pos ErrorC 1087 "Use braces when expanding arrays, e.g. ${array[idx]} (or ${var}[.. to quiet)."

    try $ char '$' >> (positional <|> special <|> regular)

  where
    wrapString p = do
        start <- getPosition
        s <- p
        end <- getPosition
        id1 <- getNextIdBetween start end
        id2 <- getNextIdBetween start end
        return $ T_NormalWord id1 [T_Literal id2 s]

readVariableName = do
    f <- variableStart
    rest <- many variableChars
    return (f:rest)


prop_readDollarLonely1 = isWarning readNormalWord "\"$\"var"
prop_readDollarLonely2 = isWarning readNormalWord "\"$\"\"var\""
prop_readDollarLonely3 = isOk readNormalWord "\"$\"$var"
prop_readDollarLonely4 = isOk readNormalWord "\"$\"*"
prop_readDollarLonely5 = isOk readNormalWord "$\"str\""
readDollarLonely quoted = do
    start <- startSpan
    char '$'
    id <- endSpan start
    when quoted $ do
        isHack <- quoteForEscape
        when isHack $
            parseProblemAtId id StyleC 1135
                "Prefer escape over ending quote to make $ literal. Instead of \"It costs $\"5, use \"It costs \\$5\"."
    return $ T_Literal id "$"
  where
    quoteForEscape = option False $ try . lookAhead $ do
        char '"'
        -- Check for "foo $""bar"
        optional $ char '"'
        c <- anyVar
        -- Don't trigger on [[ x == "$"* ]] or "$"$pattern
        return $ c `notElem` "*$"
    anyVar = variableStart <|> digit <|> specialVariable


prop_readHereDoc = isOk readScript "cat << foo\nlol\ncow\nfoo"
prop_readHereDoc2 = isNotOk readScript "cat <<- EOF\n  cow\n  EOF"
prop_readHereDoc3 = isOk readScript "cat << foo\n$\"\nfoo"
prop_readHereDoc4 = isNotOk readScript "cat << foo\n`\nfoo"
prop_readHereDoc5 = isOk readScript "cat <<- !foo\nbar\n!foo"
prop_readHereDoc6 = isOk readScript "cat << foo\\ bar\ncow\nfoo bar"
prop_readHereDoc7 = isOk readScript "cat << foo\n\\$(f ())\nfoo"
prop_readHereDoc8 = isOk readScript "cat <<foo>>bar\netc\nfoo"
prop_readHereDoc9 = isOk readScript "if true; then cat << foo; fi\nbar\nfoo\n"
prop_readHereDoc10 = isOk readScript "if true; then cat << foo << bar; fi\nfoo\nbar\n"
prop_readHereDoc11 = isOk readScript "cat << foo $(\nfoo\n)lol\nfoo\n"
prop_readHereDoc12 = isOk readScript "cat << foo|cat\nbar\nfoo"
prop_readHereDoc13 = isOk readScript "cat <<'#!'\nHello World\n#!\necho Done"
prop_readHereDoc14 = isWarning readScript "cat << foo\nbar\nfoo \n"
prop_readHereDoc15 = isWarning readScript "cat <<foo\nbar\nfoo bar\nfoo"
prop_readHereDoc16 = isOk readScript "cat <<- ' foo'\nbar\n foo\n"
prop_readHereDoc17 = isWarning readScript "cat <<- ' foo'\nbar\n  foo\n foo\n"
prop_readHereDoc18 = isOk readScript "cat <<'\"foo'\nbar\n\"foo\n"
prop_readHereDoc20 = isWarning readScript "cat << foo\n  foo\n()\nfoo\n"
prop_readHereDoc21 = isOk readScript "# shellcheck disable=SC1039\ncat << foo\n  foo\n()\nfoo\n"
prop_readHereDoc22 = isWarning readScript "cat << foo\r\ncow\r\nfoo\r\n"
prop_readHereDoc23 = isNotOk readScript "cat << foo \r\ncow\r\nfoo\r\n"
readHereDoc = called "here document" $ do
    pos <- getPosition
    try $ string "<<"
    dashed <- (char '-' >> return Dashed) <|> return Undashed
    sp <- spacing
    optional $ do
        try . lookAhead $ char '('
        let message = "Shells are space sensitive. Use '< <(cmd)', not '<<" ++ sp ++ "(cmd)'."
        parseProblemAt pos ErrorC 1038 message
    start <- startSpan
    (quoted, endToken) <- readToken
    hid <- endSpan start

    -- add empty tokens for now, read the rest in readPendingHereDocs
    let doc = T_HereDoc hid dashed quoted endToken []
    addPendingHereDoc hid dashed quoted endToken
    return doc
  where
    unquote :: String -> (Quoted, String)
    unquote "" = (Unquoted, "")
    unquote [c] = (Unquoted, [c])
    unquote s@(cl:tl) =
      case reverse tl of
        (cr:tr) | cr == cl && cl `elem` "\"'" -> (Quoted, reverse tr)
        _ -> (if '\\' `elem` s then (Quoted, filter ((/=) '\\') s) else (Unquoted, s))
    -- Fun fact: bash considers << foo"" quoted, but not << <("foo").
    readToken = do
        str <- readStringForParser readNormalWord
        -- A here doc actually works with \r\n because the \r becomes part of the token
        crstr <- (carriageReturn >> (return $ str ++ "\r")) <|> return str
        return $ unquote crstr

readPendingHereDocs = do
    docs <- popPendingHereDocs
    mapM_ readDoc docs
  where
    readDoc (HereDocPending id dashed quoted endToken ctx) =
      swapContext ctx $
      do
        docStartPos <- getPosition
        (terminated, wasWarned, lines) <- readDocLines dashed endToken
        docEndPos <- getPosition
        let hereData = unlines lines
        unless terminated $ do
            unless wasWarned $
                debugHereDoc id endToken hereData
            fail "Here document was not correctly terminated"
        list <- parseHereData quoted (docStartPos, docEndPos) hereData
        addToHereDocMap id list

    -- Read the lines making up the here doc. Returns (IsTerminated, Lines)
    readDocLines :: Monad m => Dashed -> String -> SCParser m (Bool, Bool, [String])
    readDocLines dashed endToken = do
        pos <- getPosition
        str <- rawLine
        isEof <- option False (eof >> return True)
        (isEnd, wasWarned) <- subParse pos checkEnd str
        if
            | isEnd -> return (True, wasWarned, [])
            | isEof -> return (False, wasWarned, [str])
            | True -> do
                (ok, previousWarning, rest) <- readDocLines dashed endToken
                return (ok, wasWarned || previousWarning, str:rest)
      where
        -- Check if this is the actual end, or a plausible false end
        checkEnd = option (False, False) $ try $ do
            -- Match what's basically '^( *)token( *)(.*)$'
            leadingSpacePos <- getPosition
            leadingSpace <- linewhitespace `reluctantlyTill` string endToken
            string endToken
            trailingSpacePos <- getPosition
            trailingSpace <- many linewhitespace
            trailerPos <- getPosition
            trailer <- many anyChar

            let leadingSpacesAreTabs = all (== '\t') leadingSpace
            let thereIsNoTrailer = null trailingSpace && null trailer
            let leaderIsOk = null leadingSpace
                    || dashed == Dashed && leadingSpacesAreTabs
            let trailerStart = case trailer of [] -> '\0'; (h:_) -> h
            let hasTrailingSpace = not $ null trailingSpace
            let hasTrailer = not $ null trailer
            let ppt = parseProblemAt trailerPos ErrorC

            if leaderIsOk && thereIsNoTrailer
              then return (True, False)
              else do
                let foundCause = return (False, True)
                let skipLine = return (False, False)
                -- This may be intended as an end token. Debug why it isn't.
                if
                    | trailerStart == ')' -> do
                        ppt 1119 $ "Add a linefeed between end token and terminating ')'."
                        foundCause
                    | trailerStart == '#' -> do
                        ppt 1120 "No comments allowed after here-doc token. Comment the next line instead."
                        foundCause
                    | trailerStart `elem` ";>|&" -> do
                        ppt 1121 "Add ;/& terminators (and other syntax) on the line with the <<, not here."
                        foundCause
                    | hasTrailingSpace && hasTrailer -> do
                        ppt 1122 "Nothing allowed after end token. To continue a command, put it on the line with the <<."
                        foundCause
                    | leaderIsOk && hasTrailingSpace && not hasTrailer -> do
                        parseProblemAt trailingSpacePos ErrorC 1118 "Delete whitespace after the here-doc end token."
                        -- Parse as if it's the actual end token. Will koala_man regret this once again?
                        return (True, True)
                    | not hasTrailingSpace && hasTrailer ->
                        -- The end token is just a prefix
                        skipLine
                    | hasTrailer ->
                        error $ pleaseReport "unexpected heredoc trailer"

                    -- The following cases assume no trailing text:
                    | dashed == Undashed && (not $ null leadingSpace) -> do
                        parseProblemAt leadingSpacePos ErrorC 1039 "Remove indentation before end token (or use <<- and indent with tabs)."
                        foundCause
                    | dashed == Dashed && not leadingSpacesAreTabs -> do
                        parseProblemAt leadingSpacePos ErrorC 1040 "When using <<-, you can only indent with tabs."
                        foundCause
                    | True -> skipLine

    rawLine = do
        c <- many $ noneOf "\n"
        void (char '\n') <|> eof
        return c

    parseHereData Quoted (start,end) hereData = do
        id <- getNextIdBetween start end
        return [T_Literal id hereData]

    parseHereData Unquoted (startPos, _) hereData =
        subParse startPos readHereData hereData

    readHereData = many $ doubleQuotedPart <|> readHereLiteral

    readHereLiteral = do
        start <- startSpan
        chars <- many1 $ noneOf "`$\\"
        id <- endSpan start
        return $ T_Literal id chars

    debugHereDoc tokenId endToken doc
        | endToken `isInfixOf` doc =
            let lookAt line = when (endToken `isInfixOf` line) $
                      parseProblemAtId tokenId ErrorC 1042 ("Close matches include '" ++ (e4m line) ++ "' (!= '" ++ (e4m endToken) ++ "').")
            in do
                  parseProblemAtId tokenId ErrorC 1041 ("Found '" ++ (e4m endToken) ++ "' further down, but not on a separate line.")
                  mapM_ lookAt (lines doc)
        | map toLower endToken `isInfixOf` map toLower doc =
            parseProblemAtId tokenId ErrorC 1043 ("Found " ++ (e4m endToken) ++ " further down, but with wrong casing.")
        | otherwise =
            parseProblemAtId tokenId ErrorC 1044 ("Couldn't find end token `" ++ (e4m endToken) ++ "' in the here document.")


readFilename = readNormalWord
readIoFileOp = choice [g_DGREAT, g_LESSGREAT, g_GREATAND, g_LESSAND, g_CLOBBER, redirToken '<' T_Less, redirToken '>' T_Greater ]

readIoDuplicate = try $ do
    start <- startSpan
    op <- g_GREATAND <|> g_LESSAND
    target <- readIoVariable <|> digitsAndOrDash
    id <- endSpan start
    return $ T_IoDuplicate id op target
  where
    -- either digits with optional dash, or a required dash
    digitsAndOrDash = do
        str <- many digit
        dash <- (if null str then id else option "") $ string "-"
        return $ str ++ dash


prop_readIoFile = isOk readIoFile ">> \"$(date +%YYmmDD)\""
readIoFile = called "redirection" $ do
    start <- startSpan
    op <- readIoFileOp
    spacing
    file <- readFilename
    id <- endSpan start
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
prop_readIoRedirect7 = isOk readIoRedirect "{foo}>&1-"
readIoRedirect = do
    start <- startSpan
    n <- readIoSource
    redir <- readHereString <|> readHereDoc <|> readIoDuplicate <|> readIoFile
    id <- endSpan start
    skipAnnotationAndWarn
    spacing
    return $ T_FdRedirect id n redir

prop_readHereString = isOk readHereString "<<< \"Hello $world\""
readHereString = called "here string" $ do
    start <- startSpan
    try $ string "<<<"
    id <- endSpan start
    spacing
    word <- readNormalWord
    return $ T_HereString id word

prop_readNewlineList1 = isOk readScript "&> /dev/null echo foo"
readNewlineList =
    many1 ((linefeed <|> carriageReturn) `thenSkip` spacing) <* checkBadBreak
  where
    checkBadBreak = optional $ do
                pos <- getPosition
                try $ lookAhead (oneOf "|&") --  See if the next thing could be |, || or &&
                notFollowedBy2 (string "&>") --  Except &> or &>> which is valid
                parseProblemAt pos ErrorC 1133
                    "Unexpected start of line. If breaking lines, |/||/&& should be at the end of the previous one."
readLineBreak = optional readNewlineList

prop_readSeparator1 = isWarning readScript "a &; b"
prop_readSeparator2 = isOk readScript "a & b"
prop_readSeparator3 = isWarning readScript "a &amp; b"
prop_readSeparator4 = isWarning readScript "a &gt; file; b"
prop_readSeparator5 = isWarning readScript "curl https://example.com/?foo=moo&bar=cow"
readSeparatorOp = do
    notFollowedBy2 (void g_AND_IF <|> void readCaseSeparator)
    notFollowedBy2 (string "&>")
    start <- getPosition
    f <- try (do
                    pos <- getPosition
                    char '&'
                    optional $ choice [
                        do
                            s <- lookAhead . choice . map (try . string) $
                                ["amp;", "gt;", "lt;"]
                            parseProblemAt pos ErrorC 1109 "This is an unquoted HTML entity. Replace with corresponding character.",

                        do
                            try . lookAhead $ variableStart
                            parseProblemAt pos WarningC 1132 "This & terminates the command. Escape it or add space after & to silence."
                      ]

                    spacing
                    pos <- getPosition
                    char ';'
                    -- In case statements we might have foo & ;;
                    notFollowedBy2 $ char ';'
                    parseProblemAt pos ErrorC 1045 "It's not 'foo &; bar', just 'foo & bar'."
                    return '&'
            ) <|> char ';' <|> char '&'
    end <- getPosition
    spacing
    return (f, (start, end))

readSequentialSep = void (g_Semi >> readLineBreak) <|> void readNewlineList
readSeparator =
    do
        separator <- readSeparatorOp
        readLineBreak
        return separator
     <|>
        do
            start <- getPosition
            readNewlineList
            end <- getPosition
            return ('\n', (start, end))

prop_readSimpleCommand = isOk readSimpleCommand "echo test > file"
prop_readSimpleCommand2 = isOk readSimpleCommand "cmd &> file"
prop_readSimpleCommand3 = isOk readSimpleCommand "export foo=(bar baz)"
prop_readSimpleCommand4 = isOk readSimpleCommand "typeset -a foo=(lol)"
prop_readSimpleCommand5 = isOk readSimpleCommand "time if true; then echo foo; fi"
prop_readSimpleCommand6 = isOk readSimpleCommand "time -p ( ls -l; )"
prop_readSimpleCommand7 = isOk readSimpleCommand "\\ls"
prop_readSimpleCommand7b = isOk readSimpleCommand "\\:"
prop_readSimpleCommand8 = isWarning readSimpleCommand "// Lol"
prop_readSimpleCommand9 = isWarning readSimpleCommand "/* Lolbert */"
prop_readSimpleCommand10 = isWarning readSimpleCommand "/**** Lolbert */"
prop_readSimpleCommand11 = isOk readSimpleCommand "/\\* foo"
prop_readSimpleCommand12 = isWarning readSimpleCommand "elsif foo"
prop_readSimpleCommand13 = isWarning readSimpleCommand "ElseIf foo"
prop_readSimpleCommand14 = isWarning readSimpleCommand "elseif[$i==2]"
prop_readSimpleCommand15 = isWarning readSimpleCommand "trap 'foo\"bar' INT"
readSimpleCommand = called "simple command" $ do
    prefix <- option [] readCmdPrefix
    skipAnnotationAndWarn
    cmd <- option Nothing $ Just <$> readCmdName
    when (null prefix && isNothing cmd) $ fail "Expected a command"

    case cmd of
      Nothing -> do
        id1 <- getNextIdSpanningTokenList prefix
        id2 <- getNewIdFor id1
        return $ makeSimpleCommand id1 id2 prefix [] []

      Just cmd -> do
            validateCommand cmd
            -- We have to ignore possible parsing problems from the lookAhead parser
            firstArgument <- ignoreProblemsOf . optionMaybe . try . lookAhead $ readCmdWord
            suffix <- option [] $ getParser readCmdSuffix
                    -- If `export` or other modifier commands are called with `builtin` we have to look at the first argument
                    (if isCommand ["builtin"] cmd then fromMaybe cmd firstArgument else cmd) [
                        (["declare", "export", "local", "readonly", "typeset"], readModifierSuffix),
                        (["time"], readTimeSuffix),
                        (["let"], readLetSuffix),
                        (["eval"], readEvalSuffix)
                    ]

            id1 <- getNextIdSpanningTokenList (prefix ++ (cmd:suffix))
            id2 <- getNewIdFor id1

            let result = makeSimpleCommand id1 id2 prefix [cmd] suffix
            case () of
                _ | isCommand ["source", "."] cmd -> readSource result
                _ | isCommand ["trap"] cmd -> do
                        syntaxCheckTrap result
                        return result
                _ -> return result
  where
    isCommand strings (T_NormalWord _ [T_Literal _ s]) = s `elem` strings
    isCommand _ _ = False
    getParser def cmd [] = def
    getParser def cmd ((list, action):rest) =
        if isCommand list cmd
        then action
        else getParser def cmd rest

    validateCommand cmd =
        case cmd of
            (T_NormalWord _ [T_Literal _ "//"]) -> commentWarning (getId cmd)
            (T_NormalWord _ (T_Literal _ "/" : T_Glob _ "*" :_)) -> commentWarning (getId cmd)
            (T_NormalWord _ (T_Literal _ str:_)) -> do
                let cmdString = map toLower $ takeWhile isAlpha str
                when (cmdString `elem` ["elsif", "elseif"]) $
                    parseProblemAtId (getId cmd) ErrorC 1131 "Use 'elif' to start another branch."
            _ -> return ()

    syntaxCheckTrap cmd =
        case cmd of
            (T_Redirecting _ _ (T_SimpleCommand _ _ (cmd:arg:_))) -> checkArg arg (getLiteralString arg)
            _ -> return ()
      where
        checkArg _ Nothing = return ()
        checkArg arg (Just ('-':_)) = return ()
        checkArg arg (Just str) = do
            (start,end) <- getSpanForId (getId arg)
            subParse start (tryWithErrors (readCompoundListOrEmpty >> verifyEof) <|> return ()) str

    commentWarning id =
        parseProblemAtId id ErrorC 1127 "Was this intended as a comment? Use # in sh."

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


readSource :: Monad m => Token -> SCParser m Token
readSource t@(T_Redirecting _ _ (T_SimpleCommand cmdId _ (cmd:file':rest'))) = do
    let file = getFile file' rest'
    override <- getSourceOverride
    let literalFile = do
        name <- override `mplus` getLiteralString file `mplus` stripDynamicPrefix file
        -- Hack to avoid 'source ~/foo' trying to read from literal tilde
        guard . not $ "~/" `isPrefixOf` name
        return name
    case literalFile of
        Nothing -> do
            parseNoteAtId (getId file) WarningC 1090
                "ShellCheck can't follow non-constant source. Use a directive to specify location."
            return t
        Just filename -> do
            proceed <- shouldFollow filename
            if not proceed
              then do
                -- FIXME: This actually gets squashed without -a
                parseNoteAtId (getId file) InfoC 1093
                    "This file appears to be recursively sourced. Ignoring."
                return t
              else do
                sys <- Mr.asks systemInterface
                (input, resolvedFile) <-
                    if filename == "/dev/null" -- always allow /dev/null
                    then return (Right "", filename)
                    else do
                        allAnnotations <- getCurrentAnnotations True
                        currentScript <- Mr.asks currentFilename
                        let paths = mapMaybe getSourcePath allAnnotations
                        let externalSources = listToMaybe $ mapMaybe getExternalSources allAnnotations
                        resolved <- system $ siFindSource sys currentScript externalSources paths filename
                        contents <- system $ siReadFile sys externalSources resolved
                        return (contents, resolved)
                case input of
                    Left err -> do
                        parseNoteAtId (getId file) InfoC 1091 $
                            "Not following: " ++ err
                        return t
                    Right script -> do
                        id1 <- getNewIdFor cmdId
                        id2 <- getNewIdFor cmdId

                        let included = do
                            src <- subRead resolvedFile script
                            return $ T_SourceCommand id1 t (T_Include id2 src)

                        let failed = do
                            parseNoteAtId (getId file) WarningC 1094
                                "Parsing of sourced file failed. Ignoring it."
                            return t

                        included <|> failed
  where
    getFile :: Token -> [Token] -> Token
    getFile file (next:rest) =
        case getLiteralString file of
            Just "--" -> next
            x -> file
    getFile file _ = file

    getSourcePath t =
        case t of
            SourcePath x -> Just x
            _ -> Nothing

    getExternalSources t =
        case t of
            ExternalSources b -> Just b
            _ -> Nothing

    -- If the word has a single expansion as the directory, try stripping it
    -- This affects `$foo/bar` but not `${foo}-dir/bar` or `/foo/$file`
    stripDynamicPrefix word =
        case getWordParts word of
            exp : rest | isStringExpansion exp -> do
                str <- getLiteralString (T_NormalWord (Id 0) rest)
                guard $ "/" `isPrefixOf` str
                return $ "." ++ str
            _ -> Nothing

    subRead name script =
        withContext (ContextSource name) $
            inSeparateContext $ do
                oldState <- getState
                setState $ oldState { pendingHereDocs = [] }
                result <- subParse (initialPos name) (readScriptFile True) script
                newState <- getState
                setState $ newState { pendingHereDocs = pendingHereDocs oldState }
                return result
readSource t = return t


prop_readPipeline = isOk readPipeline "! cat /etc/issue | grep -i ubuntu"
prop_readPipeline2 = isWarning readPipeline "!cat /etc/issue | grep -i ubuntu"
prop_readPipeline3 = isOk readPipeline "for f; do :; done|cat"
prop_readPipeline4 = isOk readPipeline "! ! true"
prop_readPipeline5 = isOk readPipeline "true | ! true"
readPipeline = do
    unexpecting "keyword/token" readKeyword
    readBanged readPipeSequence

readBanged parser = do
    pos <- getPosition
    (T_Bang id) <- g_Bang
    next <- readBanged parser
    return $ T_Banged id next
 <|> parser

prop_readAndOr = isOk readAndOr "grep -i lol foo || exit 1"
prop_readAndOr1 = isOk readAndOr "# shellcheck disable=1\nfoo"
prop_readAndOr2 = isOk readAndOr "# shellcheck disable=1\n# lol\n# shellcheck disable=3\nfoo"
readAndOr = do
    start <- startSpan
    apos <- getPosition
    annotations <- readAnnotations
    aid <- endSpan start

    unless (null annotations) $ optional $ do
        try . lookAhead $ readKeyword
        parseProblemAt apos ErrorC 1123 "ShellCheck directives are only valid in front of complete compound commands, like 'if', not e.g. individual 'elif' branches."

    andOr <- withAnnotations annotations $
        chainl1 readPipeline $ do
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
  where
    readTerm' current =
        do
            (sep, (start, end)) <- readSeparator
            id <- getNextIdBetween start end
            more <- option (T_EOF id) readAndOr
            case more of (T_EOF _) -> return [transformWithSeparator id sep current]
                         _         -> do
                                    list <- readTerm' more
                                    return (transformWithSeparator id sep current : list)
          <|>
            return [current]
      where
        transformWithSeparator i '&' = T_Backgrounded i
        transformWithSeparator i _  = id


readPipeSequence = do
    start <- startSpan
    (cmds, pipes) <- sepBy1WithSeparators (readBanged readCommand)
                        (readPipe `thenSkip` (spacing >> readLineBreak))
    id <- endSpan start
    spacing
    return $ T_Pipeline id pipes cmds
  where
    sepBy1WithSeparators p s = do
        let elems = (\x -> ([x], [])) <$> p
        let seps = do
            separator <- s
            return $ \(a,b) (c,d) -> (a++c, b ++ d ++ [separator])
        elems `chainl1` seps

readPipe = do
    notFollowedBy2 g_OR_IF
    start <- startSpan
    char '|'
    qualifier <- string "&" <|> return ""
    id <- endSpan start
    spacing
    return $ T_Pipe id ('|':qualifier)

readCommand = choice [
    readCompoundCommand,
    readConditionCommand,
    readCoProc,
    readSimpleCommand
    ]

readCmdName = do
    -- If the command name is `!` then
    optional . lookAhead . try $ do
        char '!'
        whitespace
    -- Ignore alias suppression
    optional . try $ do
        char '\\'
        lookAhead $ variableChars <|> oneOf ":."
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
prop_readIfClause4 = isWarning readIfClause "if false; then true; else if true; then echo lol; fi; fi"
prop_readIfClause5 = isOk readIfClause "if false; then true; else\nif true; then echo lol; fi; fi"
prop_readIfClause6 = isWarning readIfClause "if true\nthen\nDo the thing\nfi"
readIfClause = called "if expression" $ do
    start <- startSpan
    pos <- getPosition
    (condition, action) <- readIfPart
    elifs <- many readElifPart
    elses <- option [] readElsePart

    g_Fi `orFail` do
        parseProblemAt pos ErrorC 1046 "Couldn't find 'fi' for this 'if'."
        parseProblem ErrorC 1047 "Expected 'fi' matching previously mentioned 'if'."
        return "Expected 'fi'"
    id <- endSpan start

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
    g_Elif
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

readElsePart = called "else clause" $ do
    pos <- getPosition
    g_Else
    optional $ do
        try . lookAhead $ g_If
        parseProblemAt pos ErrorC 1075 "Use 'elif' instead of 'else if' (or put 'if' on new line if nesting)."

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
    start <- startSpan
    char '('
    allspacing
    list <- readCompoundList
    allspacing
    char ')' <|> fail "Expected ) closing the subshell"
    id <- endSpan start
    spacing
    return $ T_Subshell id list

prop_readBraceGroup = isOk readBraceGroup "{ a; b | c | d; e; }"
prop_readBraceGroup2 = isWarning readBraceGroup "{foo;}"
prop_readBraceGroup3 = isOk readBraceGroup "{(foo)}"
readBraceGroup = called "brace group" $ do
    start <- startSpan
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
    id <- endSpan start
    spacing
    return $ T_BraceGroup id list

prop_readBatsTest1 = isOk readBatsTest "@test 'can parse' {\n  true\n}"
prop_readBatsTest2 = isOk readBatsTest "@test random text !(@*$Y&! {\n  true\n}"
prop_readBatsTest3 = isOk readBatsTest "@test foo { bar { baz {\n  true\n}"
prop_readBatsTest4 = isNotOk readBatsTest "@test foo \n{\n true\n}"
readBatsTest = called "bats @test" $ do
    start <- startSpan
    try $ string "@test "
    spacing
    name <- readBatsName
    spacing
    test <- readBraceGroup
    id <- endSpan start
    return $ T_BatsTest id name test
  where
    readBatsName = do
        line <- try . lookAhead $ many1 $ noneOf "\n"
        let name = reverse $ f $ reverse line
        string name

    -- We want everything before the last " {" in a string, so we find everything after "{ " in its reverse
    f ('{':' ':rest) = dropWhile isSpace rest
    f (a:rest) = f rest
    f [] = ""

prop_readWhileClause = isOk readWhileClause "while [[ -e foo ]]; do sleep 1; done"
readWhileClause = called "while loop" $ do
    start <- startSpan
    kwId <- getId <$> g_While
    condition <- readTerm
    statements <- readDoGroup kwId
    id <- endSpan start
    return $ T_WhileExpression id condition statements

prop_readUntilClause = isOk readUntilClause "until kill -0 $PID; do sleep 1; done"
readUntilClause = called "until loop" $ do
    start <- startSpan
    kwId <- getId <$> g_Until
    condition <- readTerm
    statements <- readDoGroup kwId
    id <- endSpan start
    return $ T_UntilExpression id condition statements

readDoGroup kwId = do
    optional (do
                try . lookAhead $ g_Done
                parseProblemAtId kwId ErrorC 1057 "Did you forget the 'do' for this loop?")

    doKw <- g_Do `orFail` do
        parseProblem ErrorC 1058 "Expected 'do'."
        return "Expected 'do'"

    acceptButWarn g_Semi ErrorC 1059 "Semicolon is not allowed directly after 'do'. You can just delete it."
    allspacing

    optional (do
                try . lookAhead $ g_Done
                parseProblemAtId (getId doKw) ErrorC 1060 "Can't have empty do clauses (use 'true' as a no-op).")

    commands <- readCompoundList
    g_Done `orFail` do
            parseProblemAtId (getId doKw) ErrorC 1061 "Couldn't find 'done' for this 'do'."
            parseProblem ErrorC 1062 "Expected 'done' matching previously mentioned 'do'."
            return "Expected 'done'"

    optional . lookAhead $ do
        pos <- getPosition
        try $ string "<("
        parseProblemAt pos ErrorC 1142 "Use 'done < <(cmd)' to redirect from process substitution (currently missing one '<')."
    return commands


prop_readForClause = isOk readForClause "for f in *; do rm \"$f\"; done"
prop_readForClause1 = isOk readForClause "for f in *; { rm \"$f\"; }"
prop_readForClause3 = isOk readForClause "for f; do foo; done"
prop_readForClause4 = isOk readForClause "for((i=0; i<10; i++)); do echo $i; done"
prop_readForClause5 = isOk readForClause "for ((i=0;i<10 && n>x;i++,--n))\ndo \necho $i\ndone"
prop_readForClause6 = isOk readForClause "for ((;;))\ndo echo $i\ndone"
prop_readForClause7 = isOk readForClause "for ((;;)) do echo $i\ndone"
prop_readForClause8 = isOk readForClause "for ((;;)) ; do echo $i\ndone"
prop_readForClause9 = isOk readForClause "for i do true; done"
prop_readForClause10 = isOk readForClause "for ((;;)) { true; }"
prop_readForClause12 = isWarning readForClause "for $a in *; do echo \"$a\"; done"
prop_readForClause13 = isOk readForClause "for foo\nin\\\n  bar\\\n  baz\ndo true; done"
readForClause = called "for loop" $ do
    pos <- getPosition
    (T_For id) <- g_For
    spacing
    readArithmetic id <|> readRegular id
  where
    readArithmetic id = called "arithmetic for condition" $ do
        readArithmeticDelimiter '(' "Missing second '(' to start arithmetic for ((;;)) loop"
        x <- readArithmeticContents
        char ';' >> spacing
        y <- readArithmeticContents
        char ';' >> spacing
        z <- readArithmeticContents
        spacing
        readArithmeticDelimiter ')' "Missing second ')' to terminate 'for ((;;))' loop condition"
        spacing
        optional $ readSequentialSep >> spacing
        group <- readBraced <|> readDoGroup id
        return $ T_ForArithmetic id x y z group

    -- For c='(' read "((" and be lenient about spaces
    readArithmeticDelimiter c msg = do
        char c
        startPos <- getPosition
        sp <- spacing
        endPos <- getPosition
        char c <|> do
            parseProblemAt startPos ErrorC 1137 msg
            fail ""
        unless (null sp) $
            parseProblemAtWithEnd startPos endPos ErrorC 1138 $ "Remove spaces between " ++ [c,c] ++ " in arithmetic for loop."

    readBraced = do
        (T_BraceGroup _ list) <- readBraceGroup
        return list

    readRegular id = do
        acceptButWarn (char '$') ErrorC 1086
            "Don't use $ on the iterator name in for loops."
        name <- readVariableName `thenSkip` allspacing
        values <- readInClause <|> (optional readSequentialSep >> return [])
        group <- readBraced <|> readDoGroup id
        return $ T_ForIn id name values group

prop_readSelectClause1 = isOk readSelectClause "select foo in *; do echo $foo; done"
prop_readSelectClause2 = isOk readSelectClause "select foo; do echo $foo; done"
readSelectClause = called "select loop" $ do
    (T_Select id) <- g_Select
    spacing
    typ <- readRegular
    group <- readDoGroup id
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
prop_readCaseClause6 = isOk readCaseClause "case foo\n in if) :;; done) :;; esac"
readCaseClause = called "case expression" $ do
    start <- startSpan
    g_Case
    word <- readNormalWord
    allspacing
    g_In <|> fail "Expected 'in'"
    readLineBreak
    list <- readCaseList
    g_Esac <|> fail "Expected 'esac' to close the case statement"
    id <- endSpan start
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
prop_readFunctionDefinition10 = isOk readFunctionDefinition "function foo () { true; }"
prop_readFunctionDefinition11 = isWarning readFunctionDefinition "function foo{\ntrue\n}"
prop_readFunctionDefinition12 = isOk readFunctionDefinition "function []!() { true; }"
prop_readFunctionDefinition13 = isOk readFunctionDefinition "@require(){ true; }"
readFunctionDefinition = called "function" $ do
    start <- startSpan
    functionSignature <- try readFunctionSignature
    allspacing
    void (lookAhead $ oneOf "{(") <|> parseProblem ErrorC 1064 "Expected a { to open the function definition."
    group <- readBraceGroup <|> readSubshell
    id <- endSpan start
    return $ functionSignature id group
  where
    readFunctionSignature =
        readWithFunction <|> readWithoutFunction
      where
        readWithFunction = do
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
            return $ \id -> T_Function id (FunctionKeyword True) (FunctionParentheses hasParens) name

        readWithoutFunction = try $ do
            name <- many1 functionChars
            guard $ name /= "time"  -- Interferes with time ( foo )
            spacing
            readParens
            return $ \id -> T_Function id (FunctionKeyword False) (FunctionParentheses True) name

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
prop_readCoProc4 = isOk readCoProc "coproc a=b echo bar"
prop_readCoProc5 = isOk readCoProc "coproc 'foo' { echo bar; }"
prop_readCoProc6 = isOk readCoProc "coproc \"foo$$\" { echo bar; }"
prop_readCoProc7 = isOk readCoProc "coproc 'foo' ( echo bar )"
prop_readCoProc8 = isOk readCoProc "coproc \"foo$$\" while true; do true; done"
readCoProc = called "coproc" $ do
    start <- startSpan
    try $ do
        string "coproc"
        spacing1
    choice [ try $ readCompoundCoProc start, readSimpleCoProc start ]
  where
    readCompoundCoProc start = do
        notFollowedBy2 readAssignmentWord
        (var, body) <- choice [
            try $ do
                body <- readBody readCompoundCommand
                return (Nothing, body),
            try $ do
                var <- readNormalWord `thenSkip` spacing
                body <- readBody readCompoundCommand
                return (Just var, body)
            ]
        id <- endSpan start
        return $ T_CoProc id var body
    readSimpleCoProc start = do
        body <- readBody readSimpleCommand
        id <- endSpan start
        return $ T_CoProc id Nothing body
    readBody parser = do
        start <- startSpan
        body <- parser
        id <- endSpan start
        return $ T_CoProcBody id body

readPattern = (readPatternWord `thenSkip` spacing) `sepBy1` (char '|' `thenSkip` spacing)

prop_readConditionCommand = isOk readConditionCommand "[[ x ]] > foo 2>&1"
readConditionCommand = do
    cmd <- readCondition
    redirs <- many readIoRedirect
    id <- getNextIdSpanningTokenList (cmd:redirs)

    pos <- getPosition
    hasDashAo <- isFollowedBy $ do
        c <- choice $ try . string <$> ["-o", "-a", "or", "and"]
        posEnd <- getPosition
        parseProblemAtWithEnd pos posEnd ErrorC 1139 $
            "Use " ++ alt c ++ " instead of '" ++ c ++ "' between test commands."

    -- If the next word is a keyword, readNormalWord will trigger a warning
    hasKeyword <- isFollowedBy readKeyword
    hasWord <- isFollowedBy readNormalWord

    when (hasWord && not (hasKeyword || hasDashAo)) $ do
        -- We have other words following, and no error has been emitted.
        posEnd <- getPosition
        parseProblemAtWithEnd pos posEnd ErrorC 1140 "Unexpected parameters after condition. Missing &&/||, or bad expression?"

    return $ T_Redirecting id redirs cmd
  where
    alt "or" = "||"
    alt "-o" = "||"
    alt "and" = "&&"
    alt "-a" = "&&"
    alt _ = "|| or &&"

prop_readCompoundCommand = isOk readCompoundCommand "{ echo foo; }>/dev/null"
readCompoundCommand = do
    cmd <- choice [
        readBraceGroup,
        readAmbiguous "((" readArithmeticExpression readSubshell (\pos ->
            parseNoteAt pos ErrorC 1105 "Shells disambiguate (( differently or not at all. For subshell, add spaces around ( . For ((, fix parsing errors."),
        readSubshell,
        readWhileClause,
        readUntilClause,
        readIfClause,
        readForClause,
        readSelectClause,
        readCaseClause,
        readBatsTest,
        readFunctionDefinition
        ]
    redirs <- many readIoRedirect
    id <- getNextIdSpanningTokenList (cmd:redirs)
    optional . lookAhead $ do
        notFollowedBy2 $ choice [readKeyword, g_Lbrace]
        pos <- getPosition
        many1 readNormalWord
        posEnd <- getPosition
        parseProblemAtWithEnd pos posEnd ErrorC 1141 "Unexpected tokens after compound command. Bad redirection or missing ;/&&/||/|?"
    return $ T_Redirecting id redirs cmd


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
        subParse newPos (readArithmeticContents <* eof) unQuoted

    kludgeAwayQuotes :: String -> SourcePos -> (String, SourcePos)
    kludgeAwayQuotes s p =
        case s of
            first:second:rest ->
                let (last NE.:| backwards) = NE.reverse (second NE.:| rest)
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

-- Like readStringForParser, returning the span as a T_Literal
readLiteralForParser parser = do
    start <- startSpan
    str <- readStringForParser parser
    id <- endSpan start
    return $ T_Literal id str

prop_readAssignmentWord = isOk readAssignmentWord "a=42"
prop_readAssignmentWord2 = isOk readAssignmentWord "b=(1 2 3)"
prop_readAssignmentWord5 = isOk readAssignmentWord "b+=lol"
prop_readAssignmentWord7 = isOk readAssignmentWord "a[3$n'']=42"
prop_readAssignmentWord8 = isOk readAssignmentWord "a[4''$(cat foo)]=42"
prop_readAssignmentWord9 = isOk readAssignmentWord "IFS= "
prop_readAssignmentWord9a = isOk readAssignmentWord "foo="
prop_readAssignmentWord9b = isOk readAssignmentWord "foo=  "
prop_readAssignmentWord9c = isOk readAssignmentWord "foo=  #bar"
prop_readAssignmentWord11 = isOk readAssignmentWord "foo=([a]=b [c] [d]= [e f )"
prop_readAssignmentWord12 = isOk readAssignmentWord "a[b <<= 3 + c]='thing'"
prop_readAssignmentWord13 = isOk readAssignmentWord "var=( (1 2) (3 4) )"
prop_readAssignmentWord14 = isOk readAssignmentWord "var=( 1 [2]=(3 4) )"
prop_readAssignmentWord15 = isOk readAssignmentWord "var=(1 [2]=(3 4))"
readAssignmentWord = readAssignmentWordExt True
readWellFormedAssignment = readAssignmentWordExt False
readAssignmentWordExt lenient = called "variable assignment" $ do
    -- Parse up to and including the = in a 'try'
    (id, variable, op, indices) <- try $ do
        start <- startSpan
        pos <- getPosition
        -- Check for a leading $ at parse time, to warn for $foo=(bar) which
        -- would otherwise cause a parse failure so it can't be checked later.
        leadingDollarPos <-
            if lenient
            then optionMaybe $ getSpanPositionsFor (char '$')
            else return Nothing
        variable <- readVariableName
        indices <- many readArrayIndex
        hasLeftSpace <- fmap (not . null) spacing
        opStart <- getPosition
        id <- endSpan start
        op <- readAssignmentOp
        opEnd <- getPosition

        when (isJust leadingDollarPos || hasLeftSpace) $ do
            hasParen <- isFollowedBy (spacing >> char '(')
            when hasParen $
                sequence_ $ do
                    (l, r) <- leadingDollarPos
                    return $ parseProblemAtWithEnd l r ErrorC 1066 "Don't use $ on the left side of assignments."

            -- Fail so that this is not parsed as an assignment.
            fail ""
        -- At this point we know for sure.
        return (id, variable, op, indices)

    rightPosStart <- getPosition
    hasRightSpace <- fmap (not . null) spacing
    rightPosEnd <- getPosition
    isEndOfCommand <- fmap isJust $ optionMaybe (try . lookAhead $ (void (oneOf "\r\n;&|)") <|> eof))

    if hasRightSpace || isEndOfCommand
      then do
        when (variable /= "IFS" && hasRightSpace && not isEndOfCommand) $ do
            parseProblemAtWithEnd rightPosStart rightPosEnd WarningC 1007
                "Remove space after = if trying to assign a value (for empty string, use var='' ... )."
        value <- readEmptyLiteral
        return $ T_Assignment id op variable indices value
      else do
        optional $ do
            lookAhead $ char '='
            parseProblem ErrorC 1097 "Unexpected ==. For assignment, use =. For comparison, use [/[[. Or quote for literal string."

        value <- readArray <|> readNormalWord
        spacing
        return $ T_Assignment id op variable indices value
  where
    readAssignmentOp = do
        -- This is probably some kind of ascii art border
        unexpecting "===" (string "===")
        choice [
            string "+=" >> return Append,
            string "=" >> return Assign
            ]

readEmptyLiteral = do
    start <- startSpan
    id <- endSpan start
    return $ T_Literal id ""

readArrayIndex = do
    start <- startSpan
    char '['
    pos <- getPosition
    str <- readStringForParser readIndexSpan
    char ']'
    id <- endSpan start
    return $ T_UnparsedIndex id pos str

readArray :: Monad m => SCParser m Token
readArray = called "array assignment" $ do
    start <- startSpan
    opening <- getPosition
    char '('
    optional $ do
        lookAhead $ char '('
        parseProblemAt opening ErrorC 1116 "Missing $ on a $((..)) expression? (or use ( ( for arrays)."
    allspacing
    words <- readElement `reluctantlyTill` char ')'
    char ')' <|> fail "Expected ) to close array assignment"
    id <- endSpan start
    return $ T_Array id words
  where
    readElement = (readIndexed <|> readRegular) `thenSkip` allspacing
    readIndexed = do
        start <- startSpan
        index <- try $ do
            x <- many1 readArrayIndex
            char '='
            return x
        value <- readRegular <|> nothing
        id <- endSpan start
        return $ T_IndexedElement id index value
    readRegular = readArray <|> readNormalWord

    nothing = do
        start <- startSpan
        id <- endSpan start
        return $ T_Literal id ""

tryToken s t = try $ do
    start <- startSpan
    string s
    id <- endSpan start
    spacing
    return $ t id

redirToken c t = try $ do
    start <- startSpan
    char c
    id <- endSpan start
    notFollowedBy2 $ char '('
    return $ t id

tryWordToken s t = tryParseWordToken s t `thenSkip` spacing
tryParseWordToken keyword t = try $ do
    pos <- getPosition
    start <- startSpan
    str <- anycaseString keyword
    id <- endSpan start

    optional $ do
        c <- try . lookAhead $ anyChar
        let warning code = parseProblem ErrorC code $ "You need a space before the " ++ [c] ++ "."
        case c of
            '[' -> warning 1069
            '#' -> warning 1099
            '!' -> warning 1129
            ':' -> warning 1130
            _ -> return ()

    lookAhead keywordSeparator
    when (str /= keyword) $ do
        parseProblemAt pos ErrorC 1081 $
            "Scripts are case sensitive. Use '" ++ keyword ++ "', not '" ++ str ++ "' (or quote if literal)."
        fail ""
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
    start <- startSpan
    char '}'
    id <- endSpan start
    return $ T_Rbrace id

g_Lparen = tryToken "(" T_Lparen
g_Rparen = tryToken ")" T_Rparen
g_Bang = do
    start <- startSpan
    char '!'
    id <- endSpan start
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
prop_readShebang5 = isWarning readShebang "\n#!/bin/sh"
prop_readShebang6 = isWarning readShebang " # Copyright \n!#/bin/bash"
prop_readShebang7 = isNotOk readShebang "# Copyright \nfoo\n#!/bin/bash"
readShebang = do
    start <- startSpan
    anyShebang <|> try readMissingBang <|> withHeader
    many linewhitespace
    str <- many $ noneOf "\r\n"
    id <- endSpan start
    optional carriageReturn
    optional linefeed
    return $ T_Literal id str
  where
    anyShebang = choice $ map try [
        readCorrect,
        readSwapped,
        readTooManySpaces,
        readMissingHash
        ]
    readCorrect = void $ string "#!"

    readSwapped = do
        start <- startSpan
        string "!#"
        id <- endSpan start
        parseProblemAtId id ErrorC 1084
            "Use #!, not !#, for the shebang."

    skipSpaces = fmap (not . null) $ many linewhitespace
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

    withHeader = try $ do
        many1 headerLine
        pos <- getPosition
        anyShebang <*
            parseProblemAt pos ErrorC 1128 "The shebang must be on the first line. Delete blanks and move comments."

    headerLine = do
        notFollowedBy2 anyShebang
        many linewhitespace
        optional readAnyComment
        linefeed

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


readConfigFile :: Monad m => FilePath -> SCParser m [Annotation]
readConfigFile filename = do
    shouldIgnore <- Mr.asks ignoreRC
    if shouldIgnore then return [] else read' filename
  where
    read' filename = do
        sys <- Mr.asks systemInterface
        contents <- system $ siGetConfig sys filename
        case contents of
            Nothing -> return []
            Just (file, str) -> readConfig file str

    readConfig filename contents = do
        result <- lift $ runParserT readConfigKVs initialUserState filename contents
        case result of
            Right result ->
                return result

            Left err -> do
                parseProblem ErrorC 1134 $ errorFor filename err
                return []

    errorFor filename err =
        let line = "line " ++ (show . sourceLine $ errorPos err)
            suggestion = getStringFromParsec $ errorMessages err
        in
            "Failed to process " ++ (e4m filename) ++ ", " ++ line ++ ": "
                ++ suggestion

prop_readConfigKVs1 = isOk readConfigKVs "disable=1234"
prop_readConfigKVs2 = isOk readConfigKVs "# Comment\ndisable=1234 # Comment\n"
prop_readConfigKVs3 = isOk readConfigKVs ""
prop_readConfigKVs4 = isOk readConfigKVs "\n\n\n\n\t \n"
prop_readConfigKVs5 = isOk readConfigKVs "# shellcheck accepts annotation-like comments in rc files\ndisable=1234"
readConfigKVs = do
    anySpacingOrComment
    annotations <- many (readAnnotationWithoutPrefix False <* anySpacingOrComment)
    eof
    return $ concat annotations
anySpacingOrComment =
    many (void allspacingOrFail <|> void readAnyComment)

prop_readScript1 = isOk readScript "#!/bin/bash\necho hello world\n"
prop_readScript2 = isWarning readScript "#!/bin/bash\r\necho hello world\n"
prop_readScript3 = isWarning readScript "#!/bin/bash\necho hello\xA0world"
prop_readScript4 = isWarning readScript "#!/usr/bin/perl\nfoo=("
prop_readScript5 = isOk readScript "#!/bin/bash\n#This is an empty script\n\n"
prop_readScript6 = isOk readScript "#!/usr/bin/env -S X=FOO bash\n#This is an empty script\n\n"
prop_readScript7 = isOk readScript "#!/bin/zsh\n# shellcheck disable=SC1071\nfor f (a b); echo $f\n"
readScriptFile sourced = do
    start <- startSpan
    pos <- getPosition
    rcAnnotations <- if sourced
                     then return []
                     else do
                        filename <- Mr.asks currentFilename
                        readConfigFile filename

    -- Put the rc annotations on the stack so that one can ignore e.g. SC1084 in .shellcheckrc
    withAnnotations rcAnnotations $ do
        hasBom <- wasIncluded readUtf8Bom
        shebang <- readShebang <|> readEmptyLiteral
        let (T_Literal _ shebangString) = shebang
        allspacing
        annotationStart <- startSpan
        fileAnnotations <- readAnnotations

        -- Similarly put the filewide annotations on the stack to allow earlier suppression
        withAnnotations fileAnnotations $ do
            when (hasBom) $
                parseProblemAt pos ErrorC 1082
                    "This file has a UTF-8 BOM. Remove it with: LC_CTYPE=C sed '1s/^...//' < yourscript ."
            let annotations = fileAnnotations ++ rcAnnotations
            annotationId <- endSpan annotationStart
            let shellAnnotationSpecified =
                    any (\x -> case x of ShellOverride {} -> True; _ -> False) annotations
            shellFlagSpecified <- isJust <$> Mr.asks shellTypeOverride
            let ignoreShebang = shellAnnotationSpecified || shellFlagSpecified

            unless ignoreShebang $
                verifyShebang pos (executableFromShebang shebangString)
            if ignoreShebang || isValidShell (executableFromShebang shebangString) /= Just False
              then do
                    commands <- readCompoundListOrEmpty
                    id <- endSpan start
                    readPendingHereDocs
                    verifyEof
                    let script = T_Annotation annotationId annotations $
                                    T_Script id shebang commands
                    userstate <- getState
                    reparseIndices $ reattachHereDocs script (hereDocMap userstate)
                else do
                    many anyChar
                    id <- endSpan start
                    return $ T_Script id shebang []

  where
    verifyShebang pos s = do
        case isValidShell s of
            Just True -> return ()
            Just False -> parseProblemAt pos ErrorC 1071 "ShellCheck only supports sh/bash/dash/ksh/'busybox sh' scripts. Sorry!"
            Nothing -> parseProblemAt pos ErrorC 1008 "This shebang was unrecognized. ShellCheck only supports sh/bash/dash/ksh/'busybox sh'. Add a 'shell' directive to specify."

    isValidShell s =
        let good = null s || any (`isPrefixOf` s) goodShells
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
        "busybox sh",
        "bash",
        "bats",
        "ksh"
        ]
    badShells = [
        "awk",
        "csh",
        "expect",
        "fish",
        "perl",
        "python",
        "ruby",
        "tcsh",
        "zsh"
        ]

    readUtf8Bom = called "Byte Order Mark" $ string "\xFEFF"

readScript = readScriptFile False

-- Interactively run a specific parser in ghci:
-- debugParse readSimpleCommand "echo 'hello world'"
debugParse p string = runIdentity $ do
    (res, _) <- runParser testEnvironment p "-" string
    return res

-- Interactively run the complete parser in ghci:
-- debugParseScript "#!/bin/bash\necho 'Hello World'\n"
debugParseScript string =
    result {
        -- Remove the noisiest parts
        prTokenPositions = Map.fromList [
            (Id 0, (newPosition {
                posFile = "removed for clarity",
                posLine = -1,
                posColumn = -1
            }, newPosition {
                posFile = "removed for clarity",
                posLine = -1,
                posColumn = -1
            }))]
    }
  where
    result = runIdentity $
        parseScript (mockedSystemInterface []) $ newParseSpec {
            psFilename = "debug",
            psScript = string
        }

testEnvironment =
    Environment {
        systemInterface = (mockedSystemInterface []),
        checkSourced = False,
        currentFilename = "myscript",
        ignoreRC = False,
        shellTypeOverride = Nothing
    }


isOk p s =      parsesCleanly p s == Just True   -- The string parses with no warnings
isWarning p s = parsesCleanly p s == Just False  -- The string parses with warnings
isNotOk p s =   parsesCleanly p s == Nothing     -- The string does not parse

-- If the parser matches the string, return Right [ParseNotes+ParseProblems]
-- If it does not match the string,  return Left  [ParseProblems]
getParseOutput parser string = runIdentity $ do
    (res, sys) <- runParser testEnvironment
                    (parser >> eof >> getState) "-" string
    case (res, sys) of
        (Right userState, systemState) ->
            return $ Right $ parseNotes userState ++ parseProblems systemState
        (Left _, systemState) -> return $ Left $ parseProblems systemState

-- If the parser matches the string, return Just whether it was clean (without emitting suggestions)
-- Otherwise, Nothing
parsesCleanly parser string =
    case getParseOutput parser string of
        Right list -> Just $ null list
        Left _ -> Nothing

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
        headOrDefault "" (mapMaybe f $ reverse errors)  ++
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
            return newParseResult {
                prComments = map toPositionedComment $ nub $ parseNotes userstate ++ parseProblems state,
                prTokenPositions = Map.map startEndPosToPos (positionMap userstate),
                prRoot = Just script
            }
        Left err -> do
            let context = contextStack state
            return newParseResult {
                prComments =
                    map toPositionedComment $
                        (filter (not . isIgnored context) $
                            notesForContext context
                            ++ [makeErrorFor err])
                        ++ parseProblems state,
                prTokenPositions = Map.empty,
                prRoot = Nothing
            }
  where
    -- A final pass for ignoring parse errors after failed parsing
    isIgnored stack note = any (contextItemDisablesCode False (codeForParseNote note)) stack

notesForContext list = zipWith ($) [first, second] [(pos, str) | ContextName pos str <- list]
  where
    first (pos, str) = ParseNote pos pos ErrorC 1073 $
        "Couldn't parse this " ++ str ++ ". Fix to allow more checks."
    second (pos, str) = ParseNote pos pos InfoC 1009 $
        "The mentioned syntax error was in this " ++ str ++ "."

-- Go over all T_UnparsedIndex and reparse them as either arithmetic or text
-- depending on declare -A statements.
reparseIndices root = process root
  where
    process = analyze blank blank f
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
    f (TA_Variable id name indices) = do
        newIndices <- mapM (fixAssignmentIndex name) indices
        return $ TA_Variable id name newIndices
    f t = return t

    fixIndexElement name word =
        case word of
            T_IndexedElement id indices value -> do
                new <- mapM (fixAssignmentIndex name) indices
                return $ T_IndexedElement id new value
            _ -> return word

    fixAssignmentIndex name word =
        case word of
            T_UnparsedIndex id pos src -> do
                idx <- parsed name pos src
                process idx -- Recursively parse for cases like x[y[z=1]]=1
            _ -> return word

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
    newPositionedComment {
        pcStartPos = (posToPos start)
      , pcEndPos = (posToPos end)
      , pcComment = newComment {
          cSeverity = severity
        , cCode = code
        , cMessage = message
      }
    }

posToPos :: SourcePos -> Position
posToPos sp = newPosition {
    posFile = sourceName sp,
    posLine = fromIntegral $ sourceLine sp,
    posColumn = fromIntegral $ sourceColumn sp
}

startEndPosToPos :: (SourcePos, SourcePos) -> (Position, Position)
startEndPosToPos (s, e) = (posToPos s, posToPos e)

-- TODO: Clean up crusty old code that this is layered on top of
parseScript :: Monad m =>
        SystemInterface m -> ParseSpec -> m ParseResult
parseScript sys spec =
    parseShell env (psFilename spec) (psScript spec)
  where
    env = Environment {
        systemInterface = sys,
        checkSourced = psCheckSourced spec,
        currentFilename = psFilename spec,
        ignoreRC = psIgnoreRC spec,
        shellTypeOverride = psShellTypeOverride spec
    }

-- Same as 'try' but emit syntax errors if the parse fails.
tryWithErrors :: Monad m => SCParser m v -> SCParser m v
tryWithErrors parser = do
    userstate <- getState
    oldContext <- getCurrentContexts
    input <- getInput
    pos <- getPosition
    result <- lift $ runParserT (setPosition pos >> getResult parser) userstate (sourceName pos) input
    case result of
        Right (result, endPos, endInput, endState) -> do
            -- 'many' objects if we don't consume anything at all, so read a dummy value
            void anyChar <|> eof
            putState endState
            setPosition endPos
            setInput endInput
            return result

        Left err -> do
            newContext <- getCurrentContexts
            addParseProblem $ makeErrorFor err
            mapM_ addParseProblem $ notesForContext newContext
            setCurrentContexts oldContext
            fail ""
  where
    getResult p = do
        result <- p
        endPos <- getPosition
        endInput <- getInput
        endState <- getState
        return (result, endPos, endInput, endState)

return []
runTests = $quickCheckAll
