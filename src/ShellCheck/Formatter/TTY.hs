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
module ShellCheck.Formatter.TTY (format) where

import ShellCheck.Interface
import ShellCheck.Formatter.Format

import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import GHC.Exts
import System.IO
import System.Info

wikiLink = "https://www.shellcheck.net/wiki/"

-- An arbitrary Ord thing to order warnings
type Ranking = (Char, Severity, Integer)

format :: FormatterOptions -> IO Formatter
format options = do
    topErrorRef <- newIORef []
    return Formatter {
        header = return (),
        footer = outputWiki topErrorRef,
        onFailure = outputError options,
        onResult = outputResult options topErrorRef
    }

colorForLevel level =
    case level of
        "error"   -> 31 -- red
        "warning" -> 33 -- yellow
        "info"    -> 32 -- green
        "style"   -> 32 -- green
        "message" -> 1 -- bold
        "source"  -> 0 -- none
        _ -> 0         -- none

rankError :: PositionedComment -> Ranking
rankError err = (ranking, cSeverity $ pcComment err, cCode $ pcComment err)
  where
    ranking =
        if cCode (pcComment err) `elem` uninteresting
        then 'Z'
        else 'A'

    -- A list of the most generic, least directly helpful
    -- error codes to downrank.
    uninteresting = [
        1009, -- Mentioned parser error was..
        1019, -- Expected this to be an argument
        1036, -- ( is invalid here
        1047, -- Expected 'fi'
        1062, -- Expected 'done'
        1070, -- Parsing stopped here (generic)
        1072, -- Missing/unexpected ..
        1073, -- Couldn't parse this ..
        1088, -- Parsing stopped here (paren)
        1089  -- Parsing stopped here (keyword)
        ]

appendComments errRef comments max = do
    previous <- readIORef errRef
    let current = map (\x -> (rankError x, cCode $ pcComment x, cMessage $ pcComment x)) comments
    writeIORef errRef . take max . nubBy equal . sort $ previous ++ current
  where
    fst3 (x,_,_) = x
    equal x y = fst3 x == fst3 y

outputWiki :: IORef [(Ranking, Integer, String)] -> IO ()
outputWiki errRef = do
    issues <- readIORef errRef
    unless (null issues) $ do
        putStrLn "For more information:"
        mapM_ showErr issues
  where
    showErr (_, code, msg) =
        putStrLn $ "  " ++ wikiLink ++ "SC" ++ show code ++ " -- " ++ shorten msg
    limit = 36
    shorten msg =
        if length msg < limit
        then msg
        else (take (limit-3) msg) ++ "..."

outputError options file error = do
    color <- getColorFunc $ foColorOption options
    hPutStrLn stderr $ color "error" $ file ++ ": " ++ error

outputResult options ref result sys = do
    color <- getColorFunc $ foColorOption options
    let comments = crComments result
    appendComments ref comments (fromIntegral $ foWikiLinkCount options)
    let fileGroups = groupWith sourceFile comments
    mapM_ (outputForFile color sys) fileGroups

outputForFile color sys comments = do
    let fileName = sourceFile (head comments)
    result <- (siReadFile sys) fileName
    let contents = either (const "") id result
    let fileLines = lines contents
    let lineCount = fromIntegral $ length fileLines
    let groups = groupWith lineNo comments
    mapM_ (\commentsForLine -> do
        let lineNum = lineNo (head commentsForLine)
        let line = if lineNum < 1 || lineNum > lineCount
                        then ""
                        else fileLines !! fromIntegral (lineNum - 1)
        putStrLn ""
        putStrLn $ color "message" $
           "In " ++ fileName ++" line " ++ show lineNum ++ ":"
        putStrLn (color "source" line)
        mapM_ (\c -> putStrLn (color (severityText c) $ cuteIndent c)) commentsForLine
        putStrLn ""
        showFixedString color comments lineNum line
      ) groups

hasApplicableFix lineNum comment = fromMaybe False $ do
    replacements <- fixReplacements <$> pcFix comment
    guard $ all (\c -> onSameLine (repStartPos c) && onSameLine (repEndPos c)) replacements
    return True
  where
    onSameLine pos = posLine pos == lineNum

-- FIXME: Work correctly with multiple replacements
showFixedString color comments lineNum line =
    case filter (hasApplicableFix lineNum) comments of
        (first:_) -> do
            -- in the spirit of error prone
            putStrLn $ color "message" "Did you mean: "
            putStrLn $ fixedString first line
            putStrLn ""
        _ -> return ()

-- need to do something smart about sorting by end index
fixedString :: PositionedComment -> String -> String
fixedString comment line =
    case (pcFix comment) of
    Nothing -> ""
    Just rs ->
        applyReplacement (fixReplacements rs) line 0
        where
            applyReplacement [] s _ = s
            applyReplacement (rep:xs) s offset =
                let replacementString = repString rep
                    start = (posColumn . repStartPos) rep
                    end = (posColumn . repEndPos) rep
                    z = doReplace start end s replacementString
                    len_r = (fromIntegral . length) replacementString in
                applyReplacement xs z (offset + (end - start) + len_r)

-- FIXME: Work correctly with tabs
-- start and end comes from pos, which is 1 based
-- doReplace 0 0 "1234" "A" -> "A1234" -- technically not valid
-- doReplace 1 1 "1234" "A" -> "A1234"
-- doReplace 1 2 "1234" "A" -> "A234"
-- doReplace 3 3 "1234" "A" -> "12A34"
-- doReplace 4 4 "1234" "A" -> "123A4"
-- doReplace 5 5 "1234" "A" -> "1234A"
doReplace start end o r =
    let si = fromIntegral (start-1)
        ei = fromIntegral (end-1)
        (x, xs) = splitAt si o
        (y, z) = splitAt (ei - si) xs
    in
    x ++ r ++ z

-- A replacement that spans multiple line is applied by:
-- 1. merging the affected lines into a single string using `unlines`
-- 2. apply the replacement as if it only spanned a single line
-- The tricky part is adjusting the end column of the replacement
-- (the end line doesn't matter because there is only one line)
--
--   aaS  <--- start of replacement (row 1 column 3)
--   bbbb
--   cEc
--    \------- end of replacement (row 3 column 2)
--
-- a flattened string will look like:
--
--   "aaS\nbbbb\ncEc\n"
--
-- The column of E has to be adjusted by:
-- 1. lengths of lines to be replaced, except the end row itself
-- 2. end column of the replacement
-- 3. number of '\n' by `unlines`
-- Returns the original lines from the file with the replacement applied.
-- Multiline replacements completely overwrite new lines in the original string.
-- e.g. if the replacement spans 2 lines, but the replacement string does not
-- have a '\n', then the number of replaced lines will be 1 shorter.
replaceMultiLines fileLines rep =
    let startRow = fromIntegral $ (posLine . repStartPos) rep
        endRow = fromIntegral $ (posLine . repEndPos) rep
        (ys, zs) = splitAt endRow fileLines
        (xs, toReplaceLines) = splitAt (startRow-1) ys
        lengths = fromIntegral $ sum (map length (init toReplaceLines))
        newlines = fromIntegral $ (length toReplaceLines - 1) -- for the '\n' from unlines
        original = unlines toReplaceLines
        startCol = ((posColumn . repStartPos) rep)
        endCol = ((posColumn . repEndPos) rep + newlines + lengths)
        replacedLines = (lines $ doReplace startCol endCol original (repString rep))
    in
    xs ++ replacedLines ++ zs

cuteIndent :: PositionedComment -> String
cuteIndent comment =
    replicate (fromIntegral $ colNo comment - 1) ' ' ++
        makeArrow ++ " " ++ code (codeNo comment) ++ ": " ++ messageText comment
  where
    arrow n = '^' : replicate (fromIntegral $ n-2) '-' ++ "^"
    makeArrow =
        let sameLine = lineNo comment == endLineNo comment
            delta = endColNo comment - colNo comment
        in
            if sameLine && delta > 2 && delta < 32 then arrow delta else "^--"

code num = "SC" ++ show num

getColorFunc colorOption = do
    term <- hIsTerminalDevice stdout
    let windows = "mingw" `isPrefixOf` os
    let isUsableTty = term && not windows
    let useColor = case colorOption of
                       ColorAlways -> True
                       ColorNever -> False
                       ColorAuto -> isUsableTty
    return $ if useColor then colorComment else const id
  where
    colorComment level comment =
        ansi (colorForLevel level) ++ comment ++ clear
    clear = ansi 0
    ansi n = "\x1B[" ++ show n ++ "m"
