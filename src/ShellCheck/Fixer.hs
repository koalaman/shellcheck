module ShellCheck.Fixer (applyFix , replaceMultiLines, Ranged(..)) where

import ShellCheck.Interface
import Data.List
import Data.Semigroup

-- The Ranged class is used for types that has a start and end position.
class Ranged a where
    start   :: a -> Position
    end     :: a -> Position
    overlap :: a -> a -> Bool
    overlap x y =
        (yStart >= xStart && yStart <= xEnd) || (yStart < xStart && yEnd > xStart)
        where
            yStart = start y
            yEnd = end y
            xStart = start x
            xEnd = end x

instance Ranged Replacement where
    start = repStartPos
    end   = repEndPos

instance Ranged a => Ranged [a] where
    start [] = newPosition
    start xs = (minimum . map start) xs
    end []   = newPosition
    end xs   = (maximum . map end) xs

instance Ranged Fix where
    start = start . fixReplacements
    end   = end . fixReplacements

-- The Monoid instance for Fix merges replacements that do not overlap.
instance Monoid Fix where
    mempty = newFix
    mappend = (<>)

instance Semigroup Fix where
    f1 <> f2 = if overlap f1 f2 then f1 else newFix {
            fixReplacements = fixReplacements f1 ++ fixReplacements f2
    }

applyFix fix fileLines =
    -- apply replacements in sorted order by end position
    let sorted = (removeOverlap . reverse . sort) (fixReplacements fix) in
    applyReplacement sorted fileLines
    where
        applyReplacement [] s = s
        applyReplacement (rep:xs) s = applyReplacement xs $ replaceMultiLines rep s
        -- prereq: list is already sorted by start position
        removeOverlap [] = []
        removeOverlap (x:xs) = checkoverlap x xs
        checkoverlap x [] = x:[]
        checkoverlap x (y:ys) =
            if overlap x y then x:(removeOverlap ys) else x:y:(removeOverlap ys)


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
replaceMultiLines rep fileLines = -- this can replace doReplace
    let startRow = fromIntegral $ (posLine . repStartPos) rep
        endRow =  fromIntegral $ (posLine . repEndPos) rep
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
