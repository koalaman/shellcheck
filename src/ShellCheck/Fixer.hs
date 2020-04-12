{-
    Copyright 2018-2019 Vidar Holen, Ng Zhi An

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
module ShellCheck.Fixer (applyFix, removeTabStops, mapPositions, Ranged(..), runTests) where

import ShellCheck.Interface
import Control.Monad.State
import Data.Array
import Data.List
import Data.Semigroup
import GHC.Exts (sortWith)
import Test.QuickCheck

-- The Ranged class is used for types that has a start and end position.
class Ranged a where
    start   :: a -> Position
    end     :: a -> Position
    overlap :: a -> a -> Bool
    overlap x y =
        (yStart >= xStart && yStart < xEnd) || (yStart < xStart && yEnd > xStart)
        where
            yStart = start y
            yEnd = end y
            xStart = start x
            xEnd = end x
    -- Set a new start and end position on a Ranged
    setRange :: (Position, Position) -> a -> a

-- Tests auto-verify that overlap commutes
assertOverlap x y = overlap x y && overlap y x
assertNoOverlap x y = not (overlap x y) && not (overlap y x)

prop_overlap_contiguous = assertNoOverlap
        (tFromStart 10 12 "foo" 1)
        (tFromStart 12 14 "bar" 2)

prop_overlap_adjacent_zerowidth = assertNoOverlap
        (tFromStart 3 3 "foo" 1)
        (tFromStart 3 3 "bar" 2)

prop_overlap_enclosed = assertOverlap
        (tFromStart 3 5 "foo" 1)
        (tFromStart 1 10 "bar" 2)

prop_overlap_partial = assertOverlap
        (tFromStart 1 5 "foo" 1)
        (tFromStart 3 7 "bar" 2)


instance Ranged PositionedComment where
    start = pcStartPos
    end = pcEndPos
    setRange (s, e) pc = pc {
        pcStartPos = s,
        pcEndPos = e
    }

instance Ranged Replacement where
    start = repStartPos
    end   = repEndPos
    setRange (s, e) r = r {
        repStartPos = s,
        repEndPos = e
    }

-- The Monoid instance for Fix merges fixes that do not conflict.
-- TODO: Make an efficient 'mconcat'
instance Monoid Fix where
    mempty = newFix
    mappend = (<>)

instance Semigroup Fix where
    f1 <> f2 =
        -- FIXME: This might need to also discard adjacent zero-width ranges for
        --        when two fixes change the same AST node, e.g. `foo` -> "$(foo)"
        if or [ r2 `overlap` r1 | r1 <- fixReplacements f1, r2 <- fixReplacements f2 ]
        then f1
        else newFix {
            fixReplacements = fixReplacements f1 ++ fixReplacements f2
            }

-- Conveniently apply a transformation to positions in a Fix
mapPositions :: (Position -> Position) -> Fix -> Fix
mapPositions f = adjustFix
  where
    adjustReplacement rep =
        rep {
            repStartPos = f $ repStartPos rep,
            repEndPos = f $ repEndPos rep
        }
    adjustFix fix =
        fix {
            fixReplacements = map adjustReplacement $ fixReplacements fix
        }

-- Rewrite a Ranged from a tabstop of 8 to 1
removeTabStops :: Ranged a => a -> Array Int String -> a
removeTabStops range ls =
    let startColumn = realignColumn lineNo colNo range
        endColumn = realignColumn endLineNo endColNo range
        startPosition = (start range) { posColumn = startColumn }
        endPosition = (end range) { posColumn = endColumn } in
    setRange (startPosition, endPosition) range
  where
    realignColumn lineNo colNo c =
      if lineNo c > 0 && lineNo c <= fromIntegral (length ls)
      then real (ls ! fromIntegral (lineNo c)) 0 0 (colNo c)
      else colNo c
    real _ r v target | target <= v = r
    -- hit this case at the end of line, and if we don't hit the target
    -- return real + (target - v)
    real [] r v target = r + (target - v)
    real ('\t':rest) r v target = real rest (r+1) (v + 8 - (v `mod` 8)) target
    real (_:rest) r v target = real rest (r+1) (v+1) target
    lineNo = posLine . start
    endLineNo = posLine . end
    colNo = posColumn . start
    endColNo = posColumn . end


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
multiToSingleLine :: [Fix] -> Array Int String -> ([Fix], String)
multiToSingleLine fixes lines =
    (map (mapPositions adjust) fixes, unlines $ elems lines)
  where
    -- A prefix sum tree from line number to column shift.
    -- FIXME: The tree will be totally unbalanced.
    shiftTree :: PSTree Int
    shiftTree =
        foldl (\t (n,s) -> addPSValue (n+1) (length s + 1) t) newPSTree $
            assocs lines
    singleString = unlines $ elems lines
    adjust pos =
        pos {
            posLine = 1,
            posColumn = (posColumn pos) +
                (fromIntegral $ getPrefixSum (fromIntegral $ posLine pos) shiftTree)
        }

-- Apply a fix and return resulting lines.
-- The number of lines can increase or decrease with no obvious mapping back, so
-- the function does not return an array.
applyFix :: Fix -> Array Int String -> [String]
applyFix fix fileLines =
    let
        untabbed = fix {
            fixReplacements =
                map (\c -> removeTabStops c fileLines) $
                    fixReplacements fix
            }
        (adjustedFixes, singleLine) = multiToSingleLine [untabbed] fileLines
    in
        lines . runFixer $ applyFixes2 adjustedFixes singleLine


-- start and end comes from pos, which is 1 based
prop_doReplace1 = doReplace 0 0 "1234" "A" == "A1234" -- technically not valid
prop_doReplace2 = doReplace 1 1 "1234" "A" == "A1234"
prop_doReplace3 = doReplace 1 2 "1234" "A" == "A234"
prop_doReplace4 = doReplace 3 3 "1234" "A" == "12A34"
prop_doReplace5 = doReplace 4 4 "1234" "A" == "123A4"
prop_doReplace6 = doReplace 5 5 "1234" "A" == "1234A"
doReplace start end o r =
    let si = fromIntegral (start-1)
        ei = fromIntegral (end-1)
        (x, xs) = splitAt si o
        z = drop (ei - si) xs
    in
    x ++ r ++ z

-- Fail if the 'expected' string is not result when applying 'fixes' to 'original'.
testFixes :: String -> String -> [Fix] -> Bool
testFixes expected original fixes =
    actual == expected
  where
    actual = runFixer (applyFixes2 fixes original)


-- A Fixer allows doing repeated modifications of a string where each
-- replacement automatically accounts for shifts from previous ones.
type Fixer a =  State (PSTree Int) a

-- Apply a single replacement using its indices into the original string.
-- It does not handle multiple lines, all line indices must be 1.
applyReplacement2 :: Replacement -> String -> Fixer String
applyReplacement2 rep string = do
    tree <- get
    let transform pos = pos + getPrefixSum pos tree
    let originalPos = (repStartPos rep, repEndPos rep)
        (oldStart, oldEnd) = tmap (fromInteger . posColumn) originalPos
        (newStart, newEnd) = tmap transform (oldStart, oldEnd)

    let (l1, l2) = tmap posLine originalPos in
        when (l1 /= 1 || l2 /= 1) $
            error "ShellCheck internal error, please report: bad cross-line fix"

    let replacer = repString rep
    let shift = (length replacer) - (oldEnd - oldStart)
    let insertionPoint =
          case repInsertionPoint rep of
              InsertBefore -> oldStart
              InsertAfter  -> oldEnd+1
    put $ addPSValue insertionPoint shift tree

    return $ doReplace newStart newEnd string replacer
  where
    tmap f (a,b) = (f a, f b)

-- Apply a list of Replacements in the correct order
applyReplacements2 :: [Replacement] -> String -> Fixer String
applyReplacements2 reps str =
    foldM (flip applyReplacement2) str $
        reverse $ sortWith repPrecedence reps

-- Apply all fixes with replacements in the correct order
applyFixes2 :: [Fix] -> String -> Fixer String
applyFixes2 fixes = applyReplacements2 (concatMap fixReplacements fixes)

-- Get the final value of a Fixer.
runFixer :: Fixer a -> a
runFixer f = evalState f newPSTree



-- A Prefix Sum Tree that lets you look up the sum of values at and below an index.
-- It's implemented essentially as a Fenwick tree without the bit-based balancing.
-- The last Num is the sum of the left branch plus current element.
data PSTree n = PSBranch n (PSTree n) (PSTree n) n | PSLeaf
    deriving (Show)

newPSTree :: Num n => PSTree n
newPSTree = PSLeaf

-- Get the sum of values whose keys are <= 'target'
getPrefixSum :: (Ord n, Num n) => n -> PSTree n -> n
getPrefixSum = f 0
  where
    f sum _ PSLeaf = sum
    f sum target (PSBranch pivot left right cumulative) =
        case target `compare` pivot of
            LT -> f sum target left
            GT -> f (sum+cumulative) target right
            EQ -> sum+cumulative

-- Add a value to the Prefix Sum tree at the given index.
-- Values accumulate: addPSValue 42 2 . addPSValue 42 3 == addPSValue 42 5
addPSValue :: (Ord n, Num n) => n -> n -> PSTree n -> PSTree n
addPSValue key value tree = if value == 0 then tree else f tree
  where
    f PSLeaf = PSBranch key PSLeaf PSLeaf value
    f (PSBranch pivot left right sum) =
        case key `compare` pivot of
            LT -> PSBranch pivot (f left) right (sum + value)
            GT -> PSBranch pivot left (f right) sum
            EQ -> PSBranch pivot left right (sum + value)

prop_pstreeSumsCorrectly kvs targets =
  let
    -- Trivial O(n * m) implementation
    dumbPrefixSums :: [(Int, Int)] -> [Int] -> [Int]
    dumbPrefixSums kvs targets =
        let prefixSum target = sum [v | (k,v) <- kvs, k <= target]
        in map prefixSum targets
    -- PSTree O(n * log m) implementation
    smartPrefixSums :: [(Int, Int)] -> [Int] -> [Int]
    smartPrefixSums kvs targets =
        let tree = foldl (\tree (pos, shift) -> addPSValue pos shift tree) PSLeaf kvs
        in map (\x -> getPrefixSum x tree) targets
  in smartPrefixSums kvs targets == dumbPrefixSums kvs targets


-- Semi-convenient functions for constructing tests.
testFix :: [Replacement] -> Fix
testFix list = newFix {
        fixReplacements = list
    }

tFromStart :: Int -> Int -> String -> Int -> Replacement
tFromStart start end repl order =
    newReplacement {
        repStartPos = newPosition {
            posLine = 1,
            posColumn = fromIntegral start
        },
        repEndPos = newPosition {
            posLine = 1,
            posColumn = fromIntegral end
        },
        repString = repl,
        repPrecedence = order,
        repInsertionPoint = InsertAfter
    }

tFromEnd start end repl order =
    (tFromStart start end repl order) {
        repInsertionPoint = InsertBefore
    }

prop_simpleFix1 = testFixes "hello world" "hell world" [
    testFix [
        tFromEnd 5 5 "o" 1
    ]]

prop_anchorsLeft = testFixes "-->foobar<--" "--><--" [
    testFix [
        tFromStart 4 4 "foo" 1,
        tFromStart 4 4 "bar" 2
    ]]

prop_anchorsRight = testFixes "-->foobar<--" "--><--" [
    testFix [
        tFromEnd 4 4 "bar" 1,
        tFromEnd 4 4 "foo" 2
    ]]

prop_anchorsBoth1 = testFixes "-->foobar<--" "--><--" [
    testFix [
        tFromStart 4 4 "bar" 2,
        tFromEnd 4 4 "foo" 1
    ]]

prop_anchorsBoth2 = testFixes "-->foobar<--" "--><--" [
    testFix [
        tFromEnd 4 4 "foo" 2,
        tFromStart 4 4 "bar" 1
    ]]

prop_composeFixes1 = testFixes "cd \"$1\" || exit" "cd $1" [
    testFix [
        tFromStart 4 4 "\"" 10,
        tFromEnd   6 6 "\"" 10
    ],
    testFix [
        tFromEnd 6 6 " || exit" 5
    ]]

prop_composeFixes2 = testFixes "$(\"$1\")" "`$1`" [
    testFix [
        tFromStart 1 2 "$(" 5,
        tFromEnd   4 5 ")" 5
    ],
    testFix [
        tFromStart 2 2 "\"" 10,
        tFromEnd 4 4 "\"" 10
    ]]

prop_composeFixes3 = testFixes "(x)[x]" "xx" [
    testFix [
        tFromStart 1 1 "(" 4,
        tFromEnd   2 2 ")" 3,
        tFromStart 2 2 "[" 2,
        tFromEnd   3 3 "]" 1
    ]]

prop_composeFixes4 = testFixes "(x)[x]" "xx" [
    testFix [
        tFromStart 1 1 "(" 4,
        tFromStart 2 2 "[" 3,
        tFromEnd   2 2 ")" 2,
        tFromEnd   3 3 "]" 1
    ]]

prop_composeFixes5 = testFixes "\"$(x)\"" "`x`" [
    testFix [
        tFromStart 1 2 "$(" 2,
        tFromEnd   3 4 ")"  2,
        tFromStart 1 1 "\"" 1,
        tFromEnd   4 4 "\"" 1
    ]]


return []
runTests = $quickCheckAll
