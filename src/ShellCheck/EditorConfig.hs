{-
    Copyright 2012-2024 Vidar Holen

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
-- Minimal support for reading shellcheck directives from EditorConfig
-- style files (https://editorconfig.org/). Only the `shellcheck.*` keys
-- of sections whose glob matches the file being checked are extracted,
-- and turned into the same "key=value" directive syntax that is used in
-- .shellcheckrc files.
module ShellCheck.EditorConfig (parseEditorConfig, isEditorConfigRoot, invalidRootLines, globToRegexString, runTests) where

import Data.Char
import Data.List
import Data.Maybe

import ShellCheck.Data (shellForExecutable)
import ShellCheck.Regex

import Test.QuickCheck

-- Given the contents of an EditorConfig style file and the name of the
-- file being checked, return the shellcheck directives (as a
-- "key=value\n" delimited blob, suitable for feeding into the same
-- parser as .shellcheckrc) found in matching sections.
--
-- As per the EditorConfig spec, files are read top to bottom and
-- properties from later sections override those from earlier ones
-- (for the same key), so on conflicts the last matching section wins.
parseEditorConfig :: String -> FilePath -> String
parseEditorConfig contents name =
    renderDirectives . lastWins . concatMap sectionDirectives $ sections
  where
    -- Render each directive at its original line in the config file, so
    -- that any parse error (SC1134) is reported at the correct line.
    renderDirectives = snd . foldl step (0, "")
    step (prevLine, out) (line, key, value) =
        (line, out ++ replicate (line - prevLine - 1) '\n' ++ key ++ "=" ++ value ++ "\n")

    -- Keep only the last occurrence of each key, preserving the
    -- relative order of the remaining (first-seen) entries.
    lastWins = reverse . nubBy (\a b -> keyOf a == keyOf b) . reverse
      where
        keyOf (_, key, _) = key

    ls = lines contents
    sections = splitSections 1 ls

    splitSections _ [] = []
    splitSections n (l:rest) =
        case parseHeader l of
            Just pat ->
                let (body, rest') = break (isJust . parseHeader) rest
                in (pat, zip [n+1..] body) : splitSections (n + 1 + length body) rest'
            Nothing -> splitSections (n+1) rest

    parseHeader l =
        let t = trim (dropLineComment l)
        in case t of
            ('[':cs@(_:_)) | last cs == ']' -> Just (init cs)
            _ -> Nothing

    sectionDirectives (pat, body) =
        if matchesGlob pat name
        then mapMaybe toDirective body
        else []

    toDirective (line, l) =
        let t = dropLineComment l
        in case break (== '=') (trim t) of
            (key, '=':value) ->
                let key' = trim key
                    value' = trim value
                in if "shellcheck." `isPrefixOf` key'
                    then
                        let directive = drop (length "shellcheck.") key'
                        in if isUsableDirective directive value'
                            then Just (line, directive, value')
                            else Nothing
                    else Nothing
            _ -> Nothing

    -- Only emit directives that the .shellcheckrc parser can handle.
    -- Unknown shells (e.g. 'shellcheck.shell=zsh') would otherwise
    -- silently suppress the SC2148 "unknown shell" warning, so they are
    -- dropped here. Empty values are kept: the rc parser rejects them,
    -- producing an SC1134 error at the right file and line.
    isUsableDirective "shell" value = null value || isJust (shellForExecutable value)
    isUsableDirective _ value = not (null value)

-- Does the top-level (pre-section) part of an EditorConfig file
-- declare "root = true"? Per the spec, this stops the search for
-- further EditorConfig files in parent directories.
isEditorConfigRoot :: String -> Bool
isEditorConfigRoot contents =
    any (== Just "true") . map rootValue $ preSectionLines contents
  where
    isSectionHeader l =
        case trim (dropLineComment l) of
            ('[':cs@(_:_)) -> last cs == ']'
            _ -> False
    preSectionLines = takeWhile (not . isSectionHeader) . lines

    rootValue l =
        case break (== '=') (trim (dropLineComment l)) of
            (key, '=':value) | map toLower (trim key) == "root" ->
                Just (map toLower (trim value))
            _ -> Nothing

-- Returns the 1-based line numbers of invalid 'root' declarations,
-- i.e. root values other than true/false (such as 'root =').
invalidRootLines :: String -> [Int]
invalidRootLines contents =
    [ n | (n, l) <- zip [1..] (preSectionLines contents)
        , case rootValue l of
            Just value -> value `notElem` ["true", "false"]
            Nothing -> False ]
  where
    isSectionHeader l =
        case trim (dropLineComment l) of
            ('[':cs@(_:_)) -> last cs == ']'
            _ -> False
    preSectionLines = takeWhile (not . isSectionHeader) . lines

    rootValue l =
        case break (== '=') (trim (dropLineComment l)) of
            (key, '=':value) | map toLower (trim key) == "root" ->
                Just (map toLower (trim value))
            _ -> Nothing

-- EditorConfig does not allow inline comments; '#' and ';' starting
-- on the first non-whitespace character denote a full-line comment.
-- Lines not starting with '#' or ';' must be returned verbatim.
dropLineComment l =
    case dropWhile isSpace l of
        ('#':_) -> ""
        (';':_) -> ""
        _       -> l

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- Does the (relative path of the) file match the given EditorConfig glob?
matchesGlob :: String -> FilePath -> Bool
matchesGlob pattern name =
    name `matches` mkRegex (globToRegexString pattern)

-- Translate an EditorConfig glob pattern into an anchored regex string.
-- Per the spec, patterns without a path separator are matched against
-- the file at any depth (as if prefixed with "**/").
globToRegexString :: String -> String
globToRegexString pattern = "^" ++ prefix ++ go pattern ++ "$"
  where
    prefix = if '/' `elem` pattern then "" else "(.*/)?"

    go [] = ""
    go ('*':'*':rest) = ".*" ++ go rest
    go ('*':rest) = "[^/]*" ++ go rest
    go ('?':rest) = "[^/]" ++ go rest
    go ('[':rest) =
        let (cls, rest') = break (== ']') rest
        in case rest' of
            (']':rest'') -> "[" ++ translateClass cls ++ "]" ++ go rest''
            _ -> "\\[" ++ go rest
    go ('{':rest) =
        case findMatchingBrace rest of
            Just (body, rest'') ->
                buildAlternation (map go (braceAlternatives body)) ++ go rest''
            _ -> "\\{" ++ go rest
    go (c:rest)
        | c `elem` regexSpecials = ['\\', c] ++ go rest
        | otherwise = c : go rest

    regexSpecials = ".\\+()^$|"

    translateClass ('!':cs) = '^' : escapeClass cs
    translateClass cs = escapeClass cs
    escapeClass = concatMap (\c -> if c == '\\' then "\\\\" else [c])

    -- Scan past a balanced '{' ... '}' pair, returning the contents of
    -- the braces (with any nested braces verbatim) and the remainder.
    -- Returns Nothing if the braces are unbalanced.
    findMatchingBrace = goBrace 1 ""
      where
        goBrace _ _ "" = Nothing
        goBrace d acc (c:cs)
            | c == '}' =
                if d == 1 then Just (reverse acc, cs)
                else goBrace (d - 1) (c:acc) cs
            | c == '{' = goBrace (d + 1) (c:acc) cs
            | otherwise = goBrace d (c:acc) cs

    -- Wrap a list of regex alternatives in an anchored group. Empty
    -- alternatives are handled so the result is always a valid regex:
    --   * all empty        -> "" (the whole group matches the empty string)
    --   * some empty       -> "(a|b)?" (the group is optional)
    --   * none empty       -> "(a|b|c)"
    buildAlternation alts
        | all null alts = ""
        | any null alts = "(" ++ intercalate "|" (filter (not . null) alts) ++ ")?"
        | otherwise = "(" ++ intercalate "|" alts ++ ")"

    -- Expand EditorConfig brace alternatives. Besides the comma
    -- separated form, EditorConfig supports numeric ranges like
    -- '{1..3}' which match any integer in the range. Commas inside
    -- nested braces (e.g. 'ba{r,z}') are not treated as separators.
    braceAlternatives body =
        case break (== '.') body of
            (fromStr, '.':'.':toStr)
                | Just from <- readInt fromStr
                , Just to <- readInt toStr ->
                    map show $
                        if from < to
                        then [from..to]
                        else []
            _ -> splitTopLevelCommas body

    -- Split on commas, but ignore commas that appear inside nested
    -- '{...}' pairs so that 'a,b{c,d}' yields ["a", "b{c,d}"].
    splitTopLevelCommas = go 0 ""
      where
        go _ acc "" = [reverse acc]
        go d acc (c:cs)
            | c == '{' = go (d + 1) (c:acc) cs
            | c == '}' = go (max 0 (d - 1)) (c:acc) cs
            | c == ',' && d == 0 = reverse acc : go 0 "" cs
            | otherwise = go d (c:acc) cs

    readInt s =
        case reads s :: [(Int, String)] of
            [(n, "")] -> Just n
            _ -> Nothing

prop_globStar = matchesGlob "*.ebuild" "foo.ebuild"
prop_globBraceExt = matchesGlob "*.{ebuild,eclass}" "foo.eclass"
prop_globBraceExt2 = matchesGlob "*.{ebuild,eclass}" "foo.ebuild"
prop_globBraceName = matchesGlob "{PKGBUILD,APKBUILD}" "PKGBUILD"
prop_globBraceName2 = matchesGlob "{PKGBUILD,APKBUILD}" "APKBUILD"
prop_globNoMatch = not $ matchesGlob "*.ebuild" "foo.txt"
prop_globQuestion = matchesGlob "foo?.sh" "food.sh"
prop_globClass = matchesGlob "foo[0-9].sh" "foo1.sh"
prop_globClassNeg = not $ matchesGlob "foo[!0-9].sh" "foo1.sh"
-- Patterns without a path separator should match at any depth.
prop_globAnyDepth = matchesGlob "*.sh" "sub/dir/foo.sh"
prop_globAnyDepthPlain = matchesGlob "foo" "sub/foo"
-- Patterns with a path separator are only matched against the full
-- relative path.
prop_globWithSlashNoMatch = not $ matchesGlob "sub/*.sh" "other/foo.sh"
prop_globWithSlashMatch = matchesGlob "sub/*.sh" "sub/foo.sh"
-- Numeric range expansion
prop_globRange = matchesGlob "file{1..3}.sh" "file2.sh"
prop_globRangeStart = matchesGlob "file{1..3}.sh" "file1.sh"
prop_globRangeEnd = matchesGlob "file{1..3}.sh" "file3.sh"
prop_globRangeNoMatch = not $ matchesGlob "file{1..3}.sh" "file4.sh"
prop_globRangeNegative = matchesGlob "file{-2..0}.sh" "file-1.sh"
prop_globRangeDescending = not $ matchesGlob "file{3..1}.sh" "file2.sh"
prop_globLiteralDots = not $ matchesGlob "file{1..3}.sh" "file1..3.sh"
-- Empty brace alternatives (e.g. 'foo{,bar}') make the group optional:
-- 'foo' and 'foobar' both match.
prop_globBraceEmptyAlt = matchesGlob "foo{,bar}" "foo"
prop_globBraceEmptyAlt2 = matchesGlob "foo{,bar}" "foobar"
prop_globBraceEmptyAltNoMatch = not $ matchesGlob "foo{,bar}" "foobaz"
-- Nested braces: '{foo,ba{r,z}}' matches foo, bar and baz.
prop_globBraceNested1 = matchesGlob "{foo,ba{r,z}}" "foo"
prop_globBraceNested2 = matchesGlob "{foo,ba{r,z}}" "bar"
prop_globBraceNested3 = matchesGlob "{foo,ba{r,z}}" "baz"
prop_globBraceNestedNoMatch = not $ matchesGlob "{foo,ba{r,z}}" "baq"

prop_parseEditorConfig1 =
    parseEditorConfig "[*.{ebuild,eclass}]\nshellcheck.shell=bash\nshellcheck.disable=SC2034\n" "foo.ebuild"
        == "\nshell=bash\ndisable=SC2034\n"
prop_parseEditorConfig2 =
    parseEditorConfig "[*.{ebuild,eclass}]\nshellcheck.shell=bash\n" "foo.txt" == ""
prop_parseEditorConfig3 =
    parseEditorConfig "[{PKGBUILD,APKBUILD}]\nshellcheck.disable=SC2034\n" "PKGBUILD" == "\ndisable=SC2034\n"
prop_parseEditorConfig4 =
    parseEditorConfig "root = true\n[*.sh]\nindent_style = space\nshellcheck.shell=bash\n" "foo.sh"
        == "\n\n\nshell=bash\n"
-- A later, more specific section overrides an earlier, more general
-- one for the same key.
prop_parseEditorConfig5 =
    parseEditorConfig "[*]\nshellcheck.shell=sh\n\n[foo]\nshellcheck.shell=bash\n" "foo"
        == "\n\n\n\nshell=bash\n"
-- Non-conflicting keys from earlier and later sections are all kept.
prop_parseEditorConfig6 =
    parseEditorConfig "[*]\nshellcheck.shell=sh\n\n[foo]\nshellcheck.disable=SC2034\n" "foo"
        == "\nshell=sh\n\n\ndisable=SC2034\n"
-- An unsupported shell must not suppress the SC2148 warning, so the
-- directive is not emitted.
prop_parseEditorConfigUnknownShell =
    parseEditorConfig "[*]\nshellcheck.shell=zsh\n" "foo" == ""
prop_parseEditorConfigEmptyShell =
    parseEditorConfig "[*]\nshellcheck.shell=\n" "foo" == "\nshell=\n"
prop_parseEditorConfigEmptyDisable =
    parseEditorConfig "[*]\nshellcheck.disable=\n" "foo" == ""
-- EditorConfig does not allow inline comments; the whole line after
-- the value is kept (including '# ...'), making the shell value
-- invalid, so no usable directive is emitted.
prop_parseEditorConfigInlineComment =
    parseEditorConfig "[*]\nshellcheck.shell=bash # inline\n" "foo" == ""
-- Full-line comments starting on first non-ws char are stripped.
prop_parseEditorConfigLineComment =
    parseEditorConfig "[*]\n# shellcheck.shell=bash\n" "foo" == ""
prop_parseEditorConfigSemicolonComment =
    parseEditorConfig "[*]\n; shellcheck.shell=bash\n" "foo" == ""
-- Empty brace alternative makes the glob group optional; 'foo' matches
-- '[foo{,bar}]'.
prop_parseEditorConfigBraceEmpty =
    parseEditorConfig "[foo{,bar}]\nshellcheck.shell=sh\n" "foo" == "\nshell=sh\n"
-- Nested braces are expanded correctly; 'baz' matches
-- '[{foo,ba{r,z}}]'.
prop_parseEditorConfigBraceNested =
    parseEditorConfig "[{foo,ba{r,z}}]\nshellcheck.shell=sh\n" "baz" == "\nshell=sh\n"
prop_isEditorConfigRootEmpty = not $ isEditorConfigRoot "root =\n"
prop_isEditorConfigRootFalse = not $ isEditorConfigRoot "root = false\n"
prop_isEditorConfigRootTrue = isEditorConfigRoot "root = TRUE\n"
prop_invalidRootLinesEmpty = invalidRootLines "root =\n" == [1]
prop_invalidRootLinesTrue = invalidRootLines "root = true\n" == []
prop_invalidRootLinesFalse = invalidRootLines "root = false\n" == []
prop_invalidRootLinesInSection =
    invalidRootLines "[*]\nroot = true\n" == []

return []
runTests = $quickCheckAll
