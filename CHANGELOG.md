## Git

## v0.7.1 - 2020-04-04
### Fixed
- `-f diff` no longer claims that it found more issues when it didn't
- Known empty variables now correctly trigger SC2086
- ShellCheck should now be compatible with Cabal 3
- SC2154 and all command-specific checks now trigger for builtins
  called with `builtin`

### Added
- SC1136: Warn about unexpected characters after ]/]]
- SC2254: Suggest quoting expansions in case statements
- SC2255: Suggest using `$((..))` in `[ 2*3 -eq 6 ]`
- SC2256: Warn about translated strings that are known variables
- SC2257: Warn about arithmetic mutation in redirections
- SC2258: Warn about trailing commas in for loop elements

### Changed
- SC2230: 'command -v' suggestion is now off by default (-i deprecate-which)
- SC1081: Keywords are now correctly parsed case sensitively, with a warning


## v0.7.0 - 2019-07-28
### Added
- Precompiled binaries for macOS and Linux aarch64
- Preliminary support for fix suggestions
- New `-f diff` unified diff format for auto-fixes
- Files containing Bats tests can now be checked
- Directory wide directives can now be placed in a `.shellcheckrc`
- Optional checks: Use `--list-optional` to show a list of tests,
                   Enable with `-o` flags or `enable=name` directives
- Source paths: Use `-P dir1:dir2` or a `source-path=dir1` directive
                to specify search paths for sourced files.
- json1 format like --format=json but treats tabs as single characters
- Recognize FLAGS variables created by the shflags library.
- Site-specific changes can now be made in Custom.hs for ease of patching
- SC2154: Also warn about unassigned uppercase variables (optional)
- SC2252: Warn about `[ $a != x ] || [ $a != y ]`, similar to SC2055
- SC2251: Inform about ineffectual ! in front of commands
- SC2250: Warn about variable references without braces (optional)
- SC2249: Warn about `case` with missing default case (optional)
- SC2248: Warn about unquoted variables without special chars (optional)
- SC2247: Warn about $"(cmd)" and $"{var}"
- SC2246: Warn if a shebang's interpreter ends with /
- SC2245: Warn that Ksh ignores all but the first glob result in `[`
- SC2243/SC2244: Suggest using explicit -n for `[ $foo ]` (optional)
- SC1135: Suggest not ending double quotes just to make $ literal

### Changed
- If a directive or shebang is not specified, a `.bash/.bats/.dash/.ksh`
  extension will be used to infer the shell type when present.
- Disabling SC2120 on a function now disables SC2119 on call sites

### Fixed
- SC2183 no longer warns about missing printf args for `%()T`

## v0.6.0 - 2018-12-02
### Added
- Command line option --severity/-S for filtering by minimum severity
- Command line option --wiki-link-count/-W for showing wiki links
- SC2152/SC2151: Warn about bad `exit` values like `1234` and `"foo"`
- SC2236/SC2237: Suggest -n/-z instead of ! -z/-n
- SC2238: Warn when redirecting to a known command name, e.g. ls > rm
- SC2239: Warn if the shebang is not an absolute path, e.g. #!bin/sh
- SC2240: Warn when passing additional arguments to dot (.) in sh/dash
- SC1133: Better diagnostics when starting a line with |/||/&&

### Changed
- Most warnings now have useful end positions
- SC1117 about unknown double-quoted escape sequences has been retired

### Fixed
- SC2021 no longer triggers for equivalence classes like `[=e=]`
- SC2221/SC2222 no longer mistriggers on fall-through case branches
- SC2081 about glob matches in `[ .. ]` now also triggers for `!=`
- SC2086 no longer warns about spaces in `$#`
- SC2164 no longer suggests subshells for `cd ..; cmd; cd ..`
- `read -a` is now correctly considered an array assignment
- SC2039 no longer warns about LINENO now that it's POSIX

## v0.5.0 - 2018-05-31
### Added
- SC2233/SC2234/SC2235: Suggest removing or replacing (..) around tests
- SC2232: Warn about invalid arguments to sudo
- SC2231: Suggest quoting expansions in for loop globs
- SC2229: Warn about 'read $var'
- SC2227: Warn about redirections in the middle of 'find' commands
- SC2224/SC2225/SC2226: Warn when using mv/cp/ln without a destination
- SC2223: Quote warning specific to `: ${var=value}`
- SC1131: Warn when using `elseif` or `elsif`
- SC1128: Warn about blanks/comments before shebang
- SC1127: Warn about C-style comments

### Fixed
- Annotations intended for a command's here documents now work
- Escaped characters inside groups in =~ regexes now parse
- Associative arrays are now respected in arithmetic contexts
- SC1087 about `$var[@]` now correctly triggers on any index
- Bad expansions in here documents are no longer ignored
- FD move operations like {fd}>1- now parse correctly

### Changed
- Here docs are now terminated as per spec, rather than by presumed intent
- SC1073: 'else if' is now parsed correctly and not like 'elif'
- SC2163: 'export $name' can now be silenced with 'export ${name?}'
- SC2183: Now warns when printf arg count is not a multiple of format count

## v0.4.7 - 2017-12-08
### Added
- Statically linked binaries for Linux and Windows (see README.md)!
- `-a` flag to also include warnings in `source`d files
- SC2221/SC2222: Warn about overridden case branches
- SC2220: Warn about unhandled error cases in getopt loops
- SC2218: Warn when using functions before they're defined
- SC2216/SC2217: Warn when piping/redirecting to mv/cp and other non-readers
- SC2215: Warn about commands starting with leading dash
- SC2214: Warn about superfluous getopt flags
- SC2213: Warn about unhandled getopt flags
- SC2212: Suggest `false` over `[ ]`
- SC2211: Warn when using a glob as a command name
- SC2210: Warn when redirecting to an integer, e.g. `foo 1>2`
- SC2206/SC2207: Suggest alternatives when using word splitting in arrays
- SC1117: Warn about double quoted, undefined backslash sequences
- SC1113/SC1114/SC1115: Recognized more malformed shebangs

### Fixed
- `[ -v foo ]` no longer warns if `foo` is undefined
- SC2037 is now suppressed by quotes, e.g. `PAGER="cat" man foo`
- Ksh nested array declarations now parse correctly
- Parameter Expansion without colons are now recognized, e.g. `${foo+bar}`
- The `lastpipe` option is now respected with regard to subshell warnings
- `\(` is now respected for grouping in `[`
- Leading `\` is now ignored for commands, to allow alias suppression
- Comments are now allowed after directives to e.g. explain 'disable'


## v0.4.6 - 2017-03-26
### Added
- SC2204/SC2205: Warn about `( -z foo )` and `( foo -eq bar )`
- SC2200/SC2201: Warn about brace expansion in [/[[
- SC2198/SC2199: Warn about arrays in [/[[
- SC2196/SC2197: Warn about deprected egrep/fgrep
- SC2195: Warn about unmatchable case branches
- SC2194: Warn about constant 'case' statements
- SC2193: Warn about `[[ file.png == *.mp3 ]]` and other unmatchables
- SC2188/SC2189: Warn about redirections without commands
- SC2186: Warn about deprecated `tempfile`
- SC1109: Warn when finding `&amp;`/`&gt;`/`&lt;` unquoted
- SC1108: Warn about missing spaces in `[ var= foo ]`

### Changed
- All files are now read as UTF-8 with lenient latin1 fallback, ignoring locale
- Unicode quotes are no longer considered syntactic quotes
- `ash` scripts will now be checked as `dash` with a warning

### Fixed
- `-c` no longer suggested when using `grep -o | wc`
- Comments and whitespace are now allowed before filewide directives
- Here doc delimters with esoteric quoting like `foo""` are now handled
- SC2095 about `ssh` in while read loops is now suppressed when using `-n`
- `%(%Y%M%D)T` now recognized as a single formatter in `printf` checks
- `grep -F` now suppresses regex related suggestions
- Command name checks now recognize busybox applet names


## v0.4.5 - 2016-10-21
### Added
- A Docker build (thanks, kpankonen!)
- SC2185: Suggest explicitly adding path for `find`
- SC2184: Warn about unsetting globs (e.g. `unset foo[1]`)
- SC2183: Warn about `printf` with more formatters than variables
- SC2182: Warn about ignored arguments with `printf` 
- SC2181: Suggest using command directly instead of `if [ $? -eq 0 ]`
- SC1106: Warn when using `test` operators in `(( 1 -eq 2 ))`

### Changed
- Unrecognized directives now causes a warning rather than parse failure.

### Fixed
- Indices in associative arrays are now parsed correctly
- Missing shebang warning squashed when specifying with a directive
- Ksh multidimensional arrays are now supported
- Variables in substring ${a:x:y} expansions now count as referenced
- SC1102 now also handles ambiguous `$((`
- Using `$(seq ..)` will no longer suggest quoting
- SC2148 (missing shebang) is now suppressed when using shell directives
- `[ a '>' b ]` is now recognized as being correctly escaped


## v0.4.4 - 2016-05-15
### Added
- Haskell Stack support (thanks,  Arguggi!)
- SC2179/SC2178: Warn when assigning/appending strings to arrays
- SC1102: Warn about ambiguous `$(((`
- SC1101: Warn when \\ linebreaks have trailing spaces

### Changed
- Directives directly after the shebang now apply to the entire file

### Fixed
- `{$i..10}` is now flagged similar to `{1..$i}`


## v0.4.3 - 2016-01-13
### Fixed
- Build now works on GHC 7.6.3 as found on Debian Stable/Ubuntu LTS


## v0.4.2 - 2016-01-09
### Added
- First class support for the `dash` shell
- The `--color` flag similar to ls/grep's (thanks, haguenau!)
- SC2174: Warn about unexpected behavior of `mkdir -pm` (thanks, eatnumber1!)
- SC2172: Warn about non-portable use of signal numbers in `trap`
- SC2171: Warn about `]]` without leading `[[`
- SC2168: Warn about `local` outside functions

### Fixed
- Warnings about unchecked `cd` will no longer trigger with `set -e`
- `[ a -nt/-ot/-ef b ]` no longer warns about being constant
- Quoted test operators like `[ foo "<" bar ]` now parse
- Escaped quotes in backticks now parse correctly


## v0.4.1 - 2015-09-05
### Fixed
- Added missing files to Cabal, fixing the build


## v0.4.0 - 2015-09-05
### Added
- Support for following `source`d files
- Support for setting default flags in `SHELLCHECK_OPTS`
- An `--external-sources` flag for following arbitrary `source`d files
- A `source` directive to override the filename to `source`
- SC2166: Suggest using `[ p ] && [ q ]` over `[ p -a q ]`
- SC2165: Warn when nested `for` loops use the same variable name
- SC2164: Warn when using `cd` without checking that it succeeds
- SC2163: Warn about `export $var`
- SC2162: Warn when using `read` without `-r`
- SC2157: Warn about `[ "$var " ]` and similar never-empty string matches

### Fixed
- `cat -vnE file` and similar will no longer flag as UUOC
- Nested trinary operators in `(( ))` now parse correctly
- Ksh `${ ..; }` command expansions now parse


## v0.3.8 - 2015-06-20
### Changed
- ShellCheck's license has changed from AGPLv3 to GPLv3.

### Added
- SC2156: Warn about injecting filenames in `find -exec sh -c "{}" \;`

### Fixed
- Variables and command substitutions in brace expansions are now parsed
- ANSI colors are now disabled on Windows
- Empty scripts now parse


## v0.3.7 - 2015-04-16
### Fixed
- Build now works on GHC 7.10
- Use `regex-tdfa` over `regex-compat` since the latter crashes on OS X.

## v0.3.6 - 2015-03-28
### Added
- SC2155: Warn about masked return values in `export foo=$(exit 1)`
- SC2154: Warn when a lowercase variable is referenced but not assigned
- SC2152/SC2151: Warn about bad `return` values like `1234` and `"foo"`
- SC2150: Warn about `find -exec "shell command" \;`

### Fixed
- `coproc` is now supported
- Trinary operator now recognized in `((..))`

### Removed
- Zsh support has been removed


## v0.3.5 - 2014-11-09
### Added
- SC2148: Warn when not including a shebang
- SC2147: Warn about literal ~ in PATH
- SC1086: Warn about `$` in for loop variables, e.g. `for $i in ..`
- SC1084: Warn when the shebang uses `!#` instead of `#!`

### Fixed
- Empty and comment-only backtick expansions now parse
- Variables used in PS1/PROMPT\_COMMAND/trap now count as referenced
- ShellCheck now skips unreadable files and directories
- `-f gcc` on empty files no longer crashes
- Variables in $".." are now considered quoted
- Warnings about expansions in single quotes now include backticks


## v0.3.4 - 2014-07-08
### Added
- SC2146: Warn about precedence when combining `find -o` with actions
- SC2145: Warn when concatenating arrays and strings

### Fixed
- Case statements now support `;&` and `;;&`
- Indices in array declarations now parse correctly
- `let` expressions now parsed as arithmetic expressions
- Escaping is now respected in here documents

### Changed
- Completely drop Makefile in favor of Cabal (thanks rodrigosetti!)


## v0.3.3 - 2014-05-29
### Added
- SC2144: Warn when using globs in `[/[[`
- SC2143: Suggesting using `grep -q` over `[ "$(.. | grep)" ]`
- SC2142: Warn when referencing positional parameters in aliases
- SC2141: Warn about suspicious IFS assignments like `IFS="\n"`
- SC2140: Warn about bad embedded quotes like `echo "var="value""`
- SC2130: Warn when using `-eq` on strings
- SC2139: Warn about define time expansions in alias definitions
- SC2129: Suggest command grouping over `a >> log; b >> log; c >> log`
- SC2128: Warn when expanding arrays without an index
- SC2126: Suggest `grep -c` over `grep|wc`
- SC2123: Warn about accidentally overriding `$PATH`, e.g. `PATH=/my/dir`
- SC1083: Warn about literal `{/}` outside of quotes
- SC1082: Warn about UTF-8 BOMs

### Fixed
- SC2051 no longer triggers for `{1,$n}`, only `{1..$n}`
- Improved detection of single quoted `sed` variables, e.g. `sed '$s///'`
- Stop warning about single quoted variables in `PS1` and similar
- Support for Zsh short form loops, `=(..)`

### Removed
- SC1000 about unescaped lonely `$`, e.g. `grep "^foo$"`


## v0.3.2 - 2014-03-22
### Added
- SC2121: Warn about trying to `set` variables, e.g. `set var = value`
- SC2120/SC2119: Warn when a function uses `$1..` if none are ever passed
- SC2117: Warn when using `su` in interactive mode, e.g. `su foo; whoami` 
- SC2116: Detect useless use of echo, e.g. `for i in $(echo $var)`
- SC2115/SC2114: Detect some catastrophic `rm -r "$empty/"` mistakes
- SC1081: Warn when capitalizing keywords like `While`
- SC1077: Warn when using acute accents instead of backticks

### Fixed
- Shells are now properly recognized in shebangs containing flags
- Stop warning about math on decimals in ksh/zsh
- Stop warning about decimal comparisons with `=`, e.g. `[ $version = 1.2 ]`
- Parsing of `|&`
- `${a[x]}` not counting as a reference of `x`
- `(( x[0] ))` not counting as a reference of `x`


## v0.3.1 - 2014-02-03
### Added
- The `-s` flag to specify shell dialect
- SC2105/SC2104: Warn about `break/continue` outside loops
- SC1076: Detect invalid `[/[[` arithmetic like `[ 1 + 2 = 3 ]`
- SC1075: Suggest using `elif` over `else if`

### Fixed
- Don't warn when comma separating elements in brace expansions
- Improved detection of single quoted `sed` variables, e.g. `sed '$d'`
- Parsing of arithmetic for loops using `{..}` instead of `do..done`
- Don't treat the last pipeline stage as a subshell in ksh/zsh


## v0.3.0 - 2014-01-19
### Added
- A man page (thanks Dridi!)
- GCC compatible error reporting (`shellcheck -f gcc`)
- CheckStyle compatible XML error reporting (`shellcheck -f checkstyle`)
- Error codes for each warning, e.g. SC1234
- Allow disabling warnings with `# shellcheck disable=SC1234`
- Allow disabling warnings with `--exclude`
- SC2103: Suggest using subshells over `cd foo; bar; cd ..`
- SC2102: Warn about duplicates in char ranges, e.g. `[10-15]`
- SC2101: Warn about named classes not inside a char range, e.g. `[:digit:]`
- SC2100/SC2099: Warn about bad math expressions like `i=i+5`
- SC2098/SC2097: Warn about `foo=bar echo $foo`
- SC2095: Warn when using `ssh`/`ffmpeg` in `while read` loops
- Better warnings for missing here doc tokens

### Fixed
- Don't warn when single quoting variables with `ssh/perl/eval`
- `${!var}` is now counted as a variable reference

### Removed
- Suggestions about using parameter expansion over basename
- The `jsoncheck` binary. Use `shellcheck -f json` instead. 


## v0.2.0 - 2013-10-27
### Added
- Suggest `./*` instead of `*` when passing globs to commands
- Suggest `pgrep` over `ps | grep`
- Warn about unicode quotes
- Warn about assigned but unused variables
- Inform about client side expansion when using `ssh`

### Fixed
- CLI tool now uses exit codes and stderr canonically
- Parsing of extglobs containing empty patterns
- Parsing of bash style `eval foo=(bar)`
- Parsing of expansions in here documents
- Parsing of function names containing :+-
- Don't warn about `find|xargs` when using `-print0`


## v0.1.0 - 2013-07-23
### Added
- First release
