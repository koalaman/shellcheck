# Zsh test suite for ShellCheck

Two harnesses live here. The golden harness is the end-to-end gate for zsh
support, and the corpus harness measures how much of zsh's own test suite
ShellCheck can parse.

## Golden harness

`run-golden.sh` runs every fixture and records the shellcheck exit code plus
the sorted set of emitted SC codes, then diffs that against a committed
`<fixture>.golden` file.

```bash
./test/zsh/run-golden.sh                    # verify
./test/zsh/run-golden.sh --update           # regenerate after an intentional change
./test/zsh/run-golden.sh --no-build         # skip the cabal build step
SHELLCHECK=/usr/bin/shellcheck ./test/zsh/run-golden.sh
```

It builds `exe:shellcheck` first, because `cabal test` does not relink the
executable and the goldens would otherwise compare against a stale binary.

Fixtures covered: everything in `test/zsh/` plus the `test/sc24*.sh`
portability fixtures. Each `.golden` is a snapshot, not an assertion of
correctness. A golden that lists an `SC1xxx` code records a parse failure that
is still outstanding, so regenerating after a parser fix should shrink it.

The full gate is:

```bash
cabal test --allow-newer && ./test/zsh/run-golden.sh
```

CI runs this in the `zsh_golden` job, which builds the real binary rather than
using the sdist tarball, since `test/zsh/` is not shipped in the tarball.

### What the fixtures cover

Valid zsh that must stay clean: parameter expansion flags, glob qualifiers,
short and `foreach` loops, anonymous functions, `always` blocks, bare array
subscripts, `=(...)` process substitution and MULTIOS redirections.

Zsh-only syntax in a bash or sh script, which must be reported: `test/sc24*.sh`
holds one fixture per surviving SC24xx code.

Ordinary findings in zsh scripts, which must keep working: quoting, unused and
undefined variables, redirection mistakes and test operators.

Option-sensitive behavior: `setopt` and `unsetopt` change what several checks
report, so there are paired fixtures for extended_glob, ksh_arrays and multios
with the option both on and off.

## Corpus harness

`extract-ztst.sh` pulls the shell code out of a zsh checkout's `Test/*.ztst`
files, one file per code chunk, into `test/zsh/corpus/`. That directory is
gitignored: it is zsh's source, not ours, and it is roughly 2900 files.

```bash
./test/zsh/extract-ztst.sh /path/to/zsh-source
./test/zsh/corpus-report.sh              # compare against the baseline
./test/zsh/corpus-report.sh --update     # rewrite the baseline
```

`corpus-report.sh` checks every chunk in zsh mode and lists the ones ShellCheck
cannot parse (SC1072 or SC1073), then diffs that list against
`corpus-parse-failures.txt`. Lines that disappear are chunks that now parse,
and lines that appear are regressions.

The baseline is tied to the zsh revision it was taken from, which is recorded
in its header, because the chunk numbering moves whenever zsh edits its tests.
The `zsh_golden` CI job pins the same revision, so bump the pin in
`.github/workflows/build.yml` and regenerate the baseline together.
