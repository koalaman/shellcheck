#!/usr/bin/env bash
# Report how much of zsh's own test suite ShellCheck can parse.
#
# Run test/zsh/extract-ztst.sh first to populate test/zsh/corpus/. This script
# checks every extracted chunk in zsh mode and lists the ones that ShellCheck
# cannot parse (SC1072 or SC1073), then diffs that list against the committed
# baseline.
#
# The baseline is tied to a specific zsh revision, since the chunk numbering
# moves whenever zsh edits its tests. The revision it was taken from is
# recorded in the baseline header.
#
# Usage:
#   test/zsh/corpus-report.sh              # compare against the baseline
#   test/zsh/corpus-report.sh --update     # rewrite the baseline
#   SHELLCHECK=/path/to/shellcheck test/zsh/corpus-report.sh
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly SCRIPT_DIR
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
readonly REPO_ROOT
readonly CORPUS_DIR="$SCRIPT_DIR/corpus"
readonly BASELINE="$SCRIPT_DIR/corpus-parse-failures.txt"

UPDATE=0
if [[ ${1:-} == "--update" || ${1:-} == "-u" ]]; then
    UPDATE=1
elif [[ $# -gt 0 ]]; then
    printf 'corpus-report: unknown option %s\n' "$1" >&2
    exit 2
fi

if [[ ! -d "$CORPUS_DIR" ]]; then
    printf 'corpus-report: %s is missing. Run test/zsh/extract-ztst.sh <zsh-source> first.\n' \
        "${CORPUS_DIR#"$REPO_ROOT"/}" >&2
    exit 1
fi

if [[ -z "${SHELLCHECK:-}" ]] && command -v cabal >/dev/null 2>&1; then
    (cd "$REPO_ROOT" && cabal build --allow-newer exe:shellcheck) >/dev/null || {
        printf 'corpus-report: cabal build exe:shellcheck failed\n' >&2
        exit 1
    }
    SHELLCHECK=$(cd "$REPO_ROOT" && cabal list-bin --allow-newer exe:shellcheck)
fi

if [[ -z "${SHELLCHECK:-}" ]]; then
    printf 'corpus-report: no shellcheck binary. Set SHELLCHECK or install cabal.\n' >&2
    exit 1
fi
readonly SHELLCHECK

total=$(find "$CORPUS_DIR" -type f -name '*.zsh' | wc -l | tr -d ' ')
readonly total

output=$(mktemp)
trap 'rm -f "$output"' EXIT INT TERM

# A non-zero exit just means findings were reported, which is the normal case.
find "$CORPUS_DIR" -type f -name '*.zsh' -print0 \
    | xargs -0 -n 200 "$SHELLCHECK" --format=gcc --norc -s zsh > "$output" 2>&1 || true

failures=$(grep -E 'SC107[23]\]' "$output" | sed 's/:.*//' | xargs -n1 basename | sort -u) || failures=""
failure_count=0
if [[ -n "$failures" ]]; then
    failure_count=$(printf '%s\n' "$failures" | wc -l | tr -d ' ')
fi

if [[ $UPDATE -eq 1 ]]; then
    provenance=$(sed 's/^/#   /' "$CORPUS_DIR/.source" 2>/dev/null) || provenance="#   source: unknown"
    {
        printf '# Chunks of zsh'\''s own test suite that ShellCheck cannot parse.\n'
        printf '# Regenerate with test/zsh/corpus-report.sh --update after running\n'
        printf '# test/zsh/extract-ztst.sh against a zsh checkout.\n'
        printf '# %s of %s chunks, extracted from:\n' "$failure_count" "$total"
        printf '%s\n' "$provenance"
        printf '%s\n' "$failures"
    } > "$BASELINE"
    printf 'corpus-report: baseline updated, %s of %s chunks fail to parse\n' "$failure_count" "$total"
    exit 0
fi

if [[ ! -f "$BASELINE" ]]; then
    printf 'corpus-report: no baseline at %s. Run with --update.\n' "${BASELINE#"$REPO_ROOT"/}" >&2
    exit 1
fi

printf 'corpus-report: %s of %s chunks fail to parse (%s)\n' "$failure_count" "$total" "$SHELLCHECK"

if diff_out=$(diff -u <(grep -v '^#' "$BASELINE") <(printf '%s\n' "$failures")); then
    exit 0
fi

printf 'corpus-report: parse results moved away from the baseline.\n'
printf '  Lines starting with - now parse, lines starting with + no longer do.\n'
printf '%s\n' "$diff_out"
exit 1
