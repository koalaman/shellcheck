#!/usr/bin/env bash
# Golden test harness for ShellCheck's zsh support.
#
# For every fixture it records the shellcheck exit code and the sorted set of
# emitted SC codes, then diffs that against a committed "<fixture>.golden" file.
#
# Usage:
#   test/zsh/run-golden.sh              # verify against goldens
#   test/zsh/run-golden.sh --update     # regenerate goldens
#   SHELLCHECK=/path/to/shellcheck test/zsh/run-golden.sh
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly SCRIPT_DIR
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
readonly REPO_ROOT

UPDATE=0
CORPUS_ONLY=0
declare -a EXPLICIT_FIXTURES=()

usage() {
    cat <<'EOF'
Usage: run-golden.sh [--update] [--corpus-only] [fixture ...]

  --update        Rewrite golden files from current shellcheck output.
  --corpus-only   Only run the extracted zsh corpus under test/zsh/corpus/.
  fixture ...     Run only the named fixture paths.

Environment:
  SHELLCHECK      Path to the shellcheck binary. Defaults to `cabal list-bin`,
                  then any binary found under dist-newstyle/, then $PATH.
EOF
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --update|-u)
            UPDATE=1
            shift
            ;;
        --corpus-only)
            CORPUS_ONLY=1
            shift
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        -*)
            printf 'run-golden: unknown option %s\n' "$1" >&2
            usage >&2
            exit 2
            ;;
        *)
            EXPLICIT_FIXTURES+=("$1")
            shift
            ;;
    esac
done

find_shellcheck() {
    if [[ -n "${SHELLCHECK:-}" ]]; then
        printf '%s\n' "$SHELLCHECK"
        return 0
    fi

    local from_cabal
    if from_cabal=$(cd "$REPO_ROOT" && cabal list-bin --allow-newer exe:shellcheck 2>/dev/null); then
        if [[ -x "$from_cabal" ]]; then
            printf '%s\n' "$from_cabal"
            return 0
        fi
    fi

    local found
    found=$(find "$REPO_ROOT/dist-newstyle" -type f -name shellcheck -perm -u+x -print 2>/dev/null | sort | sed -n '1p')
    if [[ -n "$found" ]]; then
        printf '%s\n' "$found"
        return 0
    fi

    if command -v shellcheck >/dev/null 2>&1; then
        command -v shellcheck
        return 0
    fi

    return 1
}

SHELLCHECK_BIN=$(find_shellcheck) || {
    printf 'run-golden: no shellcheck binary found. Run "cabal build exe:shellcheck" or set SHELLCHECK.\n' >&2
    exit 1
}
readonly SHELLCHECK_BIN

# Emits "exit: N" followed by the sorted unique SC codes the fixture produced.
summarize() {
    local fixture="$1"
    local output status codes

    set +e
    output=$("$SHELLCHECK_BIN" --format=gcc --norc -- "$fixture" 2>&1)
    status=$?
    set -e

    # A clean fixture emits no SC codes at all, so an empty grep is expected.
    codes=$(printf '%s\n' "$output" | grep -oE 'SC[0-9]{4}' | sort -u) || codes=""

    printf 'exit: %d\n' "$status"
    if [[ -n "$codes" ]]; then
        printf '%s\n' "$codes"
    fi
    return 0
}

collect_fixtures() {
    if [[ ${#EXPLICIT_FIXTURES[@]} -gt 0 ]]; then
        printf '%s\n' "${EXPLICIT_FIXTURES[@]}"
        return 0
    fi

    if [[ $CORPUS_ONLY -eq 0 ]]; then
        find "$SCRIPT_DIR" -maxdepth 1 -type f \( -name '*.zsh' -o -name '*.sh' \) \
            -not -name 'run-golden.sh' -not -name 'extract-ztst.sh' -print | sort
        find "$REPO_ROOT/test" -maxdepth 1 -type f -name 'sc24*.sh' -print | sort
    fi

    if [[ -d "$SCRIPT_DIR/corpus" ]]; then
        find "$SCRIPT_DIR/corpus" -type f -name '*.zsh' -print | sort
    fi
}

pass=0
fail=0
updated=0
missing=0
declare -a failures=()

while IFS= read -r fixture; do
    [[ -n "$fixture" ]] || continue
    golden="${fixture}.golden"
    actual=$(summarize "$fixture")

    if [[ $UPDATE -eq 1 ]]; then
        printf '%s\n' "$actual" > "$golden"
        updated=$((updated + 1))
        continue
    fi

    if [[ ! -f "$golden" ]]; then
        printf 'MISSING GOLDEN  %s\n' "${fixture#"$REPO_ROOT"/}"
        missing=$((missing + 1))
        failures+=("$fixture")
        continue
    fi

    if diff_out=$(diff -u "$golden" <(printf '%s\n' "$actual") 2>&1); then
        pass=$((pass + 1))
    else
        fail=$((fail + 1))
        failures+=("$fixture")
        printf 'FAIL  %s\n' "${fixture#"$REPO_ROOT"/}"
        printf '%s\n' "$diff_out" | sed 's/^/      /'
    fi
done < <(collect_fixtures)

if [[ $UPDATE -eq 1 ]]; then
    printf 'run-golden: updated %d golden file(s) using %s\n' "$updated" "$SHELLCHECK_BIN"
    exit 0
fi

printf '\nrun-golden: %d passed, %d failed, %d missing golden (binary: %s)\n' \
    "$pass" "$fail" "$missing" "$SHELLCHECK_BIN"

if [[ ${#failures[@]} -gt 0 ]]; then
    printf 'Failing fixtures:\n'
    printf '  %s\n' "${failures[@]#"$REPO_ROOT"/}"
    exit 1
fi

exit 0
