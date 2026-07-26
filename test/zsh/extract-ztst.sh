#!/usr/bin/env bash
# Extract runnable zsh code out of zsh's own Test/*.ztst files.
#
# A .ztst file is a sequence of sections introduced by a '%' in the first
# column. Inside %prep and %test, indented lines are shell code, blank lines
# separate chunks, lines with '#' in the first column are comments, and any
# other unindented line is a harness directive (the expected status, or a
# '<', '>' or '?' redirection block). See Test/README and Test/B01cd.ztst in
# the zsh distribution.
#
# Each code chunk becomes its own file so that one unparsable chunk does not
# hide the rest, and so that chunks that are only valid on their own are not
# spliced together.
#
# Usage:
#   test/zsh/extract-ztst.sh /path/to/zsh-source
#   ZSH_SOURCE=/path/to/zsh-source test/zsh/extract-ztst.sh
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly SCRIPT_DIR
readonly CORPUS_DIR="$SCRIPT_DIR/corpus"

usage() {
    cat <<'EOF'
Usage: extract-ztst.sh [zsh-source-dir]

Writes one .zsh file per code chunk into test/zsh/corpus/, replacing whatever
is already there.

Environment:
  ZSH_SOURCE   Same as the positional argument.
EOF
}

if [[ ${1:-} == "-h" || ${1:-} == "--help" ]]; then
    usage
    exit 0
fi

ZSH_SOURCE="${1:-${ZSH_SOURCE:-}}"
if [[ -z "$ZSH_SOURCE" ]]; then
    printf 'extract-ztst: no zsh source directory given\n' >&2
    usage >&2
    exit 2
fi

readonly ZTST_DIR="$ZSH_SOURCE/Test"
if [[ ! -d "$ZTST_DIR" ]]; then
    printf 'extract-ztst: %s is not a zsh source tree (no Test/ directory)\n' "$ZSH_SOURCE" >&2
    exit 1
fi

rm -rf "$CORPUS_DIR"
mkdir -p "$CORPUS_DIR"

chunk_lines=()
chunk_count=0
file_count=0

flush_chunk() {
    if [[ ${#chunk_lines[@]} -eq 0 ]]; then
        return 0
    fi

    chunk_count=$((chunk_count + 1))
    local target
    target=$(printf '%s/%s_%03d.zsh' "$CORPUS_DIR" "$1" "$chunk_count")
    {
        printf '#!/usr/bin/env zsh\n'
        printf '# Extracted from zsh Test/%s.ztst, %%%s chunk %d.\n' "$1" "$2" "$chunk_count"
        printf '%s\n' "${chunk_lines[@]}"
    } > "$target"
    chunk_lines=()
    file_count=$((file_count + 1))
}

extract_file() {
    local ztst="$1"
    local base
    base=$(basename "$ztst" .ztst)
    local section=""

    chunk_count=0
    chunk_lines=()

    while IFS= read -r line || [[ -n "$line" ]]; do
        case "$line" in
            %*)
                flush_chunk "$base" "$section"
                section="${line#%}"
                continue
                ;;
        esac

        # Only %prep and %test hold code worth checking; %clean is teardown.
        if [[ "$section" != "prep" && "$section" != "test" ]]; then
            continue
        fi

        case "$line" in
            '#'*)
                continue
                ;;
            '')
                flush_chunk "$base" "$section"
                continue
                ;;
            [[:blank:]]*)
                chunk_lines+=("$line")
                continue
                ;;
            *)
                # A status line or a <, > or ? redirection block ends the code.
                flush_chunk "$base" "$section"
                continue
                ;;
        esac
    done < "$ztst"

    flush_chunk "$base" "$section"
}

while IFS= read -r ztst; do
    extract_file "$ztst"
done < <(find "$ZTST_DIR" -maxdepth 1 -type f -name '*.ztst' -print | sort)

# Record where the corpus came from so the parse baseline can name it. The
# remote and revision are recorded rather than the local path, since the
# baseline is committed and the path differs on every machine.
zsh_revision=$(git -C "$ZSH_SOURCE" rev-parse HEAD 2>/dev/null || echo "unknown")
zsh_remote=$(git -C "$ZSH_SOURCE" remote get-url origin 2>/dev/null || echo "local checkout")
zsh_declared_version=$(sed -n 's/^VERSION=//p' "$ZSH_SOURCE/Config/version.mk" 2>/dev/null || echo "unknown")
printf 'source: %s\nversion: %s\nrevision: %s\n' \
    "$zsh_remote" "$zsh_declared_version" "$zsh_revision" > "$CORPUS_DIR/.source"

printf 'extract-ztst: wrote %d chunk file(s) to %s from %s (zsh %s)\n' \
    "$file_count" "${CORPUS_DIR#"$SCRIPT_DIR"/}" "$ZTST_DIR" "$zsh_declared_version"
