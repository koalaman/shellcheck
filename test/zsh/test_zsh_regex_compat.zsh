#!/usr/bin/env zsh
# Test: zsh's =~ reports matches in $MATCH and $match, not BASH_REMATCH (SC2405)

text="hello123world"

if [[ $text =~ [0-9]+ ]]; then
    echo "bash spelling: $BASH_REMATCH"     # SC2405
    echo "group: ${BASH_REMATCH[1]}"        # SC2405
fi

# The zsh spelling.
if [[ $text =~ ([0-9]+) ]]; then
    echo "whole match: $MATCH"
    echo "group: $match[1]"
fi
