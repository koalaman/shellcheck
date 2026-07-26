#!/usr/bin/env zsh
# Test: unquoted leading ^ under extended_glob (SC2406)

setopt extended_glob

# Recursive ** works without extended_glob, so it is never reported.
for file in **/*.txt; do
    echo "$file"
done

# ^ is a negation pattern here, not a literal caret.
ls ^*.txt  # SC2406

# Passing a regex unquoted hits the same trap.
grep ^root /etc/passwd  # SC2406

# Quoting keeps the caret literal.
grep '^root' /etc/passwd
