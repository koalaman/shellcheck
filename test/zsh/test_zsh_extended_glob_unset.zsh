#!/usr/bin/env zsh
# Test: without extended_glob a leading ^ is literal, so SC2406 stays quiet

grep ^root /etc/passwd

for file in **/*.txt; do
    echo "$file"
done

setopt no_extended_glob
grep ^root /etc/passwd
