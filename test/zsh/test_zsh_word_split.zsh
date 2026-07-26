#!/usr/bin/env zsh
# Test: SC2086 in zsh reports empty removal and array splitting, not word
# splitting, because zsh does not split unquoted scalars by default.

value='a b'
rm $value

files=(one two)
rm $files

# Quoting is still the fix.
rm "$value"
