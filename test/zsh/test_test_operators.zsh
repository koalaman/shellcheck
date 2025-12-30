#!/usr/bin/env zsh
# Test: File test operators and common mistakes

file="/tmp/test.txt"

# Using -a instead of -e (deprecated)
if [ -a "$file" ]; then  # SC2166: -a is deprecated, use -e
    echo "Exists"
fi

# Using -o for OR (deprecated in [ ])
if [ -f "$file" -o -d "$file" ]; then  # SC2166: Use || instead
    echo "File or directory"
fi

# Correct modern versions
if [ -e "$file" ]; then
    echo "Exists"
fi

if [ -f "$file" ] || [ -d "$file" ]; then
    echo "File or directory"
fi

# Or use [[ ]]
if [[ -f "$file" || -d "$file" ]]; then
    echo "File or directory"
fi

# Missing quotes around variable
if [ -f $file ]; then  # SC2086: Quote to prevent word splitting
    echo "Exists"
fi

# Testing for empty string incorrectly
if [ $var ]; then  # SC2236: Use -n or -z for string tests
    echo "Not empty"
fi

# Correct version
if [ -n "$var" ]; then
    echo "Not empty"
fi
