#!/usr/bin/env zsh
# Test: Globbing issues

# Using ls in for loop (bad practice)
for file in $(ls *.txt); do  # SC2045: Use globs instead
    echo "$file"
done

# Correct version
for file in *.txt; do
    echo "$file"
done

# Using find with ls
find . -name "*.sh" -exec ls {} \;  # Could use -print or other actions

# Glob that might not match
for file in /nonexistent/*.txt; do  # May warn if glob doesn't match
    echo "$file"
done

# Better: check if glob matches
files=(/tmp/*.log)
if [ -e "${files[1]}" ]; then
    for file in "${files[@]}"; do
        echo "$file"
    done
fi

# ZSH extended glob (valid in zsh)
setopt extended_glob
for file in **/*.txt; do  # Recursive glob
    echo "$file"
done
