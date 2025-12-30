#!/bin/zsh
# Test: Extended glob requires setopt (SC2406)

# Using ** recursive glob without setopt
for file in **/*.txt; do  # SC2406: Consider adding setopt extended_glob
    echo "$file"
done

# Using ^ negation without setopt
ls ^*.txt  # SC2406: Consider adding setopt extended_glob

# Correct: with setopt
setopt extended_glob
for file in **/*.txt; do
    echo "$file"
done

ls ^*.txt  # Now okay
