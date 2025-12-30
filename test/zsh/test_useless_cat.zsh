#!/usr/bin/env zsh
# Test: Useless cat (UUOC)

# Classic useless cat
cat file.txt | grep pattern  # SC2002: Useless cat

# Correct version
grep pattern file.txt
# or
grep pattern < file.txt

# Another useless cat
result=$(cat data.txt | head -n 10)  # SC2002: Useless cat

# Correct version
result=$(head -n 10 data.txt)

# Valid cat usage (multiple files)
cat file1.txt file2.txt | grep pattern  # This is okay

# Valid cat with options
cat -n file.txt | less  # This is okay
