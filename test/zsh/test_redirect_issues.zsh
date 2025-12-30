#!/bin/zsh
# Test: Redirect and pipe issues

# Redirecting to same file you're reading
sort file.txt > file.txt  # SC2094: Make sure not to read and write the same file

# Correct: use temp file
sort file.txt > file.txt.tmp && mv file.txt.tmp file.txt

# Or use sponge if available
# sort file.txt | sponge file.txt

# Stderr redirect issues
command 2>&1 > file.txt  # SC2069: Wrong order; only stdout goes to file
# Correct version
command > file.txt 2>&1

# Useless redirect
echo "test" > /dev/null 2>&1 | grep pattern  # SC: redirect before pipe is useless

# Multiple redirects to same fd
command > out.txt > out2.txt  # SC2216: Second redirect overrides first

# Piping stderr without redirecting it
command | grep error  # Won't catch errors unless stderr is redirected

# Correct version
command 2>&1 | grep error
