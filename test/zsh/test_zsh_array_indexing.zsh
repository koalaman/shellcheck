#!/usr/bin/env zsh
# Test: ZSH uses 1-based array indexing (SC2404)

arr=(first second third)

# Wrong: 0-based indexing (bash style)
echo "${arr[0]}"  # SC2404: ZSH arrays are 1-indexed

# Correct: 1-based indexing
echo "${arr[1]}"  # first element
echo "${arr[2]}"  # second element
echo "${arr[3]}"  # third element

# Negative indices (valid in ZSH)
echo "${arr[-1]}"  # last element
echo "${arr[-2]}"  # second to last
