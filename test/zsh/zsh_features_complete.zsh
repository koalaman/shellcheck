#!/usr/bin/env zsh
# Zsh Feature Tests

# Test 1: Parameter expansion flags
var="hello"
echo ${(U)var}  # Uppercase
echo ${(L)var}  # Lowercase  
myarray=(c a b)
echo ${(o)myarray}  # Sorted

# Test 2: Glob qualifiers
ls *(/.)  # Directories only

# Test 3: Anonymous functions with explicit separator
() { echo "anon function"; };

# Test 4: Short for loops
for item (a b c); echo $item

