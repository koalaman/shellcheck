#!/bin/zsh
# Test: ZSH short for loop variable tracking

# Variable should be tracked in short for loop
for i (1 2 3) {
    echo "$i"
}

# Using i after the loop - should not warn about undefined
echo "Last value: $i"

# Nested short for loops
for x (a b c) {
    for y (1 2 3) {
        echo "$x-$y"
    }
}

# Check variables are accessible after nested loops
echo "x=$x, y=$y"
