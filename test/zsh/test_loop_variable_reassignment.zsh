#!/usr/bin/env zsh
# Test: Loop variable reassignment issues

# Reassigning loop variable inside loop (bad practice)
for i in 1 2 3 4 5; do
    echo "$i"
    i=10  # SC2165: This loop variable is reassigned
done

# ZSH short for loop with reassignment
for j (a b c) {
    echo "$j"
    j="modified"  # SC2165: This loop variable is reassigned
}

# While loop with reassignment
count=0
while [ $count -lt 5 ]; do
    echo "$count"
    count=$((count + 1))  # This is okay - it's the loop control
done

# Read loop - reassigning read variable
while read line; do
    echo "$line"
    line="changed"  # SC2030 or similar: modification won't affect loop
done < /dev/null
