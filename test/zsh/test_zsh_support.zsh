#!/bin/zsh
# Test script for zsh support in ShellCheck

# Test 1: Zsh parameter expansion flags
var="hello"
echo ${(U)var}  # Uppercase
echo ${(L)var}  # Lowercase
echo ${(o)array}  # Sort array

# Test 2: Glob qualifiers
ls *(.)         # Regular files only
ls *(/)         # Directories only
ls *(om[1,3])   # 3 most recently modified

# Test 3: Anonymous functions
() {
    local temp="inside anon function"
    echo $temp
}

() { echo "anon with args: $1 $2" } arg1 arg2

# Test 4: Short for loops
for i (a b c) echo $i

for file (*.txt) {
    echo "Processing: $file"
}

# Test 5: Zsh-specific variables
echo $ZSH_VERSION
echo $MATCH
echo $match

# Test 6: Zsh builtins
autoload -U compinit
compinit

whence ls
where ls

# Test 7: Combined features
for item (${(o)myarray}) {
    echo "Item: $item"
}

echo "Zsh support test complete!"
