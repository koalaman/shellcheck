#!/bin/bash
# Test: Using ZSH-only features in bash (SC2400-2403)

# ZSH parameter flags in bash script
text="hello"
echo "${(U)text}"  # SC2400: ZSH parameter flags only work in zsh

# ZSH glob qualifiers in bash
for file in *(.)  # SC2401: ZSH glob qualifiers only work in zsh
    echo "$file"
done

# ZSH anonymous function in bash
() {  # SC2402: ZSH anonymous functions only work in zsh
    echo "hello"
}

# ZSH short for loop in bash
for i (1 2 3) echo $i  # SC2403: ZSH short for syntax only works in zsh
