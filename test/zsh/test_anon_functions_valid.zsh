#!/bin/zsh
# Test: ZSH anonymous functions - valid syntax

# Simple anonymous function
() {
    echo "Anonymous function executed"
}

# Anonymous function with parameters
() {
    echo "First arg: $1"
    echo "Second arg: $2"
} arg1 arg2

# Anonymous function with local variables
() {
    local temp="temporary"
    echo "$temp"
}

# Anonymous function in pipeline
echo "test" | () {
    read line
    echo "Read: $line"
}

# Nested anonymous functions
() {
    echo "Outer"
    () {
        echo "Inner"
    }
}

# Anonymous function with command substitution
result=$( () {
    echo "computed value"
} )
echo "Result: $result"
