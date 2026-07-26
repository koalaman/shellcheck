#!/usr/bin/env zsh
# Test: the 'function { ... }' spelling of a zsh anonymous function.

variable=outside
function {
    local variable=inside
    print "I am $variable with arguments $*"
} this and that
print "I am $variable"

# No arguments.
function {
    echo "no args"
}

# Nested inside another anonymous function, with no separator before the brace.
function {
    function { echo inner }
}
