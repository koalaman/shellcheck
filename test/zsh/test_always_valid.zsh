#!/usr/bin/env zsh
# Test: zsh always blocks are valid syntax and must not warn in zsh mode.

{
    echo "try block"
} always {
    echo "cleanup runs either way"
}

# The closing brace of a zsh list needs no preceding separator.
{ echo try } always { echo cleanup }

# Semicolons and newlines are allowed after always, but not before it.
{
    echo try
} always
{
    echo cleanup
}
