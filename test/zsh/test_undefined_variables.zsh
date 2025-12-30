#!/usr/bin/env zsh
# Test: Undefined variables

echo "$undefined_var"  # SC2154: undefined_var is referenced but not assigned

# Test with command substitution
result=$(echo "$another_undefined")  # SC2154: another_undefined is referenced but not assigned

# This should be okay
defined_var="value"
echo "$defined_var"
