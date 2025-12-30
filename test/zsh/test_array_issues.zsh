#!/usr/bin/env zsh
# Test: Array usage issues

# Using array as string
array=(one two three)
echo "$array"  # SC2128: Expanding an array without an index only gives the first element

# Correct usage
echo "${array[@]}"
echo "${array[*]}"

# Undefined array index
echo "${undefined_array[1]}"  # SC2154: undefined_array is referenced but not assigned

# Test with associative array
typeset -A assoc_array
assoc_array[key1]="value1"
echo "${assoc_array[key1]}"  # This is fine

# Using regular variable as array
regular_var="string"
echo "${regular_var[1]}"  # May warn about using string as array
