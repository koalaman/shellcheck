#!/bin/zsh
# Test: Unquoted variable expansion

var="hello world"
echo $var  # SC2086: Quote to prevent word splitting

array=(one two three)
echo $array  # SC2086: Quote to prevent word splitting

# This should be okay
echo "$var"
echo "${array[@]}"
