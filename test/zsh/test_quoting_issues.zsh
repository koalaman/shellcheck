#!/bin/zsh
# Test: Quote issues in conditions

file="my file.txt"

# Unquoted variable in test
if [ -f $file ]; then  # SC2086: Quote to prevent word splitting
    echo "File exists"
fi

# Correct version
if [ -f "$file" ]; then
    echo "File exists"
fi

# Unquoted in [[ ]] - less critical but still good practice
if [[ -f $file ]]; then  # May warn depending on configuration
    echo "File exists"
fi

# Comparing unquoted variables
var1="value"
var2="other value"
if [ $var1 = $var2 ]; then  # SC2086: Quote to prevent word splitting
    echo "Equal"
fi
