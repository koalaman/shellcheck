#!/bin/zsh
# Test: Common programming errors

# Using = instead of == in test
var="test"
if [ $var = "test" ]; then  # Actually valid in POSIX, but may suggest ==
    echo "Equal"
fi

# Using == in assignment (bash/zsh specific, valid)
if [[ $var == "test" ]]; then
    echo "Equal"
fi

# Arithmetic comparison with strings
num="5"
if [ $num -eq 5 ]; then  # SC2086: num should be quoted
    echo "Five"
fi

# Missing $ in arithmetic
i=0
i=i+1  # SC2007 or similar: should use $(( )) for arithmetic

# Correct version
i=$((i+1))

# Comparing strings numerically
str1="abc"
str2="def"
if [ $str1 -gt $str2 ]; then  # SC2071: -gt is for numeric comparison
    echo "Greater"
fi

# Should use string comparison
if [ "$str1" \> "$str2" ]; then
    echo "Greater"
fi
