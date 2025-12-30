#!/bin/zsh
# Test: ZSH parameter flags - should NOT warn about undefined variables

text="hello world"

# Uppercase parameter flag
echo "${(U)text}"  # Should not warn about undefined

# Lowercase parameter flag
UPPER="HELLO"
echo "${(L)UPPER}"  # Should not warn about undefined

# Quote parameter flag
special='a"b"c'
echo "${(q)special}"  # Should not warn about undefined

# Split parameter flag
csv="one,two,three"
for item in ${(s:,:)csv}; do  # Should not warn about undefined
    echo "$item"
done

# Multiple flags
mixed="Test String"
echo "${(UL)mixed}"  # Should not warn about undefined

# Undefined variable should still warn
echo "${(U)actually_undefined}"  # SC2154: actually_undefined is referenced but not assigned
