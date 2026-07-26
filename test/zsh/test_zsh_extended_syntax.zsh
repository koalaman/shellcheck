#!/usr/bin/env zsh
# Test: zsh syntax that other shells do not have

# Bare array subscripts, equivalent to ${arr[2]}.
arr=(a b c)
print "$arr[2]"

# =(...) writes the output to a temp file instead of a fifo.
diff =(print one) =(print two)

# <(...) and >(...) work the same as in bash.
diff <(print one) <(print two)

# MULTIOS sends the output to both files.
print hi > /tmp/zsh-multios-one > /tmp/zsh-multios-two

# The csh style loop.
foreach f (a b c)
    print "$f"
end
