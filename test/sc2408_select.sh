#!/bin/sh
# SC2408: select is not POSIX, but bash, ksh and zsh all have it

select option in "Option 1" "Option 2" "Option 3"; do # [SC2408]
    echo "You selected: $option"
    break
done
