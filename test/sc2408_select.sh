#!/bin/bash
# SC2408: select loops are a zsh/ksh feature

select option in "Option 1" "Option 2" "Option 3"; do # [SC2408]
    echo "You selected: $option"
    break
done
