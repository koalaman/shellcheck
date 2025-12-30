#!/bin/zsh
# Test: ZSH regex matching works differently than bash (SC2405)

text="hello123world"

# Wrong: using =~ like in bash
if [[ $text =~ [0-9]+ ]]; then  # SC2405: ZSH =~ works differently
    echo "has numbers"
fi

# Correct in ZSH: use == with glob pattern or match condition
if [[ $text == *[0-9]* ]]; then
    echo "has numbers"
fi

# Or use regex in a different way
if [[ $text =~ "[0-9]+" ]]; then
    echo "has numbers"
fi
