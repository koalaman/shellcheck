#!/bin/bash
# SC2413: Coprocesses are a zsh-specific feature

coproc myproc { # [SC2413]
    while read line; do
        echo "Processed: $line"
    done
}

echo "test" >&p
read -p result
