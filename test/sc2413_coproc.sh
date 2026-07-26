#!/usr/bin/env zsh
# SC2413: zsh's coproc takes no name, unlike bash's

coproc myproc { # [SC2413]
    while read -r line; do
        echo "Processed: $line"
    done
}

# The zsh spelling: no name, and the >&p / <&p redirections.
coproc cat
print -p "test"
read -rp result
