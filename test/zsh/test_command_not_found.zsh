#!/usr/bin/env zsh
# Test: Command not found

nonexistent_command  # SC2317 or similar: command not found

# Using command in if
if invalid_cmd; then  # Should warn about command
    echo "won't run"
fi

# Valid commands should not warn
ls /tmp
echo "hello"

# Typo in command
ehco "oops"  # SC: command not found (typo for echo)
