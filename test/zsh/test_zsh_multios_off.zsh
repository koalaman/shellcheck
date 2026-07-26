#!/usr/bin/env zsh
# Test: with MULTIOS off, competing redirections are a mistake again (SC2261)

unsetopt multios

print hi > /tmp/zsh-one > /tmp/zsh-two  # SC2261
