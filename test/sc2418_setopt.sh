#!/bin/bash
# SC2418: setopt/unsetopt are zsh-specific builtins

setopt extended_glob # [SC2418]
setopt no_case_glob # [SC2418]
unsetopt beep # [SC2418]
