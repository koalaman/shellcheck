#!/bin/bash
# SC2410: ZSH-style glob exclusion is only supported in zsh

ls *.c~lex.c # [SC2410]
echo *.txt~backup.txt # [SC2410]
find . -name "*.sh~test*.sh" # [SC2410]
