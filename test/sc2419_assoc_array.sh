#!/bin/sh
# SC2419: Associative arrays require bash 4+ or zsh

typeset -A hash # [SC2419]
hash[key]=value
echo "${hash[key]}"
