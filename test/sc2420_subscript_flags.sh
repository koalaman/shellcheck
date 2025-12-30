#!/bin/bash
# SC2420: Array subscript flags are zsh-specific

arr=(one two three)
echo ${arr[(i)two]} # [SC2420]
echo ${arr[(r)t*]} # [SC2420]
echo ${arr[(I)three]} # [SC2420]
