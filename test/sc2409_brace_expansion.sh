#!/bin/sh
# SC2409: Brace expansion is not available in POSIX sh

for i in {1..10}; do # [SC2409]
    echo "$i"
done

echo {a..z} # [SC2409]
echo file{1..100}.txt # [SC2409]
