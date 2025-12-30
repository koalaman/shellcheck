#!/bin/sh
# SC2421: Power operator ** is not available in POSIX sh

echo $((2 ** 10)) # [SC2421]
result=$((base ** exponent)) # [SC2421]
