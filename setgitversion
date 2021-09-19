#!/bin/sh -xe
# This script hardcodes the `git describe` version as ShellCheck's version number.
# This is done to allow shellcheck --version to differ from the cabal version when
# building git snapshots.

file="src/ShellCheck/Data.hs"
test -e "$file"
tmp=$(mktemp)
version=$(git describe)
sed -e "s/=.*VERSIONSTRING.*/= \"$version\" -- VERSIONSTRING, DO NOT SUBMIT/" "$file" > "$tmp"
mv "$tmp" "$file"
