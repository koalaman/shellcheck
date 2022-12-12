#!/bin/bash
# This script builds ShellCheck through `stack` using
# various resolvers. It's run via distrotest.

resolvers=(
#  nightly-"$(date -d "3 days ago" +"%Y-%m-%d")"
)

die() { echo "$*" >&2; exit 1; }

[ -e "ShellCheck.cabal" ] ||
  die "ShellCheck.cabal not in current dir"
[ -e "stack.yaml" ] ||
  die "stack.yaml not in current dir"
command -v stack ||
  die "stack is missing"

stack setup        || die "Failed to setup with default resolver"
stack build --test || die "Failed to build/test with default resolver"

# Nice to haves, but not necessary
for resolver in "${resolvers[@]}"
do
  stack --resolver="$resolver" setup        || die "Failed to setup $resolver. This probably doesn't matter."
  stack --resolver="$resolver" build --test || die "Failed build/test with $resolver! This probably doesn't matter."
done

echo "Success"
