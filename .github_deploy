#!/bin/bash
set -x
shopt -s extglob

export EDITOR="touch"

# Sanity check
gh --version || exit 1
hub release show latest || exit 1

for tag in $TAGS
do
  if ! hub release show "$tag"
  then
    echo "Creating new release $tag"
    git show --no-patch  --format='format:%B' > description
    hub release create -F description "$tag"
  fi

  files=()
  for file in deploy/*
  do
    [[ $file == *.@(xz|gz|zip) ]] || continue
    [[ $file == *"$tag"* ]] || continue
    files+=("$file")
  done
  gh release upload "$tag" "${files[@]}" --clobber || exit 1
done
