#!/bin/bash
if [ $# -lt 2 ]
then
  echo >&2 "This script builds a source archive (as produced by cabal sdist)"
  echo >&2 "Usage: $0 sourcefile.tar.gz builddir..."
  exit 1
fi

file=$(realpath "$1")
shift

if [ ! -e "$file" ]
then
  echo >&2 "$file does not exist"
  exit 1
fi

set -ex -o pipefail

for dir
do
  tagfile="$dir/tag"
  if [ ! -e "$tagfile" ]
  then
    echo >&2 "$tagfile does not exist"
    exit 2
  fi

  docker run -i "$(< "$tagfile")" < "$file" | tar xz
done
