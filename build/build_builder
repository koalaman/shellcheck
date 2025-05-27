#!/bin/sh
if [ $# -eq 0 ]
then
  echo >&2 "No build image directories specified"
  echo >&2 "Example: $0 build/*/"
  exit 1
fi

for dir
do
  ( cd "$dir" && docker build -t "$(cat tag)" . ) || exit 1
done
