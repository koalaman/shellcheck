#!/bin/sh
set -xe
{
  tar xzv --strip-components=1
  chmod +x striptests && ./striptests
  mkdir "$TARGETNAME"
  ( IFS=';'; cabal build $CABALOPTS )
  find . -name shellcheck -type f -exec mv {} "$TARGETNAME/" \;
  ls -l "$TARGETNAME"
  "$TARGET-strip" -Sx "$TARGETNAME/shellcheck"
  ls -l "$TARGETNAME"
} >&2
tar czv "$TARGETNAME"
