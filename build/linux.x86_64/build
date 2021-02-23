#!/bin/sh
set -xe
{
  tar xzv --strip-components=1
  chmod +x striptests && ./striptests
  mkdir "$TARGETNAME"
  cabal update
  ( IFS=';'; cabal build $CABALOPTS --enable-executable-static )
  find . -name shellcheck -type f -exec mv {} "$TARGETNAME/" \;
  ls -l "$TARGETNAME"
  strip -s "$TARGETNAME/shellcheck"
  ls -l "$TARGETNAME"
  "$TARGETNAME/shellcheck" --version
} >&2
tar czv "$TARGETNAME"
