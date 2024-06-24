#!/bin/sh
set -xe
mkdir /scratch && cd /scratch
{
  tar xzv --strip-components=1
  cp /etc/cabal.project.freeze .
  chmod +x striptests && ./striptests
  mkdir "$TARGETNAME"
  # This script does not cabal update because compiling anything new is slow
  ( IFS=';'; cabal build $CABALOPTS --enable-executable-static )
  find . -name shellcheck -type f -exec mv {} "$TARGETNAME/" \;
  ls -l "$TARGETNAME"
  strip -s "$TARGETNAME/shellcheck"
  ls -l "$TARGETNAME"
  "$TARGETNAME/shellcheck" --version
} >&2
tar czv "$TARGETNAME"
