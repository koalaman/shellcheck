#!/bin/sh
cabal() {
  wine /haskell/bin/cabal.exe  "$@"
}

set -xe
{
  tar xzv --strip-components=1
  chmod +x striptests && ./striptests
  mkdir "$TARGETNAME"
  ( IFS=';'; cabal build $CABALOPTS )
  find dist*/ -name shellcheck.exe -type f -ls -exec mv {} "$TARGETNAME/" \;
  ls -l "$TARGETNAME"
  wine "/haskell/mingw/bin/strip.exe" -s "$TARGETNAME/shellcheck.exe"
  ls -l "$TARGETNAME"
  wine "$TARGETNAME/shellcheck.exe" --version
} >&2
tar czv "$TARGETNAME"
