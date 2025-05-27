#!/bin/sh
set -xe
{
  tar xzv --strip-components=1
  chmod +x striptests && ./striptests
  mkdir "$TARGETNAME"
  ( IFS=';'; cabal build $CABALOPTS )
  find . -name shellcheck -type f -exec mv {} "$TARGETNAME/" \;
  ls -l "$TARGETNAME"
  # Stripping invalidates the code signature and the build image does
  # not appear to have anything similar to the 'codesign' tool.
  # "$TARGET-strip" "$TARGETNAME/shellcheck"
  ls -l "$TARGETNAME"
  file "$TARGETNAME/shellcheck" | grep "Mach-O 64-bit arm64 executable"
} >&2
tar czv "$TARGETNAME"
