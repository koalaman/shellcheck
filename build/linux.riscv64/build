#!/bin/sh
set -xe
IFS=';'
{
  mkdir -p /tmp/scratch
  cd /tmp/scratch
  tar xzv --strip-components=1
  chmod +x striptests && ./striptests
  # Use a freeze file to ensure we use the same dependencies we cached during
  # the docker image build. We don't want to spend time compiling anything new.
  cp /etc/cabal.project.freeze .
  mkdir "$TARGETNAME"
  # Retry in case of random segfault
  scutil retry 3 cabal build --enable-executable-static
  find . -name shellcheck -type f -exec mv {} "$TARGETNAME/" \;
  ls -l "$TARGETNAME"
  "$TARGET-strip" -s "$TARGETNAME/shellcheck"
  ls -l "$TARGETNAME"
  "$TARGETNAME/shellcheck" --version
} >&2
tar czv "$TARGETNAME"
