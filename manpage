#!/bin/sh
echo >&2 "Generating man page using pandoc"
pandoc -s -f markdown-smart -t man shellcheck.1.md -o shellcheck.1 || exit
echo >&2 "Done. You can read it with:   man ./shellcheck.1"
