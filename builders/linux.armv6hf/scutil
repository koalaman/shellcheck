#!/bin/dash
# Various ShellCheck build utility functions

# Generally set a ulimit to avoid QEmu using too much memory
ulimit -v "$((10*1024*1024))"
# If we happen to invoke or run under QEmu, make sure to follow execve.
# This requires a patched QEmu.
export QEMU_EXECVE=1

# Retry a command until it succeeds
# Usage: scutil retry 3 mycmd
retry() {
  n="$1"
  ret=1
  shift
  while [ "$n" -gt 0 ]
  do
    "$@"
    ret=$?
    [ "$ret" = 0 ] && break
    n=$((n-1))
  done
  return "$ret"
}

# Install all dependencies from a freeze file
# Usage: scutil install_from_freeze /path/cabal.project.freeze cabal install
install_from_freeze() {
  linefeed=$(printf '\nx')
  linefeed=${linefeed%x}
  flags=$(
    sed 's/constraints:/&\n /' "$1" |
    grep -vw -e rts -e base |
    sed -n -e 's/^  *\([^,]*\).*/\1/p' |
    sed -e 's/any\.\([^ ]*\) ==\(.*\)/\1-\2/; te; s/.*/--constraint\n&/; :e')
  shift
  # shellcheck disable=SC2086
  ( IFS=$linefeed; set -x; "$@" $flags )
}

# Run a command under emulation.
# This assumes the correct emulator is named 'qemu' and the chroot is /chroot
# Usage: scutil emu echo "Hello World"
emu() {
  chroot /chroot /bin/qemu /usr/bin/env "$@"
}

"$@"
