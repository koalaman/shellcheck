#!/bin/sh
# SC2414: dirstack references need bash or zsh, not POSIX sh

cd ~1 || exit # [SC2414]
cd ~2 || exit # [SC2414]
cd ~+1 || exit # [SC2414]
cd ~-2 || exit # [SC2414]

# These are OK
cd ~ || exit
cd ~/dir || exit
cd ~username || exit
