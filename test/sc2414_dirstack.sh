#!/bin/bash
# SC2414: ZSH directory stack references are zsh-specific

cd ~1 # [SC2414]
cd ~2 # [SC2414]
cd ~+1 # [SC2414]
cd ~-2 # [SC2414]

# These are OK
cd ~
cd ~/dir
cd ~username
