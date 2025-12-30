#!/bin/bash
# shellcheck disable=SC2317

# SC2407: ZSH always blocks are only supported in zsh
{ 
    echo "command"
} always { # [SC2407]
    echo "cleanup"
}
