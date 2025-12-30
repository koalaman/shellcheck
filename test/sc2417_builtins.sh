#!/bin/bash
# SC2417: ZSH-specific builtins

autoload -U compinit # [SC2417]
zmodload zsh/complist # [SC2417]
compinit # [SC2417]
compdef _git g # [SC2417]
zstyle ':completion:*' menu select # [SC2417]
bindkey '^R' history-incremental-search-backward # [SC2417]
zle -N my-widget # [SC2417]
