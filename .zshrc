# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="flarp"
#ZSH_THEME="random"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git vi-mode django command-not-found pip)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
alias gp='git push'
alias gc='git commit -m'
alias gs='git status'
alias gb='git branch'
alias gdc='git diff --word-diff --color=always --cached'
alias gdh='git diff --word-diff --color=always HEAD'
alias ga='git add'
alias gpl='git pull'
alias gcp='git cherry-pick'
alias gcpn='git cherry-pick -n'
alias ack='ack-grep --literal --smart-case'
alias sch='scheme-r5rs'
alias sshd='ssh mcrittenden@iade106lmp01.blackmesh.com -p 9567'
alias sshp='ssh iade106@iade106lwb01.blackmesh.com -p 9567'
alias pm='python manage.py'
alias pmr='python manage.py runserver'

bindkey -M viins '^r' history-incremental-search-backward
bindkey -M vicmd '^r' history-incremental-search-backward
