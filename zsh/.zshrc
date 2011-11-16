# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="flarp"

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

plugins=(git vi-mode django pip)

source $ZSH/oh-my-zsh.sh

alias gp='git push'
alias gc='git commit -m'
alias gs='git status'
alias gb='git branch'
alias gdc='git diff --word-diff --color=always --cached'
alias gdh='git diff --word-diff --color=always HEAD'
alias ga='git add'
alias gpl='git pull'
alias gcp='git cherry-pick'
alias gl="git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative"
alias gcpn='git cherry-pick -n'
alias ack='ack --literal --smart-case'
alias sch='scheme-r5rs'
alias sshd='ssh mcrittenden@iade106lmp01.blackmesh.com -p 9567'
alias sshp='ssh iade106@iade106lwb01.blackmesh.com -p 9567'
alias pm='python manage.py'
alias pmr='python manage.py runserver'
alias up='sudo packer -Syu'
alias c='clear'
alias feh='feh -ZF'

bindkey -M viins '^r' history-incremental-search-backward
bindkey -M vicmd '^r' history-incremental-search-backward

source /etc/profile
