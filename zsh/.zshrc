# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh
export VIMCLOJURE_SERVER_JAR="$HOME/Misc/server-2.3.1.jar"

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

plugins=(gitfast vi-mode)

source $ZSH/oh-my-zsh.sh
source $HOME/.rvm/scripts/rvm

alias gp='git push'
alias gc='git commit -m'
alias gs='git status'
alias gb='git branch'
alias gdc='git diff --color=always --cached'
alias gdcw='git diff --word-diff --color=always --cached'
alias gdh='git diff'
alias gdhw='git diff --word-diff --color=always HEAD'
alias ga='git add'
alias gpl='git pull'
alias gcp='git cherry-pick'
alias gl="git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative"
alias ack='ack --all --literal --smart-case'
alias up='sudo packer -Syu'
alias c='clear'
alias feh='feh -ZF'
alias ll='ls -lah'
alias e='dtrx'
alias ag='ag -a'
findreplace_function() {
  find ./ -type f -exec sed -i 's/'$1'/'$2'/' {} \;
}
renamereplace_function() {
  find . -iname "*$1*" -exec rename $1 $2 {} \;
}
alias findreplace=findreplace_function
alias renamereplace=renamereplace_function

bindkey -M viins '^r' history-incremental-search-backward
bindkey -M vicmd '^r' history-incremental-search-backward

source /etc/profile

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

eval "$(fasd --init auto)"
alias v='f -t -e vim -b viminfo'
alias j='fasd_cd -d'

export DC_DEV="yup"
export GIT_PS1_SHOWDIRTYSTATE="yup"
export GIT_PS1_SHOWUNTRACKEDFILES="yup"
export ETOWAH_WS="/srv/http/etowah"
