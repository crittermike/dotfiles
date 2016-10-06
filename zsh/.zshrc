# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh
export VIMCLOJURE_SERVER_JAR="$HOME/Misc/server-2.3.1.jar"

ZSH_THEME="flarp"

source $ZSH/oh-my-zsh.sh

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
alias ack='ack --literal --smart-case'
alias up='sudo packer -Syu'
alias c='clear'
alias feh='feh -ZF'
alias ll='ls -lah'
alias e='dtrx'
alias ag='ag -a'
alias dlgit='drush pm-download `basename "$PWD"` --package-handler=git_drupalorg --select --destination=`dirname \`pwd\`` && cd ../`basename "$PWD"` && open http://drupal.org/node/add/project-issue/`basename "$PWD"`'

#alias aggr_dis='drush vset preprocess_css 0 --yes && drush vset preprocess_js 0 --yes && drush cc css-js'
#alias aggr_en='drush vset preprocess_css 1 --yes && drush vset preprocess_js 1 --yes && drush cc css-js'

bindkey -M viins '^r' history-incremental-search-backward
bindkey -M vicmd '^r' history-incremental-search-backward

source /etc/profile

eval "$(fasd --init auto)"
alias v='f -t -e vim -b viminfo'
alias j='fasd_cd -d'

export GIT_PS1_SHOWDIRTYSTATE="yup"
export GIT_PS1_SHOWUNTRACKEDFILES="yup"

#export PATH=$PATH:/Applications/MAMP/Library/bin
#export PATH=/Applications/MAMP/bin/php/php5.5.3/bin:$PATH
export PATH="$PATH:$HOME/.composer/vendor/bin"

eval "$(rbenv init -)"

mlsmerge_function() {
  git checkout release && git up && git merge origin/feature/mlsweb-$1 && git push && git checkout develop && git merge release && git push && git checkout release
}

alias mlsmerge=mlsmerge_function

eval "$(devtools config)"
