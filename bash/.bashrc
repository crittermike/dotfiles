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

alias aggr_dis='drush vset preprocess_css 0 --yes && drush vset preprocess_js 0 --yes && drush cc css-js'
alias aggr_en='drush vset preprocess_css 1 --yes && drush vset preprocess_js 1 --yes && drush cc css-js'

mlsmerge_function() {
  git checkout release && git up && git merge origin/feature/mlsweb-$1 && git push && git checkout develop && git merge release && git push && git checkout release
}

alias mlsmerge=mlsmerge_function
