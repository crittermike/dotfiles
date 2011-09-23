function collapse_pwd {
    echo $(pwd | sed -e "s,^$HOME,~,")
}

ZSH_THEME_GIT_PROMPT_PREFIX="%{$reset_color%}%{$fg[green]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg_bold[yellow]%} ⚡%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN=""

#PROMPT='%{$fg[cyan]%}$(collapse_pwd)%{$reset_color%}%{$fg[red]%}|%{$reset_color%}$(git_prompt_info)%{$fg_bold[magenta]%}⇒%{$reset_color%} '
PROMPT='%{$fg[cyan]%}%1~%{$reset_color%}%{$fg[red]%}|%{$reset_color%}$(git_prompt_info)%{$fg_bold[magenta]%}⇒%{$reset_color%} '
