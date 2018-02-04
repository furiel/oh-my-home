export ZSH=$HOME/.oh-my-zsh

HYPHEN_INSENSITIVE="true"
plugins=(git zsh-autosuggestions)

source $ZSH/oh-my-zsh.sh

function collapse_pwd {
    echo $(pwd | sed -e "s,^$HOME,~,")
}

function prompt_char {
    git branch >/dev/null 2>/dev/null && echo '$' && return
    echo '○'
}

function virtualenv_info {
    [ $VIRTUAL_ENV ] && echo '('`basename $VIRTUAL_ENV`') '
}

PROMPT='
%{$fg[yellow]%}%n%{$reset_color%} at %{$fg[blue]%}%m%{$reset_color%} in %{$fg_bold[green]%}$(collapse_pwd)%{$reset_color%}$(git_prompt_info)
$(virtualenv_info)$(prompt_char) '

RPROMPT='%T'

ZSH_THEME_GIT_PROMPT_PREFIX=" on %{$fg[magenta]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[green]%}!"
ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg[green]%}?"
ZSH_THEME_GIT_PROMPT_CLEAN=""

. $HOME/z/z.sh

autoload -z edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line

setopt extended_glob

. $HOME/.oh-my-home/profile.inc

alias ls="ls --color=tty --group-directories-first"

function find-contain {
        pw="$1"
        shift
        echo "*$@*"
        find "$pw" -name "*$@*"
}
