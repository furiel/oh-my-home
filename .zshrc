export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="robbyrussell"
HYPHEN_INSENSITIVE="true"
plugins=(git)

source $ZSH/oh-my-zsh.sh

autoload -U promptinit
promptinit
prompt clint
export PROMPT="""%F{red}[%F{cyan}%D{%a %y/%m/%d %R %Z}%F{red}]%F{red}[%F{green}%l%F{red}]%F{red}[%F{cyan}x86_64/linux-gnu/4.4.0-45-generic%F{red}]%F{red}[%F{cyan}5.1.1%F{red}]
%F{red}<%F{green}%n@%m%F{white}:%F{yellow}%~%F{red}>%f """


. $HOME/z/z.sh

autoload -z edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line

setopt extended_glob

export VISUAL="emacsclient-snapshot -nw"
export GOPATH=~/gopath

alias emacs='emacs-snapshot'
alias emacsclient='emacsclient-snapshot'
alias emnw='emacsclient-snapshot -nw'
alias ssh-insecure='ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no'
alias emacs-eval='emacs-snapshot -Q --batch --eval'
alias magit="emacs-snapshot --eval \"(progn (require 'package) (package-initialize) (magit-status default-directory))\""

function find-contain {
        pw="$1"
        shift
        echo "*$@*"
        find "$pw" -name "*$@*"
}
