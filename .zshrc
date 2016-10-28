export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="robbyrussell"
HYPHEN_INSENSITIVE="true"
plugins=(git)

source $ZSH/oh-my-zsh.sh

autoload -U promptinit
promptinit
prompt adam2

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
