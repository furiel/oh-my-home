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

VISUAL="emacsclient -nw"
alias emnw='emacsclient -nw'
alias ssh-insecure='ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no'
alias emacs-eval='emacs -Q --batch --eval'
alias magit="emacs --eval \"(progn (require 'package) (package-initialize) (magit-status default-directory))\""
