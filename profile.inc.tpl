__emacs_bin="emacs"
__emacsclient_bin="emacsclient"

alias emnw="${__emacsclient_bin} -nw"
alias emacs-eval="$__emacs_bin -Q --batch --eval"
alias magit="$__emacs_bin --eval \"(progn (require 'package) (package-initialize) (magit-status default-directory))\""

alias ssh-insecure='ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no'

export VISUAL="${__emacsclient_bin} -nw"
export GOPATH=~/gopath
export GOROOT=/usr/lib64/go
export PATH=$PATH:~/gopath/bin

unset __emacs_bin
unset __emacsclient_bin
