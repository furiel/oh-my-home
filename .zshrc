export _Z_NO_RESOLVE_SYMLINKS=1

export ZSH=$HOME/.oh-my-zsh

ZSH_THEME=fino-time
HYPHEN_INSENSITIVE="true"
plugins=(git)

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

# PROMPT='
# %{$fg[yellow]%}%n%{$reset_color%} at %{$fg[blue]%}%m%{$reset_color%} in %{$fg_bold[green]%}$(collapse_pwd)%{$reset_color%}$(git_prompt_info)
# $(virtualenv_info)$(prompt_char) '

# RPROMPT='%T'

# ZSH_THEME_GIT_PROMPT_PREFIX=" on %{$fg[magenta]%}"
# ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
# ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[green]%}!"
# ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg[green]%}?"
# ZSH_THEME_GIT_PROMPT_CLEAN=""

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

# commands to ignore
cmdignore=(htop tmux top vim)

# end and compare timer, notify-send if needed
function notifyosd-precmd() {
    if [ -z $ENABLE_LONG_COMMAND_NOTIFY ]; then
        return
    fi

    retval=$?
    if [[ ${cmdignore[(r)$cmd_basename]} == $cmd_basename ]]; then
        return
    else
        if [ ! -z "$cmd" ]; then
            cmd_end=`date +%s`
            ((cmd_time=$cmd_end - $cmd_start))
        fi
        if [ $retval -gt 0 ]; then
			cmdstat="with warning"
			sndstat="/usr/share/sounds/gnome/default/alerts/sonar.ogg"
		else
            cmdstat="successfully"
			sndstat="/usr/share/sounds/gnome/default/alerts/glass.ogg"
        fi
        if [ ! -z "$cmd" -a $cmd_time -gt ${LONG_COMMAND_NOTIFY_TIMEOUT:-60} ]; then
            if [ ! -z $SSH_TTY ] ; then
                notify-send -i utilities-terminal -u low "$cmd_basename on `hostname` completed $cmdstat" "\"$cmd\" took $cmd_time seconds"; #play -q $sndstat
            else
                notify-send -i utilities-terminal -u low "$cmd_basename completed $cmdstat" "\"$cmd\" took $cmd_time seconds"; #play -q $sndstat
            fi
        fi
        unset cmd
    fi
}

# make sure this plays nicely with any existing precmd
precmd_functions+=( notifyosd-precmd )

# get command name and start the timer
function notifyosd-preexec() {
    cmd=$1
    cmd_basename=${${cmd:s/sudo //}[(ws: :)1]}
    cmd_start=`date +%s`
}

# make sure this plays nicely with any existing preexec
preexec_functions+=( notifyosd-preexec )

export EDITOR="emacs"
export PATH=$PATH:/home/furiel/bin

