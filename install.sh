#!/bin/bash
if [ ! -d ~/.oh-my-home ]; then
    git clone https://github.com/furiel/oh-my-home ~/.oh-my-home
    cd ~/.oh-my-home
else
    cd ~/.oh-my-home
    git pull
fi

make
