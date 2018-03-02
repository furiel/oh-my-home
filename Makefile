install: emacs zsh

~/.oh-my-zsh:
	git clone https://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
	git clone https://github.com/zsh-users/zsh-autosuggestions.git ~/.zsh/zsh-autosuggestions
	echo "export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=4'" > ~/.oh-my-zsh/custom/my_customizations.zsh

~/z:
	git clone https://github.com/rupa/z ~/z

zsh: ~/.oh-my-zsh ~/z
	-mv ~/.zshrc ~/.zshrc.old
	ln -s `pwd`/.zshrc ~/.zshrc

emacs:
	-ln -s `pwd`/.emacs.d ~/.emacs.d
