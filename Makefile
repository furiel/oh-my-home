install: emacs zsh

~/.oh-my-zsh:
	git clone https://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh

~/z:
	git clone https://github.com/rupa/z ~/z

zsh: ~/.oh-my-zsh ~/z
	-mv ~/.zshrc ~/.zshrc.old
	ln -s `pwd`/.zshrc ~/.zshrc

emacs:
	-mv ~/.emacs ~/.emacs.old
	-mv ~/.emacs.d ~/.emacs.d.old
	ln -s `pwd`/.emacs ~/.emacs
	ln -s `pwd`/.emacs.d ~/.emacs.d
	emacs -Q --batch --script install_packages.el
