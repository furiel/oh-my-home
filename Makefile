install: emacs zsh

~/.oh-my-zsh:
	git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh

zsh: ~/.oh-my-zsh
	-mv ~/.zshrc ~/.zshrc.old
	ln -s `pwd`/.zshrc ~/.zshrc

emacs:
	-mv ~/.emacs ~/.emacs.old
	-mv ~/.emacs.d ~/.emacs.d.old
	ln -s `pwd`/.emacs ~/.emacs
	ln -s `pwd`/.emacs.d ~/.emacs.d
	emacs -Q --batch --script install_packages.el
