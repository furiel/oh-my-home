(setq inhibit-startup-screen t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("bagolyodu" . "https://bagolyodu.dyndns.hu/emacs-packages/") t)
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

(let ((current-directory (file-name-directory (or load-file-name buffer-file-name))))
  (org-babel-load-file (expand-file-name (format "%s/config.org" current-directory))))

(setq custom-file "~/.emacs.d/customizations.el")
(load custom-file)
