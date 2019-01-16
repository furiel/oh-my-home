(setq inhibit-startup-screen t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-gtags-prefix-key "")
 '(helm-gtags-suggested-key-mapping t)
 '(magit-commit-arguments nil)
 '(show-paren-mode t))
 '(package-archive-upload-base "/path/to/archive")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 158 :width normal)))))
(put 'narrow-to-region 'disabled nil)
