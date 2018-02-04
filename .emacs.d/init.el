(setq inhibit-startup-screen t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

(use-package try
	:ensure t)

(use-package which-key
	:ensure t
	:config
	(which-key-mode))

(load-theme 'tango-dark)

(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(require 'wc-mode)
(wc-mode)

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

(setq make-backup-files nil)
(setq auto-save-interval 1000)
(electric-indent-mode -1)

(setq dired-listing-switches "-lXGh --group-directories-first")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-gtags-suggested-key-mapping t)
 '(show-paren-mode t)
 '(tramp-syntax (quote default) nil (tramp)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 158 :width normal)))))

(require 'org)
(require 'ob)
(setq org-src-fontify-natively t)

(let ((current-directory (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (format "%s/conf.d" current-directory)))
(load "version-control.el")
(load "development.el")
