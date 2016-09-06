(setq package-enable-at-startup nil)
(package-initialize)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(setq make-backup-files nil)
(setq auto-save-interval 1000)
(electric-indent-mode -1)

(require 'color-theme)
(color-theme-initialize)
(color-theme-hober)

(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(require 'wc-mode)
(wc-mode)

(setq inhibit-startup-screen t)
