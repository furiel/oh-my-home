(setq package-enable-at-startup nil)

(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)

(setq make-backup-files nil)
(setq auto-save-interval 1000)
(electric-indent-mode -1)

(setq c-default-style "linux"
      c-basic-offset 4)

(setq dired-listing-switches "-lXGh --group-directories-first")

(require 'color-theme)
(color-theme-initialize)
(color-theme-hober)

(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(require 'wc-mode)
(wc-mode)

(require 'find-file-in-project)
(global-set-key (kbd "C-x f") 'find-file-in-project)

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

(require 'yasnippet)
(yas-global-mode 1)

; Auto complete c headers
(defun custom:ac-c-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers))
(add-hook 'c++-mode-hook 'custom:ac-c-header-init)
(add-hook 'c-mode-hook 'custom:ac-c-header-init)

; iedit
(define-key global-map (kbd "C-c ;") 'iedit-mode)

; Turn on semantic mode
(semantic-mode 1)
(defun custom:add-semantic-to-auto-complete ()
  (add-to-list 'ac-sources 'ac-source-semantic))
(add-hook 'c-mode-common-hook 'custom:add-semantic-to-auto-complete)
(global-semantic-idle-scheduler-mode 1)

(add-hook 'before-save-hook 'whitespace-cleanup)

(setq inhibit-startup-screen t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (iedit yaml-mode wc-mode magit helm find-file-in-project color-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(define-minor-mode linux-kernel-mode
  "Toggle linux-kernel mode."
 ;; The initial value.
 nil
 ;; The indicator for the mode line.
 " linux-kernel"
 ;; The minor mode bindings.
 nil
 :group 'linux-kernel
 (progn
   (setq indent-tabs-mode t)
   (c-set-style "linux")
   )
 )
