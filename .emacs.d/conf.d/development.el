(require 'yasnippet)
(yas-global-mode 1)

(setq c-default-style "linux" c-basic-offset 4)

; iedit
(define-key global-map (kbd "C-c ;") 'iedit-mode)

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

(show-paren-mode 1)

; Turn on semantic mode
(semantic-mode 1)
(defun custom:add-semantic-to-auto-complete ()
  (add-to-list 'ac-sources 'ac-source-semantic))
(add-hook 'c-mode-common-hook 'custom:add-semantic-to-auto-complete)
(global-semantic-idle-scheduler-mode 1)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun balabit-c-mode-hooks ()
  (let ((bname (buffer-file-name)))
    (cond
     ((string-match ".*syslog.*" bname) (c-set-style "gnu") ))))

(add-hook 'c-mode-hook 'balabit-c-mode-hooks)
(add-hook 'c-mode-hook 'helm-gtags-mode)

(setq godef-command "/home/furiel/gopath/bin/godef")

(setq-default indent-tabs-mode nil)

(define-key global-map (kbd "<f7>") '(lambda () (interactive) (recompile)))

(add-to-list 'auto-mode-alist '("\\.zts\\'" . python-mode))

(setq compilation-scroll-output 'first-error)
