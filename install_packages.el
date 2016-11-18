(require 'cl)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(package-refresh-contents)

(flet ((package-install-if-not-exists (pkg)
	 (unless (require pkg nil 'noerror)
	   (package-install pkg))))
  (package-install-if-not-exists 'helm)
  (package-install-if-not-exists 'wc-mode)
  (package-install-if-not-exists 'color-theme)
  (package-install-if-not-exists 'magit)
  (package-install-if-not-exists 'find-file-in-project)
  (package-install-if-not-exists 'yaml-mode)
  )
