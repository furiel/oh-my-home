(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(package-refresh-contents)

(package-install 'helm)
(package-install 'wc-mode)
(package-install 'color-theme)
