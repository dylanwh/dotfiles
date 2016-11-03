(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" .  "http://elpa.gnu.org/packages/"))
      package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (unless (assoc 'use-package package-archive-contents)
    (package-refresh-contents))
  (package-install 'use-package))

(require 'use-package)
(use-package org :ensure t)
(org-babel-load-file "~/.emacs.d/config.org")
