;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Dylan Hardison"
      user-mail-address "dylan@hardison.net")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "SauceCodePro Nerd Font Mono" :size 14))
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dark+)

;; (setq doom-dark+-blue-modeline nil)

(setq +doom-dashboard-ascii-banner-fn (lambda () ))

(setq doom-modeline-unicode-fallback nil)
(setq xterm-extra-capabilities '(reportBackground))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(load! "perltidy.el")
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(defalias 'perl-mode 'cperl-mode)

(defun my-cperl-mode ()
  (cperl-set-style "Whitesmith"))

(after! cperl-mode
  (define-key cperl-mode-map "{" nil)
  (setq cperl-highlight-variables-indiscriminately t)

  (add-hook 'cperl-mode-hook #'my-cperl-mode))

;; For python
(add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
;; For perl
(add-hook 'cperl-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
;; For ruby
(add-hook 'ruby-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
;; For Javascript
(add-hook 'js2-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
;; for C and related
(add-hook 'c-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

(add-hook 'rust-mode #'(lambda () (modify-syntax-entry ?_ "w")))

(add-hook 'ruby-mode-hook 'evil-ruby-text-objects-mode)

(setq lsp-rust-server 'rust-analyzer)
(setq lsp-inlay-hint-enable t)
(setq lsp-rust-analyzer-display-parameter-hints t)
(setq lsp-rust-analyzer-display-closure-return-type-hints t)
(setq lsp-inlay-hint-enable t)
(setq confirm-kill-emacs #'yes-or-no-p)
(setq lsp-copilot-enabled nil)
(setq lsp-copilot-applicable-fn (lambda (&rest _) nil))

(map! :leader :desc "show link to github" :n "g h l" #'git-link)
(map! :leader :n "p e" #'projectile-switch-vterm)

(use-package! json-mode
  :mode ("\\.hujson\\'" . jsonc-mode))

(use-package! web-mode
  :mode ("\\.tt" . web-mode))

(add-to-list 'auto-mode-alist '("\\.t\\'" . cperl-mode))

(setq +format-on-save-enabled-modes
      '(not json-mode jsonc-mode))

(setq projectile-project-search-path
      '(("~/Git" . 2)))

(setq frame-title-format
      '("[" (:eval system-name) "] "
        "emacs %b"))

(setq bookmark-default-file "~/.doom.d/bookmarks")
(setq bookmark-save-flag 1)

;; Global Auto-Revert Mode is a global minor mode that reverts any
;; buffer associated with a file when the file changes on disk.  Use
;; auto-revert-mode to revert a particular buffer.
(global-auto-revert-mode 1)

;; also enable this for non-file buffers, like dired
(setq global-auto-revert-non-file-buffers t)

(defun projectile-switch-vterm ()
  "Switch to a project and open a vterm"
  (interactive)
  (counsel-projectile-switch-project #'counsel-projectile-switch-project-action-run-vterm))

(defun eshell/emacs (file)
  (find-file file))

(defun eshell/vi (file)
  (find-file file))

(defun eshell/vim (file)
  (find-file file))

(after! counsel-projectile
  (setq counsel-projectile-switch-project-action
        '(16
          ("o" counsel-projectile-switch-project-action
           "jump to a project buffer or file")
          ("f" counsel-projectile-switch-project-action-find-file
           "jump to a project file")
          ("d" counsel-projectile-switch-project-action-find-dir
           "jump to a project directory")
          ("D" counsel-projectile-switch-project-action-dired
           "open project in dired")
          ("b" counsel-projectile-switch-project-action-switch-to-buffer
           "jump to a project buffer")
          ("m" counsel-projectile-switch-project-action-find-file-manually
           "find file manually from project root")
          ("S" counsel-projectile-switch-project-action-save-all-buffers
           "save all project buffers")
          ("k" counsel-projectile-switch-project-action-kill-buffers
           "kill all project buffers")
          ("K" counsel-projectile-switch-project-action-remove-known-project
           "remove project from known projects")
          ("c" counsel-projectile-switch-project-action-compile
           "run project compilation command")
          ("C" counsel-projectile-switch-project-action-configure
           "run project configure command")
          ("E" counsel-projectile-switch-project-action-edit-dir-locals
           "edit project dir-locals")
          ("v" counsel-projectile-switch-project-action-vc
           "open project in vc-dir / magit / monky")
          ("s" counsel-projectile-switch-project-action-rg
           "search project with rg")
          ("xs" counsel-projectile-switch-project-action-run-shell
           "invoke shell from project root")
          ("xe" counsel-projectile-switch-project-action-run-eshell
           "invoke eshell from project root")
          ("xt" counsel-projectile-switch-project-action-run-term
           "invoke term from project root")
          ("xv" counsel-projectile-switch-project-action-run-vterm
           "invoke vterm from project root")
          ("Oc" counsel-projectile-switch-project-action-org-capture
           "capture into project")
          ("Oa" counsel-projectile-switch-project-action-org-agenda
           "open project agenda"))))

(setq +doom-dashboard-menu-sections
      '(
        ("Recently opened files"
         :icon (nerd-icons-faicon "nf-fa-file_text" :face 'doom-dashboard-menu-title)
         :action recentf-open-files)
        ("Reload last session"
         :icon (nerd-icons-octicon "nf-oct-history" :face 'doom-dashboard-menu-title)
         :when (cond ((modulep! :ui workspaces) (file-exists-p (expand-file-name persp-auto-save-fname persp-save-dir))) ((require 'desktop nil t) (file-exists-p (desktop-full-file-name))))
         :action doom/quickload-session)
        ("Open org-agenda"
         :icon (nerd-icons-octicon "nf-oct-calendar" :face 'doom-dashboard-menu-title)
         :when (fboundp 'org-agenda)
         :action org-agenda)
        ("Open project"
         :icon (nerd-icons-octicon "nf-oct-briefcase" :face 'doom-dashboard-menu-title)
         :action projectile-switch-project)
        ("Open terminal"
         :icon (nerd-icons-octicon "nf-oct-terminal" :face 'doom-dashboard-menu-title)
         :action projectile-switch-vterm)
        ;; +vterm/here)
        ("Jump to bookmark"
         :icon (nerd-icons-octicon "nf-oct-bookmark" :face 'doom-dashboard-menu-title)
         :action bookmark-jump)
        ("Open private configuration" :icon (nerd-icons-octicon "nf-oct-tools" :face 'doom-dashboard-menu-title)
         :when (file-directory-p doom-user-dir)
         :action doom/open-private-config)
        ("Open documentation"
         :icon (nerd-icons-octicon "nf-oct-book"     :face 'doom-dashboard-menu-title)
         :action doom/help)
        )))
