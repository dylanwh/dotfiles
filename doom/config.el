;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;;

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Dylan Hardison"
      user-mail-address "dylan@hardison.net")

;; for some reason emacs doesn't understand the TZ env I set.
(set-time-zone-rule "PST8PDT")

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
(setq doom-font (font-spec :family "SauceCodePro Nerd Font Mono" :style "Light" :size 14.0))
(setq doom-big-font (font-spec :family "SauceCodePro Nerd Font Mono" :style "Light" :size 28.0))
(setq nerd-icons-font-family "SauceCodePro Nerd Font Mono")

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

;; Org-mode configuration
(load! "org-links.el")

(with-eval-after-load 'org
  (setq org-capture-templates
        (cl-remove "l" org-capture-templates :key #'car :test #'string=))
  (add-to-list 'org-capture-templates
               '("l" "Link" entry
                 (file+headline "~/org/links.org" "Inbox")
                 (function org-links-capture-template)) t)
  ;; Log timestamp when a task is marked DONE
  (setq org-log-done 'time))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(load! "perltidy.el")
(load! "ssh-utils.el")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `with-eval-after-load' block, otherwise Doom's defaults may override your
;; settings. E.g.
;;
;;   (with-eval-after-load 'PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look them up).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(defalias 'perl-mode 'cperl-mode)

(defun my-cperl-mode ()
  (cperl-set-style "Whitesmith"))

(with-eval-after-load 'cperl-mode
  (define-key cperl-mode-map "{" nil)
  (setq cperl-highlight-variables-indiscriminately t)

  (add-hook 'cperl-mode-hook #'my-cperl-mode))

;; emacs doesn't typically consider _ to be a word character, which is annoying
;; in several languages. Modify those modes syntax entries to consider _ a word
;; character
(let ((wordy-modes '(python-mode-hook cperl-mode-hook ruby-mode-hook
                     js2-mode-hook c-mode-hook rust-mode))
      (hook #'(lambda () (modify-syntax-entry ?_ "w"))))
  (dolist (mode wordy-modes)
    (add-hook mode hook)))

(add-hook 'ruby-mode-hook 'evil-ruby-text-objects-mode)

(setq lsp-copilot-applicable-fn (lambda (&rest _) nil))
(setq lsp-copilot-enabled nil)

(setq lsp-inlay-hint-enable t)
(setq lsp-rust-analyzer-display-closure-return-type-hints t)
(setq lsp-rust-analyzer-display-parameter-hints t)
(setq lsp-rust-server 'rust-analyzer)
(setq confirm-kill-emacs #'yes-or-no-p)

(map! :leader :desc "show link to github" :n "g h l" #'git-link)
(map! :leader :n "p v" #'projectile-switch-vterm)
(map! :leader :n "g %" #'magit-worktree)
(map! :leader :n "g P" #'magit-push-current-to-upstream)

(defun project-shelldon-async-command ()
  "Run `shelldon-async-command' in the current project's root directory."
  (declare (interactive-only shelldon-async-command))
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (call-interactively #'shelldon-async-command)))

(defun project-shelldon ()
  "Run `shell-command' in the current project's root directory."
  (declare (interactive-only shelldon))
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (call-interactively #'shelldon)))

(add-to-list 'display-buffer-alist
             '("*shelldon:"
               (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window display-buffer-pop-up-window)
               (side . right)
               (slot . 0)
               (window-width . 80)))

(map! :ni "M-&" #'project-shelldon-async-command)
(map! :ni "M-!" #'project-shelldon)

(defun my/configure-opener ()
  "Configure SSH for opener."
  (interactive)
  (shelldon-async-command "echo \"StreamLocalBindUnlink yes\" | sudo tee /etc/ssh/sshd_config.d/opener.conf"))

(defun my/smart-rebuild ()
  "Run smart-rebuild."
  (interactive)
  (shelldon-async-command "smart-rebuild"))

(defun my/doom-sync ()
  "Run doom sync with AOT compilation and GC."
  (interactive)
  (shelldon-async-command "doom sync --aot -j 4 --gc"))

(defun my/ssh-add ()
  "Add SSH keys to agent."
  (interactive)
  (let ((key-files '("~/.ssh/id_ed25519" "~/.ssh/id_rsa")))
    (cond
     ((eq system-type 'darwin)
      (call-process "ssh-add" nil nil nil "-q" "--apple-load-keychain")
      (dolist (file key-files)
        (when (file-exists-p (expand-file-name file))
          (call-process "ssh-add" nil nil nil "--apple-use-keychain" (expand-file-name file)))))
     (t
      (unless (zerop (call-process "ssh-add" nil nil nil "-q"))
        (dolist (file key-files)
          (when (file-exists-p (expand-file-name file))
            (call-process "ssh-add" nil nil nil (expand-file-name file)))))))
    (message "SSH keys added")))

(map! :leader
      :prefix ("a" . "actions")
      :desc "Agent shell" "a" #'agent-shell
      :desc "Configure opener" "o" #'my/configure-opener
      :desc "Smart rebuild" "r" #'my/smart-rebuild
      :desc "Doom sync" "d" #'my/doom-sync
      :desc "SSH add keys" "S" #'my/ssh-add
      :desc "SSH terminal" "s" #'ssh-terminal
      :desc "Terminal here" "t" #'terminal-here)

(defun gib (n)
  (* n 1024 1024 1024))

(defun mib (n)
  (* n 1024 1024))

(with-eval-after-load 'gcmh
  (setq gcmh-high-cons-threshold (gib 1))
  (setq gcmh-low-cons-threshold  (mib 300)))

(use-package json-mode
  :mode ("\\.hujson\\'" . jsonc-mode))

(use-package kdl-mode
  :mode ("\\.kdl\\'" . kdl-mode))

(use-package web-mode
  :mode ("\\.tt" . web-mode))

(add-to-list 'auto-mode-alist '("\\.t\\'" . cperl-mode))

(setq +format-on-save-enabled-modes
      '(not json-mode jsonc-mode fish-mode))

(setq projectile-project-search-path
      '(("~/Git" . 2)))

(setq frame-title-format
      '((:eval (if (getenv "SSH_CONNECTION") (concat "[" system-name "] ")))
        "emacs %b"))

(setq bookmark-default-file "~/.doom.d/bookmarks")
(setq bookmark-save-flag 1)

(setq recentf-max-saved-items 400)

;; Global Auto-Revert Mode is a global minor mode that reverts any
;; buffer associated with a file when the file changes on disk.  Use
;; auto-revert-mode to revert a particular buffer.
(global-auto-revert-mode 1)

;; also enable this for non-file buffers, like dired
(setq global-auto-revert-non-file-buffers t)

(menu-bar-mode -1)

(defun projectile-switch-vterm ()
  "Switch to a project and open a vterm"
  (interactive)
  (counsel-projectile-switch-project #'counsel-projectile-switch-project-action-run-vterm))

(with-eval-after-load 'eshell
  (defun my-eshell-mode-company ()
    (setq-local company-idle-delay nil))
  (add-hook 'eshell-mode-hook 'my-eshell-mode-company)

  (defun eshell/emacs (file)
    (find-file file))

  (defun eshell/vi (file)
    (find-file file))

  (defun eshell/vim (file)
    (find-file file))

  (defun eshell/pp ()
    (counsel-projectile-switch-project #'counsel-projectile-switch-project-action-run-eshell))


  (set-eshell-alias!
   "home" "cd ~/Git/dylanwh/dotfiles"
   "pull" "git pull"
   "push" "git push")


  (add-hook! 'eshell-directory-change-hook
    (company-mode (if (file-remote-p default-directory) -1 +1))))

(with-eval-after-load 'counsel-projectile
  (setq counsel-projectile-switch-project-action
        '(1
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
      '(("Recently opened files"
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
         :action doom/help)))

;; for some reason, emacsformacosx has scratch buffer not being in lisp-interactive-mode?
(defun fix-scratch-buffer (&rest _)
  (when (get-buffer "*scratch*")
    (with-current-buffer "*scratch*"
      (unless (eq major-mode 'lisp-interaction-mode)
        (lisp-interaction-mode)))))

(advice-add 'scratch-buffer :after #'fix-scratch-buffer)

(if (version< emacs-version "30.1")
    (setq tramp-use-ssh-controlmaster-options t)
  (setq tramp-use-connection-share t))


(with-eval-after-load 'evil
  (evil-ex-define-cmd "q" 'bury-buffer)
  (evil-ex-define-cmd "wq" 'doom/save-and-kill-buffer))

(use-package age
  :custom
  (age-program "rage")
  (age-default-identity
   (cl-find-if #'file-readable-p
               '( "~/.ssh/id_ed25519" "~/.ssh/id_rsa")))
  (age-default-recipient
   '("~/.ssh/authorized_keys"))
  :config
  (setenv "PINENTRY_PROGRAM" "")
  (age-file-enable))

(setq shell-file-name (executable-find "bash"))

(let ((fish (executable-find "fish")))
  (setq-default vterm-shell fish)
  (setq-default explicit-shell-file-name fish))

(defun treesit-show-parser-used-at-point ()
  "Shows treesit parser used at point."
  (interactive)
  (if (and (fboundp 'treesit-available-p)
           (treesit-available-p))
      (message (format "%s" (treesit-language-at (point))))
    (message "treesit is not available")))

(use-package agent-shell
  :config
  (setq agent-shell-show-welcome-message nil)
  (setq agent-shell-header-style nil)

  (setq agent-shell-anthropic-claude-environment
        (agent-shell-make-environment-variables :inherit-env t))

  (add-hook! 'diff-mode-hook
    (when (string-match-p "\\*agent-shell-diff\\*" (buffer-name))
      (evil-emacs-state))))

(with-eval-after-load 'agent-shell
  (map! :map agent-shell-mode-map
        :i "RET" #'newline
        :n "RET" #'comint-send-input))

;; Load local configuration if it exists
(let ((local-config (expand-file-name "local-config.el" doom-user-dir)))
  (when (file-exists-p local-config)
    (load local-config)))


(setq code-review-auth-login-marker 'forge)

(defun my/browse-url-remote-opener (url &optional _new-window)
  (let ((socket-path (expand-file-name "~/.opener.sock")))
    (condition-case nil
        (let ((proc (make-network-process
                     :name "opener-client"
                     :family 'local
                     :service socket-path
                     :buffer nil
                     :noquery t)))
          (process-send-string proc (concat url "\n"))
          (delete-process proc)
          (message "opener: %s" url))
      
      (error (message "opener not available, url: %s" url)))))



;; Set it as the default browser for Emacs
(setq browse-url-browser-function 'my/browse-url-remote-opener)

(use-package terminal-here
  :custom
  (terminal-here-command-flag "--")
  (terminal-here-terminal-command 
   (lambda (dir)
     "Return a kitty launcher function appropriate for the current OS."
     (let ((cwd (or dir default-directory))
           (listen (concat "unix:" (car (file-expand-wildcards (expand-file-name "~/.kitty.sock-*"))))))
       (list "kitty" "@"
             "--to" listen 
             "launch" "--type=os-window"
             "--cwd" cwd))))
  :config
  (advice-add 'terminal-here--maybe-add-mac-os-open :override
              (lambda (terminal-command)
                (if (string-suffix-p ".app" (car terminal-command))
                    (append (list "open" "-a" (car terminal-command) "." "--args")
                            (cdr terminal-command))
                  terminal-command))))

(setq
 mue4e-headers-skip-duplicates  t
 mu4e-view-show-images t
 mu4e-view-show-addresses t
 mu4e-compose-format-flowed nil
 ;; mu4e-date-format "%y/%m/%d"
 mu4e-headers-date-format "%Y/%m/%d"
 mu4e-change-filenames-when-moving t
 mu4e-attachments-dir "~/Downloads"

 ;; note that these folders below must start with /
 ;; the paths are relative to maildir root
 mu4e-refile-folder "/Archive"
 mu4e-sent-folder   "/Sent Items"
 mu4e-drafts-folder "/Drafts"
 mu4e-trash-folder  "/Trash"

 mu4e-headers-hide-predicate
 (lambda (msg)
   (member 'trashed (mu4e-message-field msg :flags)))
 
 mu4e-bookmarks
 (let ((exclude (concat "AND NOT \"maildir:/Junk Mail\""
                        " AND NOT maildir:/Trash"
                        " AND NOT maildir:/Bugzilla"
                        " AND NOT maildir:/+SaneBlackhole"
                        " AND NOT maildir:/GMail")))
   `((:name "Unread messages"
      :query ,(concat "flag:unread " exclude)
      :key ?u)
     (:name "Today's messages"
      :query ,(concat "date:today..now " exclude)
      :key ?t)
     (:name "Last 7 days"
      :query ,(concat "date:7d..now "
                      "AND NOT \"maildir:/Junk Mail\" "
                      "AND NOT maildir:/Trash "
                      "AND NOT maildir:/+SaneBlackhole")
      :hide-unread t
      :key ?w)
     (:name "Messages with images"
      :query "mime:image/* AND NOT maildir:/+SaneBlackhole"
      :key ?p)
     (:name "Flagged messages"
      :query ,(concat "flag:flagged "
                      "AND NOT \"maildir:/Junk Mail\" "
                      "AND NOT maildir:/Trash "
                      "AND NOT maildir:/+SaneBlackhole")
      :key ?f)))

 mu4e-maildir-shortcuts
 '((:maildir "/INBOX"     :key ?i)
   (:maildir "/SaneLater" :key ?l :hide-if-no-unread t)
   (:maildir "/SaneNews"  :key ?n :hide-if-no-unread t)
   (:maildir "/Archive"   :key ?a :hide-if-no-unread t)
   (:maildir "/Billing"   :key ?b :hide-if-no-unread t)
   (:maildir "/Bugzilla"  :key ?z :hide-if-no-unread t)
   (:maildir "/Junk Mail" :key ?j)
   (:maildir "/Trash"     :key ?t)))

(map! :leader :desc "Elfeed" :n "o n" #'elfeed)
(with-eval-after-load 'elfeed
  (map! :after elfeed
        :map elfeed-search-mode-map
        :leader :desc "Elfeed update" :n "o u" #'elfeed-update)

  (defvar my/elfeed-ignored-tags '(elfeed unread starred)
    "Tags to exclude from the elfeed tag filter list.")

  (defun my/elfeed-db-tags ()
    "Return all unique tags from the elfeed database, excluding `my/elfeed-ignored-tags'."
    (let ((tags (make-hash-table :test 'eq)))
      (with-elfeed-db-visit (entry _feed)
                            (dolist (tag (elfeed-entry-tags entry))
                              (unless (memq tag my/elfeed-ignored-tags)
                                (puthash tag t tags))))
      (hash-table-keys tags)))

  (defun my/elfeed-unread-count (tag)
    "Return the number of unread elfeed entries with TAG."
    (let ((count 0))
      (with-elfeed-db-visit (entry _feed)
                            (when (and (memq 'unread (elfeed-entry-tags entry))
                                       (memq tag (elfeed-entry-tags entry)))
                              (cl-incf count)))
      count))

  (defun my/elfeed-tag-filter ()
    "Select an elfeed tag filter via completing-read."
    (interactive)
    (let* ((tags (my/elfeed-db-tags))
           (candidates
            (cl-loop for tag in (sort tags (lambda (a b) (string< (symbol-name a) (symbol-name b))))
                     for count = (my/elfeed-unread-count tag)
                     when (> count 0)
                     collect (cons (format "%s (%d)" (symbol-name tag) count)
                                   tag)))
           (candidates (cons '("All unread" . nil) candidates)))
      (if (= (length candidates) 1)
          (message "No unread entries.")
        (let* ((choice (completing-read "Elfeed filter: " (mapcar #'car candidates) nil t))
               (tag (cdr (assoc choice candidates))))
          (elfeed-search-set-filter
           (if tag
               (format "+unread +%s" (symbol-name tag))
             "+unread"))))))

  (map! :map elfeed-search-mode-map
        :n "T" #'my/elfeed-tag-filter))

(require 'server)
(unless (server-running-p)
  (server-start))
