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

(defvar my/host-theme-alist
  '()
  "Alist mapping hostnames to theme names.") 

(with-eval-after-load 'vterm
  (remove-hook 'vterm-mode-hook #'hide-mode-line-mode))

(with-eval-after-load 'eshell
  (remove-hook 'eshell-mode-hook #'hide-mode-line-mode))

(with-eval-after-load 'shell
  (remove-hook 'shell-mode-hook #'hide-mode-line-mode))

(setq +doom-dashboard-ascii-banner-fn (lambda () ))

(setq doom-modeline-unicode-fallback nil)
(setq xterm-extra-capabilities '(reportBackground))

;; I like to have plenty of time to read a message
(setq minibuffer-message-timeout 60)

(setq initial-buffer-choice (lambda () (doom/switch-to-project-scratch-buffer)))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Org-mode configuration
(load! "org-links.el")
(load! "ssh-utils.el")
(load! "shellfish.el")

(with-eval-after-load 'org
  (require 'org-protocol)
  ;; Remove before adding to avoid duplicates on re-eval.
  (setq org-capture-templates
        (cl-remove "l" org-capture-templates :key #'car :test #'string=))
  (setq org-capture-templates
        (cl-remove "L" org-capture-templates :key #'car :test #'string=))
  (add-to-list 'org-capture-templates
               '("l" "Link" entry
                 (file+headline "~/org/links.org" "Inbox")
                 (function org-links-capture-template)) t)
  (add-to-list 'org-capture-templates
               '("L" "Link (protocol)" entry
                 (file+headline "~/org/links.org" "Inbox")
                 (function org-links-protocol-capture-template)
                 :immediate-finish t) t)
  ;; Log timestamp when a task is marked DONE
  (setq org-log-done 'time))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(load! "perltidy.el")

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

(add-hook 'eglot-managed-mode-hook #'eglot-inlay-hints-mode)
(setq-default eglot-workspace-configuration
              '(:rust-analyzer (:inlayHints (:closureReturnTypeHints (:enable "always")
                                             :parameterHints (:enable t)))))

(use-package eglot-x
  :after eglot
  :config
  (eglot-x-setup))

(after! eglot-x
  ;; Enhanced reference finding (supports ccls, rust-analyzer extended methods)
  (map! :leader
        :desc "Find references (extended)" "c ." #'eglot-x-find-refs)

  ;; Rust-analyzer keybindings
  (map! :map rustic-mode-map
        :localleader
        (:prefix ("r" . "rust")
         :desc "Expand macro"             "e" #'eglot-x-expand-macro
         :desc "View memory layout"       "m" #'eglot-x-view-recursive-memory-layout
         :desc "Related tests"            "t" #'eglot-x-ask-related-tests
         :desc "Reload workspace"         "r" #'eglot-x-reload-workspace
         :desc "View crate graph"         "g" #'eglot-x-view-crate-graph
         :desc "Find crate"               "f" #'eglot-x-find-crate
         :desc "Analyzer status"          "s" #'eglot-x-analyzer-status
         :desc "View syntax tree"         "S" #'eglot-x-view-syntax-tree))

  ;; Additional rust-analyzer commands available:
  ;; - eglot-x-rebuild-proc-macros
  ;; - eglot-x-view-hir / eglot-x-view-mir
  ;; - eglot-x-interpret-function

  ;; Available Taplo (TOML) commands:
  ;; - eglot-x-taplo-show-associated-schema
  ;; - eglot-x-taplo-find-associated-schema
  ;; - eglot-x-taplo-list-schemas
  )

(setq confirm-kill-emacs #'yes-or-no-p)

(map! :leader :desc "GitHub link" :n "g h l" #'git-link)
(map! :leader :desc "Magit worktree" :n "g %" #'magit-worktree)
(map! :leader :desc "Magit push" :n "g P" #'magit-push-current-to-upstream)

(autoload 'shelldon "shelldon" nil t)
(autoload 'shelldon-async-command "shelldon" nil t)
(autoload 'shelldon-output-history "shelldon" nil t)

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

(defun my/imapfilter ()
  "Run imapfilter"
  (interactive)
  (shelldon-async-command "imapfilter"))

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

(defun my/wezterm-here ()
  "Open a new WezTerm window in the current directory."
  (interactive)
  (let ((dir (or default-directory (expand-file-name "~"))))
    (start-process "wezterm" nil "wezterm" "cli" "spawn" "--new-window" "--cwd" dir)))

(map! :leader "a" nil)

(map! :leader
      :desc "embark act" "e" #'embark-act)

(map! :leader
      :prefix ("a" . "actions")
      :desc "ensure ssh auto-closes opener socket" "O" #'my/configure-opener
      :desc "imapfilter" "i" #'my/imapfilter
      :desc "dired" "d" #'dired
      :desc "agent shell" "a" #'agent-shell
      :desc "wezterm here" "t" #'my/wezterm-here
      (:prefix ("u" . "updates")
       :desc "nix rebuild" "n" #'my/smart-rebuild
       :desc "doom sync" "d" #'my/doom-sync
       :desc "add ssh keys" "k" #'my/ssh-add
       :desc "refresh ssh auth socket" "a" #'ssh-update-auth)
      :desc "shelldon output history" "h" #'shelldon-output-history)

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

(defun my/short-system-name ()
  (seq-take-while (lambda (elt) (not (eq elt ?.))) (system-name)))

(setq frame-title-format
      '((:eval (if (getenv "SSH_CONNECTION") (concat "[" (my/short-system-name) "] ")))
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

(with-eval-after-load 'eshell
  (add-hook 'eshell-mode-hook (lambda ()
                                (setq-local corfu-auto nil)
                                (corfu-mode)))

  (defun eshell/emacs (file)
    (find-file file))

  (defun eshell/vi (file)
    (find-file file))

  (defun eshell/vim (file)
    (find-file file))

  (set-eshell-alias!
   "home" "cd ~/Git/dylanwh/dotfiles"
   "pull" "git pull"
   "push" "git push"
   "full-disk-access-p" "plutil -lint /Library/Preferences/com.apple.TimeMachine.plist"
   "eza" "eza -F -h --group-directories-first -b --smart-group -soldest -r $*")

  )



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
          (message "opener: %s" url)
          t)
      
      (error (message "opener not available, url: %s" url)
             nil))))

;; Set it as the default browser for Emacs
(with-eval-after-load 'browse-url
  (setq browse-url-browser-function 'my/browse-url-remote-opener))

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
   (:maildir "/+SaneLater" :key ?l :hide-if-no-unread t)
   (:maildir "/+SaneNews"  :key ?n :hide-if-no-unread t)
   (:maildir "/Archive"   :key ?a :hide-if-no-unread t)
   (:maildir "/Billing"   :key ?b :hide-if-no-unread t)
   (:maildir "/Bugzilla"  :key ?z :hide-if-no-unread t)
   (:maildir "/Junk Mail" :key ?j)
   (:maildir "/Trash"     :key ?t)))

(map! :desc "Open config.el" :ni "s-," (cmd! (find-file (expand-file-name "config.el" doom-user-dir))))

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

(defun my/notify-new-buffer ()
  "Send an OSC 777 notification for new emacsclient buffers."
  (when-let* ((tty (terminal-name (frame-terminal)))
              (buf (buffer-name))
              (_ (not (string-match-p "\\`COMMIT_EDITMSG\\'" buf))))
    (send-string-to-terminal
     (format "\e]777;notify;emacs;%s\e\\" buf)
     (frame-terminal))))

(require 'server)
(unless (server-running-p)
  (server-start))

(add-hook 'server-visit-hook #'my/notify-new-buffer)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(let ((theme (or (alist-get (my/short-system-name)
                            my/host-theme-alist
                            nil nil #'string=) 'doom-dark+)))
  (load-theme theme t))
